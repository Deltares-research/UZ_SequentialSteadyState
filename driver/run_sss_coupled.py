"""Coupled MF6 + SequentialSteadyState (SSS) Fortran DLL driver.

Mirrors CoupledExperimentalSimulation.update() of MegaSWAP (src/mf6_simulation.py):
  - SSS prepare_time_step -> vsim, sc1 at the start-of-step head
  - per outer iteration: (iter < max_update_iter) SSS re-evaluated at the current MF6 head
    (SSS solve = do_unsa), push vsim/sc1 into MF6, then MF6 solve
  - after convergence: SSS finalize_time_step with the final MF6 head

Units: the DLL vsim is m3 per step; MF6 RCH is m/d -> rch = vsim / area / dt.
qmodf is computed here (the DLL qmodf/qrun/pond outputs are shifted by an argument
order problem in sss_comp.f90 and are not used).

usage: python run_sss_coupled.py [ntime] [max_update_iter] [tag]

All inputs are inside this checkout (driver\data). Requirements outside the repo: the DLL built from the
solution (SeqSteadyState.dll, found in driver\build or set SSS_DLL), a Python env with numpy + xmipy, and
libmf6.dll on PATH (or set LIBMF6_DLL).
"""
import os
import shutil
import sys
import time
from pathlib import Path

import numpy as np
from xmipy import XmiWrapper

HERE = Path(__file__).resolve().parent
REPO = HERE.parent
DLL = Path(os.environ.get("SSS_DLL", HERE / "build" / "SeqSteadyState.dll"))
NC_BIN = REPO / "netcdf" / "netCDF 4.6.1" / "bin"
MF6_SRC = HERE / "data" / "mf6_model"
UNSA_SRC = HERE / "data" / "unsa_db" / "unsa_001_100.nc"
NODES_SRC = HERE / "data" / "unsa_db" / "nodes_001.nc"
LIBMF6 = os.environ.get("LIBMF6_DLL", "libmf6.dll")
MF6_NAME = "model"

# test parameters (same as MegaSWAP run_coupled.py)
QRCH = np.array([0.0026] * 30 + [0.0] * 10 + [0.0032] * 30 + [0.0026] * 250)  # m/d
AREA = 100.0
DTGW = 1.0
INIT_GWL = -3.0  # m
INIT_PHEAD = -1.5  # m
ROOTZONE = 1.0  # m
ZMAX_PONDING = 0.02  # m
MAX_INFILTRATION = 0.004  # m/d
SOIL_RESISTANCE = 1.0  # d
SURFACE_ELEVATION = 0.0
SOF_COND = AREA / SOIL_RESISTANCE  # same as Python get_sof_parameters (cond 100)
SOF_ELEV = ZMAX_PONDING
MIN_ITER = 0


def setup_run(ntime: int) -> tuple[Path, Path]:
    run = HERE / "run"
    sss_dir = run / "sss"
    mf6_dir = run / "mf6"
    for d in (sss_dir, mf6_dir):
        if d.exists():
            shutil.rmtree(d)
    (sss_dir / "unsa_db").mkdir(parents=True)
    mf6_dir.mkdir(parents=True)

    for f in MF6_SRC.iterdir():
        if f.is_file():
            shutil.copy2(f, mf6_dir / f.name)

    shutil.copy2(UNSA_SRC, sss_dir / "unsa_db" / UNSA_SRC.name)
    shutil.copy2(NODES_SRC, sss_dir / "unsa_db" / NODES_SRC.name)
    (sss_dir / "unsa_db" / "rootzones.txt").write_text(f"1 {ROOTZONE}\n")

    # node,svat,x,y,soil_id,rotation_id,meteo_id,ct_id,level,hmf6,area,top,init_gwli(cm),dprz,
    # zmax_ponding,maxinf,soil_resist,init_phead,sw_irrig,rds,pondmx,rsro,rsroexp
    cols = [1, 1, 0.0, 0.0, 1, 1, 1, 1, 0.0, 0.0, AREA, SURFACE_ELEVATION,
            INIT_GWL * 100.0, ROOTZONE, ZMAX_PONDING, MAX_INFILTRATION, SOIL_RESISTANCE,
            INIT_PHEAD, 0.0, 0.0, 0.0, 0.0, 0.0]
    (sss_dir / "swap_ini.csv").write_text(",".join(str(c) for c in cols) + "\n")

    # meteo: time svat rr(cm/d) ev(cm/d); rows day 1 .. ntime+2
    rr = np.concatenate([QRCH, np.full(max(0, ntime + 2 - QRCH.size), QRCH[-1])])
    with open(sss_dir / "mete_svat.inp", "w") as f:
        for day in range(1, ntime + 3):
            f.write(f"{day} 1 {rr[day - 1] * 100.0:.6f} 0.0\n")
    return sss_dir, mf6_dir


def main() -> None:
    ntime = int(sys.argv[1]) if len(sys.argv) > 1 else 250
    max_update_iter = int(sys.argv[2]) if len(sys.argv) > 2 else 100
    tag = sys.argv[3] if len(sys.argv) > 3 else f"n{ntime}"

    sss_dir, mf6_dir = setup_run(ntime)

    mf6 = XmiWrapper(lib_path=LIBMF6, working_directory=str(mf6_dir))
    mf6.initialize()
    max_iter = int(mf6.get_value_ptr("SLN_1/MXITER")[0])
    name = MF6_NAME.upper()
    head = mf6.get_value_ptr(f"{name}/X")
    sto = mf6.get_value_ptr(f"{name}/STO/SS")
    rch = mf6.get_value_ptr(f"{name}/RCH-1/RECHARGE")
    sof_elev = mf6.get_value_ptr(f"{name}/SOF/ELEV")
    sof_cond = mf6.get_value_ptr(f"{name}/SOF/COND ")
    sof_cond[0] = SOF_COND
    sof_elev[0] = SOF_ELEV

    sss = XmiWrapper(lib_path=str(DLL), lib_dependency=NC_BIN, working_directory=str(sss_dir))
    sss.initialize()
    lvgw = sss.get_value_ptr("lvgwmodf")
    vsim_a = sss.get_value_ptr("vsim")
    sc1_a = sss.get_value_ptr("sc1sim")
    tiop = sss.get_value_ptr("tiop")
    phead = sss.get_value_ptr("phead")
    nbox = sss.get_value_ptr("nbox")
    print("SSS gwl at start (m):", lvgw[0], "MF6 head:", head[:], "max_iter:", max_iter, flush=True)

    out = {
        "head": np.full(ntime, np.nan), "head_old": np.full(ntime, np.nan),
        "vsim": np.full(ntime, np.nan), "sc1": np.full(ntime, np.nan),
        "qmodf": np.full(ntime, np.nan), "niter": np.zeros(ntime, dtype=int),
        "converged": np.zeros(ntime, dtype=bool),
        "pre_vsim": np.full(ntime, np.nan), "pre_sc1": np.full(ntime, np.nan),
        "it_h_in": np.full((ntime, max_iter), np.nan),
        "it_h_out": np.full((ntime, max_iter), np.nan),
        "it_vsim": np.full((ntime, max_iter), np.nan),
        "it_sc1": np.full((ntime, max_iter), np.nan),
        "phead": np.full((ntime, phead.shape[1]), np.nan),
        "nbox": np.zeros(ntime, dtype=int),
    }

    t0 = time.time()
    for td in range(1, ntime + 1):
        i = td - 1
        mf6.prepare_time_step(0.0)
        h_old = float(head[0])
        out["head_old"][i] = h_old

        tiop[0] = float(td)  # integer day -> piecewise constant forcing (wt = 0)
        lvgw[0] = h_old
        sss.prepare_time_step(DTGW)  # includes do_unsa at h_old
        vsim = vsim_a[0] / AREA / DTGW  # m/d
        sc1 = float(sc1_a[0])
        out["pre_vsim"][i] = vsim
        out["pre_sc1"][i] = sc1
        rch[:] = vsim
        sto[0] = sc1

        mf6.prepare_solve(1)
        converged = False
        it = 0
        for it in range(1, max_iter + 1):
            out["it_h_in"][i, it - 1] = head[0]
            if it < max_update_iter:
                lvgw[0] = float(head[0])
                status = sss.lib.solve()  # do_unsa at current head
                if status != 0:
                    raise RuntimeError(f"SSS solve failed, status {status}")
                vsim = vsim_a[0] / AREA / DTGW
                sc1 = float(sc1_a[0])
                rch[:] = vsim
                sto[0] = sc1
            out["it_vsim"][i, it - 1] = vsim
            out["it_sc1"][i, it - 1] = sc1
            converged = bool(mf6.solve(1))
            out["it_h_out"][i, it - 1] = head[0]
            if converged and it >= MIN_ITER:
                break
        h_new = float(head[0])
        qmodf = (h_new - h_old) * sc1 / DTGW - vsim  # m/d, same as Python experimental
        mf6.finalize_solve(1)
        mf6.finalize_time_step()

        lvgw[0] = h_new
        sss.finalize_time_step()

        out["head"][i] = h_new
        out["vsim"][i] = vsim
        out["sc1"][i] = sc1
        out["qmodf"][i] = qmodf
        out["niter"][i] = it
        out["converged"][i] = converged
        out["phead"][i, :] = phead[0, :]
        out["nbox"][i] = int(nbox[0])
        if td % 25 == 0 or td in (1, 2, 3):
            print(f"td {td:4d} head {h_new: .5f} vsim {vsim: .6f} sc1 {sc1: .5f} "
                  f"iters {it} conv {converged}", flush=True)

    print(f"finished {ntime} steps in {time.time() - t0:.1f} s", flush=True)
    res = HERE / "results"
    res.mkdir(exist_ok=True)
    fn = res / f"sss_coupled_{tag}.npz"
    np.savez(fn, **out)
    print("saved", fn, flush=True)
    sss.finalize()
    mf6.finalize()


if __name__ == "__main__":
    main()
