r"""Plot SSS DLL coupled runs vs MegaSWAP Python runs and the Fortran MetaSWAP reference.

usage: python plot_comparison.py   -> results\sss_vs_python.png

needs results\sss_coupled_{n300,split_n300,split_noninterp_n300}.npz from run_sss_coupled.py
(tags n300, split_n300, split_noninterp_n300; see reference\SOURCES.txt for the reference data).
"""
from pathlib import Path

import matplotlib
matplotlib.use("Agg")
import matplotlib.pyplot as plt
import numpy as np

HERE = Path(__file__).resolve().parent
PYRES = HERE / "reference" / "python"
MSWRES = HERE / "reference" / "metaswap"
QRCH = np.array([0.0026] * 30 + [0.0] * 10 + [0.0032] * 30 + [0.0026] * 250)[:300]
td = np.arange(1, 301)

s0 = np.load(HERE / "results" / "sss_coupled_n300.npz")
s1 = np.load(HERE / "results" / "sss_coupled_split_n300.npz")
s = np.load(HERE / "results" / "sss_coupled_split_noninterp_n300.npz")
std = np.load(PYRES / "dt_run_1_n300.npz", allow_pickle=True)
spl = np.load(PYRES / "dt_exp_1_split_n300.npz", allow_pickle=True)

# Fortran MetaSWAP reference: MF6 layer-1 head series and sc1 (svat_per csv)
msw_head = np.loadtxt(MSWRES / "head_layer1.csv", delimiter=",")
msw_td, msw_h = msw_head[:, 0], msw_head[:, 1]
msw_sc1 = np.genfromtxt(MSWRES / "svat_per_0000000001.csv", delimiter=",", names=True)["sc1m3m2m"]

fig, ax = plt.subplots(4, 1, figsize=(10, 12), sharex=True)

ax[0].plot(td, s0["head"], ":", color="gray", label="SSS before fix")
ax[0].plot(td, s1["head"], "-.", label="SSS split fix, newinterp=T")
ax[0].plot(td, s["head"], label="SSS split fix, newinterp=F")
ax[0].plot(td, np.asarray(std["mf6_head"], float), label="Python standard")
ax[0].plot(td, np.asarray(spl["mf6_head"], float), label="Python split (cap 100)")
ax[0].plot(msw_td, msw_h, "k--", label="Fortran MetaSWAP")
ax[0].set_ylabel("MF6 head layer 1 (m)")
ax[0].legend()

ax[1].plot(td, s0["sc1"], ":", color="gray", label="SSS before fix")
ax[1].plot(td, s1["sc1"], "-.", label="SSS split fix, newinterp=T")
ax[1].plot(td, s["sc1"], label="SSS split fix, newinterp=F")
ax[1].plot(td, np.asarray(std["sc1_used"], float), label="Python standard")
ax[1].plot(td, np.asarray(spl["sc1_used"], float), "--", label="Python split")
ax[1].plot(msw_td, msw_sc1[: msw_td.size], "k--", label="Fortran MetaSWAP (svat_per)")
ax[1].set_ylabel("sc1 used (-)")
ax[1].legend()

ax[2].plot(td, QRCH - s0["pre_vsim"], ":", color="gray", label="SSS before fix")
ax[2].plot(td, QRCH - s1["pre_vsim"], "-.", label="SSS split fix, newinterp=T")
ax[2].plot(td, QRCH - s["pre_vsim"], label="SSS split fix, newinterp=F")
ax[2].plot(td, QRCH - np.asarray(std["pre_vsim"], float), label="Python standard")
ax[2].plot(td, QRCH - np.asarray(spl["pre_vsim"], float), "--", label="Python split")
ax[2].set_ylabel("ds = qrch - pre_vsim (m/d)")
ax[2].legend()

ax[3].plot(td, s1["niter"], "-.", label="SSS split fix, newinterp=T")
ax[3].plot(td, s["niter"], label="SSS split fix, newinterp=F")
ax[3].plot(td, np.asarray(std["niter"], float), label="Python standard")
ax[3].plot(td, np.asarray(spl["it_n"], float), "--", label="Python split")
ax[3].set_ylabel("outer iterations")
ax[3].set_xlabel("td")
ax[3].legend()

for a in ax:
    a.grid(alpha=0.3)
fig.tight_layout()
fn = HERE / "results" / "sss_vs_python.png"
fig.savefig(fn, dpi=120)
print("saved", fn)
