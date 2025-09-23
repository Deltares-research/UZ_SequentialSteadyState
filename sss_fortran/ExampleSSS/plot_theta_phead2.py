#!/usr/bin/env python
import sys
import numpy as np
import netCDF4 as nc
import matplotlib.pyplot as plt

ds=nc.Dataset(sys.argv[1])
ibox=int(sys.argv[2])

thnod = ds.variables["thnode"][:]
phnod = ds.variables["phnode"][:]
znod = ds.variables["znode"][:]
thbox = ds.variables["theta"][:]
phbox = ds.variables["phead"][:]
n2b = ds.variables["nod2box"][:]
n2bmask=(n2b==ibox)
print(n2bmask)

figure2, ax2 = plt.subplot_mosaic(
    """
    2
    3
    """,
figsize=(8, 8)
)

col = (0.7, 0.7, 0.7)
print (znod[n2bmask])
znod_sel = znod[n2bmask]
phnod_sel=phnod[:,n2bmask] - znod_sel
print (znod_sel)
print (phnod_sel)
for i in range(np.shape(phnod_sel)[1]):
    ax2["2"].plot(phnod_sel[:,i]+znod_sel[i],'-',color=col)
ax2["2"].plot(phbox[:,ibox-1],'k*')
ax2["2"].set_title("phead")

thnod_sel=thnod[:,n2bmask]
for i in range(np.shape(thnod_sel)[1]):
    ax2["3"].plot(thnod_sel[:,i],color=col)
ax2["3"].plot(thbox[:,ibox-1],'k*')
ax2["3"].set_title("theta")

#box = ax2["8"].get_position()
#ax2["8"].legend(loc='upper left', bbox_to_anchor=(1.2, 1))


plt.show()
