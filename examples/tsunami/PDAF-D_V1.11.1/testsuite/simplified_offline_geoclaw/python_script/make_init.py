import numpy as np

def makeinit(xv, yv, outfile):
    """
    Create main initial data file and its ensemble members
    """
    
    # Gaussian bump
    ze = -((xv)**2 + (yv)**2)/10.
    z1 = np.where(ze>-10., 40.e0*np.exp(ze), 0.)
    #z1 = np.full_like(xv, 0.0)

    #z2 = np.full_like(xv,-2.0)


    # Write output for initial data. This format is qinit format for GeoCLAW
        
    dstacked = np.dstack((xv,yv,z1))
    with file(outfile,'w') as outfile:
        for slice_2d in dstacked:
            np.savetxt(outfile,slice_2d, fmt='%-7.5f')

    return z1
