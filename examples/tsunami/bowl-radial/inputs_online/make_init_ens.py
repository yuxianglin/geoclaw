import numpy as np
import make_obs

def makeinitens(xv, yv, num_ens, ictype="hump"):
    """
    Write output for ensemble members. This format is input for PDAF
    """
    
    #for i in range(1,num_ens + 1):
    #    filename = "ens_" + str(i) + ".txt"
    #    np.savetxt(filename, z1*0.1*(i+0.1), fmt='%-7.5f')

    if ictype == "planewave":
        param = 0.001 
        print "Plane wave initial condition selected"
        for i in range(1,num_ens/2+1):
            z = makeinit_planewave(xv, yv, i*param)
            np.savetxt("ens_"+str(i)+".txt", z, fmt='%12.10f')

        z = makeinit_planewave(xv, yv, 0.0)
        np.savetxt("ens_" + str(num_ens/2 + 1)+".txt", z, fmt='%12.10f')

        for i in range((num_ens/2)+2, num_ens+1):
            z = makeinit_planewave(xv, yv, -i*param)
            np.savetxt("ens_"+str(i)+".txt", z, fmt='%12.10f')
    
    if ictype == "hump":
        param = 1.0
        print "Gaussian hump initial condition selected"
        for i in range(1,num_ens/2+1):
            z = makeinit_hump(xv, yv, i*param)
            np.savetxt("ens_"+str(i)+".txt", z, fmt='%12.10f')

        z = makeinit_hump(xv, yv, 0.0)
        np.savetxt("ens_" + str(num_ens/2 + 1)+".txt", z, fmt='%12.10f')

        for i in range((num_ens/2)+2, num_ens+1):
            z = makeinit_hump(xv, yv, i*param)
            np.savetxt("ens_"+str(i)+".txt", z, fmt='%12.10f')

def makeinitens2(xv, yv, num_ens, lower=1.5, upper=3.0):
    """
    Write output for ensemble members. This format is input for PDAF
    """
    
    print "Gaussian hump initial condition selected"
    #for i in range(1,num_ens + 1):
    #    filename = "ens_" + str(i) + ".txt"
    #    np.savetxt(filename, z1*0.1*(i+0.1), fmt='%-7.5f')
    if num_ens == 1:
        z = makeinit_hump(xv, yv, 0)
        np.savetxt("ens_1.txt", z, fmt='%12.10f')
    else:
        pert = lambda x: (upper-lower)*np.exp(0.8)*(x-1)/(num_ens -1) + (-5.0+lower*np.exp(0.8))
        for i in range(1,num_ens +1):
            z = makeinit_hump(xv, yv, pert(i))
            np.savetxt("ens_"+str(i)+".txt", z, fmt='%12.10f')

        #z = makeinit_hump(xv, yv, 0.0)
        #np.savetxt("ens_" + str(num_ens/2 + 1)+".txt", z, fmt='%12.10f')
#
        #for i in range((num_ens/2)+2, num_ens+1):
        #    z = makeinit_hump(xv, yv, i*param)
        #    np.savetxt("ens_"+str(i)+".txt", z, fmt='%12.10f')
    


def makeinit_hump(xv, yv, perturb):
    """
    Create main initial data file and write in geoclaw initial data format

    ARGUMENTS:
        xv, yv: Input arrays of size nx*ny
        outfile: Output file to write into

    RETURNS:
        z: WSE using f(xv,yv) for Gaussian bump. Size nx*ny 
    """
    
    # Gaussian bump
    ze = -((xv)**2 + (yv)**2)/10.
    z = np.where(ze>-10., (5.e0 + perturb)*np.exp(ze), 0.)
    print "Max amplitude = ", np.max(z)

    #return np.transpose(z)
    return z


def makeinit_planewave(xv,yv,perturb):
    k = 5*np.pi/100.0
    z = 0.8*np.sin(k * (xv+10.0 + perturb))
    #return np.transpose(z)
    return z


    

if __name__ == "__main__":
    nx = 50
    ny = 50
    xlower = -98.0
    xupper = 98.0
    ylower = -98.0
    yupper = 98.0
    num_ens = 1
    ictype = "hump"
    #ictype = "planewave"

    X = np.linspace(xlower, xupper, nx)
    Y = np.linspace(ylower, yupper, ny)
    xv,yv = np.meshgrid(X,Y)
   
    #Make initial ensemble
    if ictype == "hump":
        makeinitens(xv, yv, num_ens, ictype)
    else:
        makeinitens2(xv, yv, num_ens)
   
    #Make observations
    xobs_start = 15 
    yobs_start = 15
    xobs_end = 30
    yobs_end = 30
    nxobs = 5
    nyobs = 5
    make_obs.make_obs(xv, yv, xobs_start, yobs_start, xobs_end, yobs_end, nxobs, nyobs, ictype)
