import plotmap
import ReadAmrForLevel as ramr
import myplots
import numpy as np
import matplotlib.pyplot as plt


def make_obs(mxv, myv, obs_time_list, xobs_start, yobs_start, xobs_end, yobs_end, nxobs, nyobs, ictype):

    obs_x = np.linspace(xobs_start,xobs_end, nxobs).astype('int32')
    obs_y = np.linspace(yobs_start,yobs_end, nyobs).astype('int32')
    print obs_x, obs_y
    obs_xv, obs_yv = np.meshgrid(obs_x, obs_y)
    obs_mat = np.ones((50,50))*-999.0

    for i,j in enumerate(obs_time_list):

        read_geoclaw_output = "./_output_original_" + ictype + "/fort.q" + str(i+1).zfill(4)
        original_case = ramr.ReadAmrForLevel(read_geoclaw_output, 1.0)
        original_water = original_case.water
        original_land = original_case.land
        #mxv = original_case.mxv
        #myv = original_case.myv

        #Construct observations
        obs_file = "obs_step"+str(j)+".txt"
        obs_mat[obs_xv,obs_yv] = original_water[obs_xv,obs_yv]

        print "Observation at chosen location - ",obs_mat[obs_xv,obs_yv] 
        print "Writing observation file - ", obs_file
        np.savetxt(obs_file, obs_mat, fmt = "%12.10f")
        obs_mat_water = np.ma.array(obs_mat,mask = original_case.land==0.0)
        obs_mat_land = original_case.land

        plotmap.docontour(mxv,myv,obs_mat_water, obs_mat_land, "Observation", -999.0, 1.0)
        #plt.show()
    

if __name__=="__main__":

    x = np.linspace(-98,98,50)
    y = np.linspace(-98,98,50)
    mxv,myv = np.meshgrid(x,y)
    xobs_start = 24 
    yobs_start = 24
    xobs_end = 26
    yobs_end = 26
    nxobs = 3
    nyobs = 3
    ictype = "hump"
    #num_time_steps=6

    make_obs(mxv, myv, np.linspace(40,200,5, dtype="int32"), xobs_start, yobs_start, xobs_end, yobs_end, nxobs, nyobs, ictype)
