import numpy as np
from inputs_online import make_obs
from inputs_online import error_analysis
from inputs_online import create_mask
from inputs_online import run_geoclaw
from inputs_online import make_init_ens
from inputs_online import error_analysis

if __name__ == "__main__":
    
    #------------------------------------------------------#
    #Follow these instructions to run GeoCLAW manually
    #1. Use -cpp as only #FFLAGS in Makefile
    #2. Run "make .output"
    #3. Move the output to _output_original_hump. This output will serve as observation values for assimilation
    #------------------------------------------------------#
    
    nx = 50
    ny = 50
    xlower = -98.0
    xupper = 98.0
    ylower = -98.0
    yupper = 98.0
    num_ens = 2
    ictype = "hump"
    total_steps = 200
    output_step_interval = 20
    delt_obs = output_step_interval
    rms_obs = 0.0000001
    
    obs_time_list = np.linspace(output_step_interval, total_steps,total_steps/output_step_interval, dtype='int32')
    num_time_steps = total_steps/output_step_interval + 1
    X = np.linspace(xlower, xupper, nx)
    Y = np.linspace(ylower, yupper, ny)
    xv,yv = np.meshgrid(X,Y)
   
    #Make initial ensemble
    #make_init_ens.makeinitens2(xv, yv, num_ens)
    make_init_ens.makeinitens(xv, yv, num_ens,ictype)
   
    #Make observations
    xobs_start = 25 
    yobs_start = 25
    xobs_end = 35
    yobs_end = 35
    nxobs = 5
    nyobs = 5
    make_obs.make_obs(xv, yv, obs_time_list, xobs_start, yobs_start, xobs_end, yobs_end, nxobs, nyobs, ictype)

    #------------------------------------------------------#
    #Follow these instructions to run assimilation version of GeoCLAW
    #1. Use the alternate #FFLAGS in Makefile
    #2. Make sure setrun.py has num_ens set to same value as above num_ens
    #3. Recompile GeoCLAW after "make clobber"
    #4. Run "make .output"
    #5. Proceed to error analysis
    #------------------------------------------------------#
    run_geoclaw.run_geoclaw("../xgeoclaw",\
            num_ens=num_ens, \
            rms_obs=rms_obs,\
            delt_obs=delt_obs,\
            output_step_interval=output_step_interval,\
            total_steps=total_steps)

    #------------------------------------------------------#
    #Error analysis
    error_analysis.error_calc(ictype, num_time_steps, ploterror=True)
    #------------------------------------------------------#

    #------------------------------------------------------#
    #Vizualization
    #------------------------------------------------------#
    #Create mask of land and water
    #zmin = 80.
    #create_mask(xv,yv,zmin)

