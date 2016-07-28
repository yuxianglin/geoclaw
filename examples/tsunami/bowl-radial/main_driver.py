import numpy as np
from inputs_online import make_obs
from inputs_online import error_analysis
from inputs_online import create_mask
from inputs_online import run_geoclaw
from inputs_online import make_init_ens
from inputs_online import error_analysis
from inputs_online import chunk
if __name__ == "__main__":
    #------------------------------------------------------#
    #Follow these instructions to run GeoCLAW manually
    #1. Use -cpp as only #FFLAGS in Makefile
    #2. Run "make .output"
    #3. Move the output to _output_original_hump. This output will serve as observation values for assimilation
    #------------------------------------------------------#
    nx = 100
    ny = 100
    xlower = -99.0
    xupper = 99.0
    ylower = -99.0
    yupper = 99.0
    #------------------------------------------#
    # ensemble setup
    num_ens =100
    h1=6
    h2=2.0
    ens_type='randn'#linear
    ictype = "hump"
    update_ensemble=0
    total_steps = 400
    output_step_interval = 20
    delt_obs = output_step_interval
    #-------------------------------------_#
    #pdaf setup
    rms_obs =0.0000001
    filtertype = 7
    local_range=0
    obs_time_list = np.linspace(output_step_interval, total_steps,total_steps/output_step_interval, dtype='int32')
    num_time_steps = total_steps/output_step_interval + 1
    X = np.linspace(xlower, xupper, nx)
    Y = np.linspace(ylower, yupper, ny)
    xv,yv = np.meshgrid(X,Y)
    if update_ensemble: 
        #Make initial ensemble
        make_init_ens.makeinitens2(xv, yv,num_ens,h1,h2,ictype,ens_type)
        #make_init_ens.makeinitens(xv, yv, num_ens,ictype=ictype)
        if ((nx > 50) & (ny > 50)):
            chunk.chunk_write(range(1,num_ens+1),type1="init")
  #Make observations
   #if num_ens != 1:
    xobs_start = 20 
    yobs_start = 20
    xobs_end = 80
    yobs_end = 80
    nxobs =60
    nyobs =60
    update_observation=1
    if update_observation:
        make_obs.make_obs(xv, yv, obs_time_list, xobs_start, yobs_start, xobs_end, yobs_end, nxobs, nyobs, ictype)
        #make_obs.make_obs_testing(xv, yv, obs_time_list, xobs_start, yobs_start, xobs_end, yobs_end, nxobs, nyobs, ictype)
        if ((nx > 50) & (ny > 50)):
            chunk.chunk_write(obs_time_list,type1="obs")






    dic={0:'SEEK',1:'SEIK',2:"EnKF",3:"LSEIK",4:"ETKF",5:"LETKF",6:"ESTKF",7:"LESTKF"}
    if filtertype==3 or filtertype==5 or filtertype==7:
        out_dir='_output_'+dic[filtertype]+'_r='+str(local_range)+'_ens='+str(num_ens)+'_obs='+str(nxobs)+'by'+str(nyobs)+'_rms='+str(rms_obs)
    else:
        out_dir='_output_'+dic[filtertype]+'_ens='+str(num_ens)+'_m'+str(h1)+'s2'+'_obs='+str(nxobs)+'by'+str(nyobs)+'_rms='+str(rms_obs)
#    out_dir='_output_no_update_'+'_ens='+str(num_ens)
#------------------------------------------------------#
  #Follow these instructions to run assimilation version of GeoCLAW
    #1. Use the alternate #FFLAGS in Makefile
    #2. Make sure setrun.py has num_ens set to same value as above num_ens
    #3. Recompile GeoCLAW after "make clobber"
    #4. Run "make .output"
    #5. Proceed to error analysis
    #------------------------------------------------------#
#    run_geoclaw.run_geoclaw("../xgeoclaw",\
#            filtertype, \
#            num_ens, \
#            rms_obs,\
#            delt_obs,\
#            local_range,\
#            out_dir,\
#            output_step_interval,\
#            total_steps)
    #------------------------------------------------------#
   # #Error analysis
#    error_analysis.error_calc(ictype, num_time_steps, ploterror=True)
    #------------------------------------------------------#
    #------------------------------------------------------#
    #Vizualization
    #------------------------------------------------------#
    #Create mask of land and water
    #zmin = 80.
    #create_mask(xv,yv,zmin)
