import matplotlib.pyplot as plt
import matplotlib.cm as cm
import numpy as np
from pylab import *
import numpy.ma as ma
import plotmap
import ReadAmrForLevel as ramr

def gen_figures(obs_time, plot_type, vmin, vmax, colorbar=False):
    read_geoclaw_output = "../_output_original_hump/fort.q0001"
    original_case = ramr.ReadAmrForLevel(read_geoclaw_output, 1.0)
    original_water = original_case.water
    original_land = original_case.land
    for j in range(1, numens+1):

        if plot_type=="init":
            outfile = "ens_"+str(j)+"_ini.png"
            state = np.loadtxt("ens_" + str(j) + ".txt")
            title = "Ensemble #" + str(j)

        if plot_type=="for":
            outfile = "ens_" + str(j) + "_step" + str(obs_time)+ "_for.png"
            state = np.loadtxt("../_output/ens_0" + str(j) +"_step" + str(obs_time) + "_for.txt")
            title = "Ensemble #" + str(j) + "Timestep " + str(obs_time)

        if plot_type=="indiv_ana":
            outfile = "ens_" + str(j) + "_step" + str(obs_time)+ "_ana.png"
            state = np.loadtxt("../_output/ens_0" + str(j) +"_step" + str(obs_time) + "_ana.txt")
            title = "Ensemble #" + str(j) + "Timestep " + str(obs_time)

        if plot_type=="assim_ana":
            outfile = "state_step" + str(obs_time)+ "_ana.png"
            state = np.loadtxt("../_output/state_step" + str(obs_time) + "0_ana.txt")
            title = "Timestep " + str(obs_time)

        masked_val = np.loadtxt("masked_topo.txt")
        water = np.ma.array(state, mask = original_case.land==0.0)
        land = original_case.land
        #water = np.ma.array(state, mask = masked_val==1)
        #land = np.ma.array(state, mask = masked_val==0)

        plotmap.docontour(mxv,myv,water, land, title, vmin, vmax, colorbar = colorbar)
        plt.savefig(outfile,bbox_inches='tight')

if __name__=="__main__":
    numens = 5
    numsteps = 9
    x = np.linspace(-98.,98.,50)
    y = np.linspace(-98.,98.,50)
    mxv,myv = np.meshgrid(x,y)
    obs_time_list = [20,40,60,80,100,120,140,160,180]


    #Generate figures of initial conditions: ens_X_ini.png
    gen_figures(0, "init", -50., 50.)
    
    #Forecasted states: "ens_XX_stepYY_for.png"
    for i in obs_time_list:
        gen_figures(i, "for", -2.6, 2.3)

    #Individual analysis states: "ens_XX_stepYY_ana.png"
    for i in obs_time_list:
        gen_figures(i, "indiv_ana", -2.6, 2.3)
    
    #Assimilated analysis state
    for i in obs_time_list:
        gen_figures(i, "assim_ana", -2.6, 2.3)

    plt.show()
