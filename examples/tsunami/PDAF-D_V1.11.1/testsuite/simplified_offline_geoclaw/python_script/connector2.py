import os
import sys
import subprocess
import shutil
import numpy as np
import os
import sys
import make_init
import make_init_ens
import pdaf_to_geoclaw
import remove_file
import obs


# First will try running using qinit.

#Create initial ensemble

#----------------------------------------------------#
#Do the forecast step
#----------------------------------------------------#
#Use the data of ens_n.txt to re-initialize the data.
#Note that the output is written to fort.q. The 4th column is the eta
# Write it in hump.xyz format [x y val]

def main():
    """
    Do everything
    """
    #Model Parameters
    nxpoints = 51
    nypoints = 51
    xlower = -50.e0
    xupper = 50.e0
    yupper = 50.e0
    ylower = -50.e0
    geoclaw_input = "hump.xyz"     
    radialbowl_files = ["bowl.topotype2", "Makefile", "setrun.py"]
    radialbowl_path = "/h2/pkjain/Desktop/Pushkar/clawpack/geoclaw/examples/tsunami/bowl-radial/"
    
    #DA parameters
    num_ens = 9
    obs_t_interval = 10
    stddev_obs = 0.5
    dxobs = 5;
    dyobs = 4;

    x = np.linspace(xlower,xupper,nxpoints)
    y = np.linspace(yupper,ylower,nypoints)
    xv,yv = np.meshgrid(x,y)
        
    #Create main initial data file in format of the Geoclaw input
    mean_init_z = make_init.makeinit(xv, yv, geoclaw_input)

    #Create observation data
    obs.make_obs(nxpoints, nypoints, dxobs, dyobs,stddev_obs,mean_init_z)

    #Create ensemble members based on the mean value vector
    make_init_ens.makeinitens(mean_init_z,num_ens, "ens_")
    
    #Convert format of ensemble to data input format of Geoclaw qinit
    firsttime = True


    for i in range(1, num_ens+1):
        
        #Check if path exists and create subfolders to run individual geoclaw
        subdir_name = "ens_"+str(i)
        if os.access(subdir_name,os.F_OK):
            remove_file.remove(subdir_name)
        os.mkdir(subdir_name, 0755)

        #Change to subdirectory
        os.chdir(subdir_name)

        #Define pdaf input and output file names
        pdaf_input = "../ens_" + str(i) + ".txt"
        pdaf_output = "../../ens_0"+str(i)+"_ana.txt"

        #Prepare qinit files for geoclaw
        if firsttime:
            pdaf_to_geoclaw.pdaf_to_geoclaw(xv, yv, pdaf_input, geoclaw_input)
            #firsttime = False
        else:
            pdaf_to_geoclaw.pdaf_to_geoclaw(xv, yv, pdaf_output, geoclaw_input)

        #Copy the radial bowl test case files to every sub directory
        for files in radialbowl_files:
            shutil.copy2(os.path.join(radialbowl_path,files),os.getcwd())

        #Run Geoclaw forecast step
        subprocess.call(["make",".output"])

        #Extract water surface elevation from geoclaw fort.q file
        eta = np.loadtxt("_output/fort.q00" + str(obs_t_interval), skiprows=9, usecols = [3])
        np.savetxt("../ens_"+str(i)+".txt_new", eta)

        
        
        #Go back one directory  
        os.chdir("../")
        
    #Run PDAF assimilation step

    # Run Geoclaw forecast step for all the ensemble members


 

#Do the assimilation step


#Just end everything and move on ahead in life


if __name__=='__main__':
    main()
