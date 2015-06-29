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
    outfile_ens = "ens_"
    x = np.linspace(xlower,xupper,nxpoints)
    y = np.linspace(yupper,ylower,nypoints)
    xv,yv = np.meshgrid(x,y)
    num_ens = 9
    radialbowl_files = ["bowl.topotype2", "Makefile", "setrun.py"]
    radialbowl_path = "/h2/pkjain/Desktop/Pushkar/clawpack/geoclaw/examples/tsunami/bowl-radial/"

        
    #Create main initial data file in format of the Geoclaw input
    mean_init_z = make_init.makeinit(xv, yv, geoclaw_input)

    #Create ensemble members based on the mean value vector
    make_init_ens.makeinitens(mean_init_z,num_ens, outfile_ens)
    
    #Convert format of ensemble to data input format of Geoclaw qinit
    firsttime = True
    for i in range(1, num_ens+1):
        os.mkdir(str(i), 0755)
        os.chdir(str(i))
        pdaf_input = "../ens_" + str(i) + ".txt"
        pdaf_output = "../../ens_0"+str(i)+"_ana.txt"
        if firsttime:
            pdaf_to_geoclaw.pdaf_to_geoclaw(xv, yv, pdaf_input, geoclaw_input)
            #firsttime = False
        else:
            pdaf_to_geoclaw.pdaf_to_geoclaw(xv, yv, pdaf_output, geoclaw_input)
        for files in radialbowl_files:
            shutil.copy2(os.path.join(radialbowl_path,files),os.getcwd())
        os.chdir("../")
    # Run Geoclaw forecast step for all the ensemble members


 

#Do the assimilation step


#Just end everything and move on ahead in life


if __name__=='__main__':
    main()
