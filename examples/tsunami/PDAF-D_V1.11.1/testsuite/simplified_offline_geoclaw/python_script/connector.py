import numpy as np
import os
import sys
import make_init
import make_init_ens
#sys.path.append()


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
    outfile= "hump.xyz"     
    outfile_ens = "ens_"
    x = np.linspace(xlower,xupper,nxpoints)
    y = np.linspace(yupper,ylower,nypoints)
    xv,yv = np.meshgrid(x,y)
    num_ens = 9

        
    #Create main initial data file and its ensemble members
    mean_init_z = make_init.makeinit(xv, yv, outfile)

    make_init_ens.makeinitens(mean_init_z,num_ens, outfile_ens)
 

#Do the assimilation step


#Just end everything and move on ahead in life


if __name__=='__main__':
    main()
