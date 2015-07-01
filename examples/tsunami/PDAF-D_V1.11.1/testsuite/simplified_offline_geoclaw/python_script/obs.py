import numpy as np

def make_obs():
    dimx = 51
    dimy = 51
    dxobs = 5
    dyobs = 4
    stddev_obs = 0.5

    truefield = np.zeros((dimx,dimy))
    obs_error = stddev_obs*np.random.randn(dimx,dimy)
    
    full_obs = truefield + obs_error
    obs = np.zeros_like(truefield)- 999
    obs[dxobs:dimx:dxobs,dyobs:dimy:dyobs]=full_obs[dxobs:dimx:dxobs,dyobs:dimy:dyobs]

    print obs
    np.savetxt("obs.txt",obs)

if __name__=='__main__':
    make_obs()
