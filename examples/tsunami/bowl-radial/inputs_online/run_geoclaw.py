import ensemble_class
import subprocess
import os
import pdb

def run_geoclaw(geoclaw_exec, filtertype = 2, num_ens = 9, rms_obs = 0.01, delt_obs = 10, local_range=2,output_dir='_output',output_step_interval=10, total_steps=200):
	exist=os.path.exists(output_dir)
	if exist:
		print "Directory already exist"
		exit()
	subprocess.call(["make","clean"])
	subprocess.call(["make","clobber"])
	subprocess.call(["make"])
	
	os.mkdir(output_dir)
	os.chdir(output_dir)
	hello = ensemble_class.ensemble()
	#hello.rundata.clawdata.t0 = dtobs[k]
    #hello.rundata.clawdata.tfinal = dtobs[k+1]
    #hello.rundata.clawdata.num_cells[0] = mx
    #hello.rundata.clawdata.num_cells[1] = my
    #hello.rundata.qinit_data.qinitfiles[-1][-1] = geoclaw_input
    # hello.rundata.topo_data.topofiles[-1]=[2, 1, 1, 0., 1.e10, topo_path]
    #hello.rundata.clawdata.num_output_times = output_times
	#hello.rundata.amrdata.amr_levels_max = max_amr
	#pdb.set_trace()
	hello.rundata.pdaf_data.filtertype = filtertype
	hello.rundata.pdaf_data.num_ensembles = num_ens
	hello.rundata.pdaf_data.rms_obs = rms_obs
	hello.rundata.pdaf_data.delt_obs = delt_obs
	hello.rundata.pdaf_data.local_range=local_range
	#pdb.set_trace()
	hello.rundata.qinit_data.qinitfiles[-1][-1]="../hump.xyz"
	hello.rundata.topo_data.topofiles[-1][-1] = "../bowl.topotype2"
	hello.rundata.clawdata.output_step_interval = output_step_interval
	hello.rundata.clawdata.total_steps = total_steps
	
	hello.rundata.write()
	FNULL = open('Astdout','w')
	subprocess.call(geoclaw_exec)
	#subprocess.call(geoclaw_exec, stdout=FNULL, stderr=subprocess.STDOUT)

if __name__=="__main__":
    run_geoclaw("../xgeoclaw")
