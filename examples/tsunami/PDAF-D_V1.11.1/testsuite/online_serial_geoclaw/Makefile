
main_push:
	mpif90 -c nullmpi.F90
	mpif90 -c mod_parallel_pdaf.F90
	mpif90 -c parser_mpi.F90
	mpif90 -c init_parallel_pdaf.F90
	mpif90 -c mod_assimilation.F90
	mpif90 -c mod_model.F90
	mpif90 -c init_pdaf.F90
	mpif90 -c init_pdaf_parse.F90 
	mpif90 -c init_pdaf_info.F90
	mpif90 -c init_ens.F90
	mpif90 -c distribute_state_pdaf.F90
	mpif90 -c next_observation_pdaf.F90
	mpif90 -c prepoststep_ens_pdaf.F90
	mpif90 main_push.F90 *.o -L ../../lib/ -lpdaf-d

clean:
	rm *.o *.mod


