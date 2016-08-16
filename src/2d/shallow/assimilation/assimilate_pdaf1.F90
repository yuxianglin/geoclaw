!$Id: assimilate_pdaf.F90 1415 2013-09-25 14:33:26Z lnerger $
!BOP
!
! !ROUTINE: assimilate_pdaf - Routine to control perform analysis step
!
! !INTERFACE:
SUBROUTINE assimilate_pdaf(nvar,naux)

! !DESCRIPTION:
! This routine is called during the model integrations at each time 
! step. It check whether the forecast phase is completed. If so, 
! PDAF_put_state_X is called to perform the analysis step.
!
! !REVISION HISTORY:
! 2013-08 - Lars Nerger - Initial code for NEMO
! Later revisions - see svn log
!
! !USES:
!  use mod_model, only: field
!  use mod_assimilation, only: dim_ens, filename,
!     & filtertype, first_assimilation
!  use PDAF_mod_filter, only: cnt_steps,nsteps,type_filter
  use mod_parallel, only: mype_world,abort_parallel,mpi_comm_world,mpierr
  USE mod_assimilation, &      ! Variables for assimilation
       ONLY: filtertype, stepnow,assimilate_step
   use mod_model,only:field

  IMPLICIT NONE

! !CALLING SEQUENCE:
! Called by: step
! CAlls: PDAF_assimilate_X
!EOP

! Local variables
  INTEGER,INTENT(IN)::nvar
  INTEGER,INTENT(IN)::naux
  INTEGER :: status_pdaf       ! PDAF status flag
  INTEGER :: steps
  INTEGER :: doexit
  REAL :: time


  ! External subroutines
  EXTERNAL :: prodRinvA_pdaf,&
             prodRinvA_l_pdaf,&
             next_observation_pdaf,&
      ! Provide time step, model time and dimension of next observation
             distribute_state_pdaf,&
      ! Routine to distribute a state vector to model fields
             prepoststep_ens_pdaf,&
      ! Routine to collect a state vector from model fields
             collect_state_pdaf,&
      ! Initialize dimension of observation vector
             init_dim_obs_pdaf,&
             init_dim_obs_f_pdaf,&
             init_dim_obs_l_pdaf,&
      ! Implementation of observation operator
             obs_op_pdaf,&
             obs_op_f_pdaf,&
      ! Routine to provide vector of measurements
             init_obs_pdaf,&
             init_obs_f_pdaf,&
             init_obs_l_pdaf,&
      ! Add obs. error covariance R to HPH in EnKF
             add_obs_error_pdaf,&
      ! Initialize obs error covar R in EnKF
             init_obscovar_pdaf,&
      !
             init_n_domains_pdaf,&
             init_dim_l_pdaf,&
             g2l_state_pdaf,&
             l2g_state_pdaf,&
             g2l_obs_pdaf,&
             init_obsvar_pdaf,&
             init_obsvar_l_pdaf
! *********************************
! *** Call assimilation routine ***
! *********************************
   stepnow=stepnow+1
   if (stepnow==assimilate_step) then
!      print *,first_assimilation
 !     first_assimilation=.false.
 !     call MPI_barrier(mpi_comm_world,mpierr)
      call alloc2field(nvar,naux)
!      print *,'here'
!      call MPI_barrier(mpi_comm_world,mpierr)
!      print *,'here'
!     print *,field(2500)
!      print *,"before assimilation"
!      print *,filtertype
!      call MPI_barrier(mpi_comm_world,mpierr)


!       if(mype_world==0) write(*,'(a,5x,a)') 'PDAF','Perform assimilation with PDAF'
        if (filtertype==2) then
             CALL PDAF_put_state_enkf(collect_state_pdaf,&
                 init_dim_obs_pdaf, obs_op_pdaf, init_obs_pdaf,&
                 prepoststep_ens_pdaf, add_obs_error_pdaf,&
                 init_obscovar_pdaf,status_pdaf)
         else if (filtertype==1) then
             CALL PDAF_put_state_seik(collect_state_pdaf,distribute_state_pdaf,&
                 init_dim_obs_pdaf, obs_op_pdaf, init_obs_pdaf,&
                 prepoststep_ens_pdaf, prodRinvA_pdaf,&
                 init_obsvar_pdaf,next_observation_pdaf, status_pdaf)
          else if (filtertype==4) then
             CALL PDAF_put_state_etkf(collect_state_pdaf,&
             init_dim_obs_pdaf, obs_op_pdaf, init_obs_pdaf,&
             prepoststep_ens_pdaf, prodRinvA_pdaf,&
             init_obsvar_pdaf,status_pdaf)
        else if (filtertype==0) then
            CALL PDAF_assimilate_seek(collect_state_pdaf,&
             init_dim_obs_pdaf, obs_op_pdaf, init_obs_pdaf,&
             prepoststep_ens_pdaf, prodRinvA_pdaf, status_pdaf)
        else if (filtertype==6) then
            CALL PDAF_put_state_estkf(collect_state_pdaf,&
             init_dim_obs_pdaf, obs_op_pdaf, init_obs_pdaf,&
             prepoststep_ens_pdaf, prodRinvA_pdaf,&
             init_obsvar_pdaf, status_pdaf)
        else if (filtertype==7) then
            CALL PDAF_put_state_lestkf(collect_state_pdaf,&
             init_dim_obs_f_pdaf,obs_op_f_pdaf,init_obs_f_pdaf,&
             init_obs_l_pdaf,prepoststep_ens_pdaf,&
             prodRinvA_l_pdaf,init_n_domains_pdaf,&
             init_dim_l_pdaf,init_dim_obs_l_pdaf,g2l_state_pdaf,&
             l2g_state_pdaf, g2l_obs_pdaf, init_obsvar_pdaf,&
             init_obsvar_l_pdaf,status_pdaf)
        else if (filtertype==3) then
            CALL PDAF_put_state_lseik(collect_state_pdaf,&
             init_dim_obs_f_pdaf,obs_op_f_pdaf,init_obs_f_pdaf,&
             init_obs_l_pdaf,prepoststep_ens_pdaf,&
             prodRinvA_l_pdaf,init_n_domains_pdaf,&
             init_dim_l_pdaf,init_dim_obs_l_pdaf,g2l_state_pdaf,&
             l2g_state_pdaf, g2l_obs_pdaf, init_obsvar_pdaf,&
             init_obsvar_l_pdaf,status_pdaf)
        else if (filtertype==5) then
            CALL PDAF_put_state_letkf(collect_state_pdaf,&
             init_dim_obs_f_pdaf,obs_op_f_pdaf,init_obs_f_pdaf,&
             init_obs_l_pdaf,prepoststep_ens_pdaf,&
             prodRinvA_l_pdaf,init_n_domains_pdaf,&
             init_dim_l_pdaf,init_dim_obs_l_pdaf,g2l_state_pdaf,&
             l2g_state_pdaf, g2l_obs_pdaf, init_obsvar_pdaf,&
             init_obsvar_l_pdaf,status_pdaf)
         else
             if (mype_world==0) print *,"invalid filter type"
             CALL  abort_parallel()
         endif
!        if (filtertype==2) then
!             CALL PDAF_assimilate_enkf(collect_state_pdaf,distribute_state_pdaf,&
!                 init_dim_obs_pdaf, obs_op_pdaf, init_obs_pdaf,&
!                 prepoststep_ens_pdaf, add_obs_error_pdaf,&
!                 init_obscovar_pdaf, next_observation_pdaf,status_pdaf)
!         else if (filtertype==1) then
!             CALL PDAF_assimilate_seik(collect_state_pdaf,distribute_state_pdaf,&
!                 init_dim_obs_pdaf, obs_op_pdaf, init_obs_pdaf,&
!                 prepoststep_ens_pdaf, prodRinvA_pdaf,&
!                 init_obsvar_pdaf,next_observation_pdaf, status_pdaf)
!          else if (filtertype==4) then
!             CALL PDAF_assimilate_etkf(collect_state_pdaf,distribute_state_pdaf,&
!             init_dim_obs_pdaf, obs_op_pdaf, init_obs_pdaf,&
!             prepoststep_ens_pdaf, prodRinvA_pdaf,&
!             init_obsvar_pdaf,next_observation_pdaf,status_pdaf)
!        else if (filtertype==0) then
!            CALL PDAF_assimilate_seek(collect_state_pdaf,&
!             init_dim_obs_pdaf, obs_op_pdaf, init_obs_pdaf,&
!             prepoststep_ens_pdaf, prodRinvA_pdaf, status_pdaf)
!        else if (filtertype==6) then
!            CALL PDAF_assimilate_estkf(collect_state_pdaf,distribute_state_pdaf,&
!             init_dim_obs_pdaf, obs_op_pdaf, init_obs_pdaf,&
!             prepoststep_ens_pdaf, prodRinvA_pdaf,&
!             init_obsvar_pdaf, next_observation_pdaf,status_pdaf)
!        else if (filtertype==7) then
!            CALL PDAF_assimilate_lestkf(collect_state_pdaf,distribute_state_pdaf,&
!             init_dim_obs_f_pdaf,obs_op_f_pdaf,init_obs_f_pdaf,&
!             init_obs_l_pdaf,prepoststep_ens_pdaf,&
!             prodRinvA_l_pdaf,init_n_domains_pdaf,&
!             init_dim_l_pdaf,init_dim_obs_l_pdaf,g2l_state_pdaf,&
!             l2g_state_pdaf, g2l_obs_pdaf, init_obsvar_pdaf,&
!             init_obsvar_l_pdaf,next_observation_pdaf,status_pdaf)
!        else if (filtertype==3) then
!            CALL PDAF_assimilate_lseik(collect_state_pdaf,distribute_state_pdaf,&
!             init_dim_obs_f_pdaf,obs_op_f_pdaf,init_obs_f_pdaf,&
!             init_obs_l_pdaf,prepoststep_ens_pdaf,&
!             prodRinvA_l_pdaf,init_n_domains_pdaf,&
!             init_dim_l_pdaf,init_dim_obs_l_pdaf,g2l_state_pdaf,&
!             l2g_state_pdaf, g2l_obs_pdaf, init_obsvar_pdaf,&
!             init_obsvar_l_pdaf,next_observation_pdaf,status_pdaf)
!        else if (filtertype==5) then
!            CALL PDAF_assimilate_letkf(collect_state_pdaf,distribute_state_pdaf,&
!             init_dim_obs_f_pdaf,obs_op_f_pdaf,init_obs_f_pdaf,&
!             init_obs_l_pdaf,prepoststep_ens_pdaf,&
!             prodRinvA_l_pdaf,init_n_domains_pdaf,&
!             init_dim_l_pdaf,init_dim_obs_l_pdaf,g2l_state_pdaf,&
!             l2g_state_pdaf, g2l_obs_pdaf, init_obsvar_pdaf,&
!             init_obsvar_l_pdaf,next_observation_pdaf,status_pdaf)
!         else
!             if (mype_world==0) print *,"invalid filter type"
!             CALL  abort_parallel()
!         endif

         IF (out_flag==0) then
             call PDAF_get_state(steps,time,doexit,U_next_observation,U_distribute_state,U_prepoststep,outflag)
         Endif
!         nsteps=steps
!     else
!         outflag=0
!     endif
!  print *,"after assimilation"
  call field2alloc(nvar,naux)
!**********update*******************

call update(2,nvar,naux)!hard code to 2, need to set to level-1
call update(1,nvar,naux)







  ! Check for errors during execution of PDAF

  IF (status_pdaf /= 0) THEN
     WRITE (*,'(/1x,a6,i3,a43,i4,a1/)') &
          'ERROR ', status_pdaf, &
          ' in PDAF_put_state - stopping! (PE ', mype_world,')'
     CALL  abort_parallel()
  END IF

END SUBROUTINE assimilate_pdaf
