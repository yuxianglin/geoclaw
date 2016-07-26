!$Id: init_dim_obs_pdaf.F90 1443 2013-10-04 10:52:09Z lnerger $
!BOP
!
! !ROUTINE: init_dim_obs_f_pdaf --- Compute number of observations
!
! !INTERFACE:
SUBROUTINE init_dim_obs_f_pdaf(step, dim_obs_f)

! !DESCRIPTION:
! User-supplied routine for PDAF.
! Used in the filters: LSEIK/LETKF/LESTKF
!
! The routine is called in PDAF\_lseik\_update
! at the beginning of the analysis step before
! the loop through all local analysis domains.
! It has to determine the dimension of the
! observation vector according to the current
! time step for all observations required for
! the analyses in the loop over all local
! analysis domains on the PE-local state domain.
!
! Implementation for the 2D online example
! without parallelization.
!
! !REVISION HISTORY:
! 2013-02 - Lars Nerger - Initial code
! Later revisions - see svn log
!
! !USES:
  USE mod_assimilation, &
       !ONLY : obs, obs_index, coords_obs
       ONLY : obs, obs_index, coords_obs_1d, coords_obs_2d, coords_obs
  USE mod_model, &
       ONLY: nx, ny

  IMPLICIT NONE

! !ARGUMENTS:
  INTEGER, INTENT(in)  :: step      ! Current time step
  INTEGER, INTENT(out) :: dim_obs_f ! Dimension of full observation vector

! !CALLING SEQUENCE:
! Called by: PDAF_lseik_update   (as U_init_dim_obs)
! Called by: PDAF_lestkf_update  (as U_init_dim_obs)
! Called by: PDAF_letkf_update   (as U_init_dim_obs)
!EOP

! *** Local variables
  INTEGER :: i, j                     ! Counters
  INTEGER :: cnt, cnt0                ! Counters
  !REAL, ALLOCATABLE :: obs_field(:,:) ! Array for observation field read from file
  REAL, ALLOCATABLE :: obs_field(:) ! Array for observation field read from file
  CHARACTER(len=4) :: stepstr         ! String for time step
  real(kind=8) :: temp_coord_obs_2d(2)


! *********************************************
! *** Initialize full observation dimension ***
! *********************************************

  ! Read observation field form file
  !ALLOCATE(obs_field(ny, nx))
  ALLOCATE(obs_field(ny*nx))

!  IF (step<10) THEN
!     WRITE (stepstr, '(i1)') step
!  ELSE
!     WRITE (stepstr, '(i2)') step
!  END IF
     WRITE (stepstr, '(i4)') step

  OPEN (12, file='../obs_step'//TRIM(ADJUSTL(stepstr))//'.txt', status='old')
!  DO i = 1, ny
!     READ (12, *) obs_field(i, :)
!  END DO
  READ (12, *) obs_field
  CLOSE (12)

  ! Count observations
  cnt = 0
!  DO i = 1, ny
!     DO j = 1, nx
!        IF (obs_field(i,j) > -999.0) cnt = cnt + 1
!     END DO
!  END DO
  DO j = 1, nx*ny
     IF (obs_field(j) > -999.0) cnt = cnt + 1
  END DO

  ! Set number of observations
  dim_obs_f = cnt

  ! Initialize vector of observations and index array
  IF (ALLOCATED(obs_index)) DEALLOCATE(obs_index)
  IF (ALLOCATED(obs)) DEALLOCATE(obs)
  IF (ALLOCATED(coords_obs)) DEALLOCATE(coords_obs)
  IF (ALLOCATED(coords_obs_1d)) DEALLOCATE(coords_obs_1d)
  IF (ALLOCATED(coords_obs_2d)) DEALLOCATE(coords_obs_2d)
  ALLOCATE(obs_index(dim_obs_f))
  ALLOCATE(obs(dim_obs_f))
  ALLOCATE(coords_obs(2, dim_obs_f))
  ALLOCATE(coords_obs_1d(dim_obs_f))
  ALLOCATE(coords_obs_2d(2,dim_obs_f))

  cnt = 0
  cnt0 = 0


   DO j = 1, ny*nx
    cnt0 = cnt0 + 1
    IF (obs_field(j) > -999.0) THEN
       cnt = cnt + 1
       obs_index(cnt) = cnt0      ! Index of observation in state vector
       obs(cnt) = obs_field(j) ! Vector of observations
!       if (j==2500+(21-1)*50+1) then
!          print *,"here"
!       endif
       !coords_obs(cnt)=j
!       if (j>=6450) then
!          print *,"here"
!        endif
       call ind1d_to_coord2d(cnt0,  temp_coord_obs_2d)
!       print *,temp_coord_obs_2d
       coords_obs(1, cnt) = temp_coord_obs_2d(1)
       coords_obs(2, cnt) = temp_coord_obs_2d(2)
!       coords_obs(1,1)=0.0
!       print *,coords_obs(1,1:5)
!       print *,coords_obs(2,1:5)
    END IF
 END DO
! print *, coords_obs(1,1:61)
! print *, coords_obs(2,1:61)

! *** Clean up ***

  DEALLOCATE(obs_field)

END SUBROUTINE init_dim_obs_f_pdaf

