PROGRAM MAIN
!USES
USE mod_parallel,ONLY: MPI_COMM_WORLD, MPIerr, npes_world, & 
mype_world, init_parallel, finalize_parallel
USE timer, & ! Timing
    ONLY:timeit, time_tot
USE mod_memcount, &     ! Counting allocated memory
       ONLY: memcount_ini, memcount_get

IMPLICIT NONE
CALL INIT_PARALLEL();


! **********************************************************
! ***               PROGRAM CONTROL                      ***
! **********************************************************


! ********************************
! ***      INITIALIZATION      ***
! ********************************

! *** Initial Screen output ***
  initscreen: IF (mype_world == 0) THEN

     WRITE (*, '(/8x, a/)') '+++++ PDAF Geoclaw - offline mode +++++'
     WRITE (*, '(16x, a)') 'Data assimilation with PDAF'

     IF (npes_world > 1) THEN
        WRITE (*, '(/21x, a, i3, a/)') 'Running on ', npes_world, ' PEs'
     ELSE
        WRITE (*, '(/21x, a/)') 'Running on 1 PE'
     END IF
     WRITE (*, '(/)')

  END IF initscreen

! *** set number of timers ***
  CALL timeit(6, 'ini')

! *** set first timer ***
  CALL timeit(1, 'new')

! *** set number of memory counters ***
  CALL memcount_ini(3)


! *** Initialize MPI communicators for PDAF (model and filter) ***
! *** NOTE: It is always n_modeltasks=1 for offline mode       ***
  CALL init_parallel_pdaf(0, 1)

! *** Initialize model information ***
! *** This should only be information on the model dimension
! *** Generally, this could be joined with init_pdaf.
  CALL timeit(2, 'new')
!  CALL initialize() !!!!IMPORTANT
  CALL timeit(2, 'old')



END PROGRAM
