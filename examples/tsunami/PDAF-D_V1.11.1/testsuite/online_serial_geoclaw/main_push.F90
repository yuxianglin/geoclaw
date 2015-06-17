      PROGRAM MAIN_PUSH
      IMPLICIT NONE
      
      CALL INIT_PARALLEL_PDAF(0,0); 

      CALL INIT_PDAF();

!     Time stepping loop
      DO I=1,100
          ASSIMILATE_PDAF
      ENDDO


      END PROGRAM MAIN_PUSH
