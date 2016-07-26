subroutine print_gauges_and_reset_nextLoc(gaugeNum, nvar)
!
!    Array of gauge data for this gauge reached max capacity
!    print to file.

      implicit none
      integer :: gaugeNum,j,inum,k,idigit,ipos,myunit,nvar
      character*14 :: fileName
      integer :: omp_get_thread_num, mythread

      ! open file for gauge gaugeNum, figure out name
      ! not writing gauge number since it is in file name now
      ! status is old, since file name and info written for
      ! each file in in set_gauges.
      !
      ! NB: output written in different order, losing backward compatibility


      fileName = 'gaugexxxxx.txt'    ! NB different name convention too
      inum = igauge(gaugeNum)
      do ipos = 10,6,-1              ! do this to replace the xxxxx in the name
         idigit = mod(inum,10)
         fileName(ipos:ipos) = char(ichar('0') + idigit)
         inum = inum / 10
      end do


      mythread = 0
!$    mythread = omp_get_thread_num()
      myunit = OUTGAUGEUNIT+mythread

!     add thread number of outgaugeunit to make a unique unit number.
!     ok since writing to a unique file. in serial, still using only IOUTGAUGEUNIT
      open(unit=myunit, file=fileName, status='old',    &
           position='append', form='formatted')

      ! called either because array is full (write MAXDATA amount of gauge data)
      ! or checkpoint time, so write whatever is in array and reset.
      ! nextLoc has already been increment before this subr. called
      do j = 1, nextLoc(gaugeNum)-1
        write(myunit,100) levelArray(j,gaugeNum),(gaugeArray(k,j,gaugeNum),k=1,5)
      end do
      nextLoc(gaugeNum) = 1

      ! if you want to modify number of digits printed, modify this...
100     format(i5.2, 5e15.7)

      ! close file
      close(myunit)

      end subroutine print_gauges_and_reset_nextLoc
