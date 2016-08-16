module mapdomain
        use amr_module
        use sortarr

        implicit none


contains
subroutine get_ordered_array(mptr_array,ordered_mptr_array)
#ifdef USE_PDAF    
        use mod_assimilation,only: first_assimilation
#endif

        implicit none
! This subroutine gives the ordered mptr array for the domains going
! from left to right and upwards

        !Local integers
        integer :: ind,temp(1)
        integer,allocatable, intent(out) :: mptr_array(:)
        integer, allocatable, intent(out) :: ordered_mptr_array(:)

        real(kind=8), allocatable :: corner_array(:,:)
        integer :: i_pkj
        integer :: mptr
#ifdef USE_PDAF
        if (first_assimilation) then
!                    print *,"in get ordered array first assimilation=true"
            ind=1
        else
!                    print *,"in get ordered array first assimilation=false"
            temp=minloc(numgrids)! get the last non-zero number, ad-hoc
            ind=temp(1)-1
        endif
#endif
!                ind=1
        allocate(mptr_array(numgrids(ind)))
        allocate(ordered_mptr_array(numgrids(ind)))
        allocate(corner_array(numgrids(ind),2))



        mptr = lstart(ind)
        do i_pkj=1,numgrids(ind)
          mptr_array(i_pkj)=mptr
          corner_array(i_pkj,1) = rnode(cornxlo,mptr)
          corner_array(i_pkj,2) = rnode(cornylo,mptr)
          mptr = node(levelptr,mptr)
        enddo

        call set_global(corner_array,mptr_array,ordered_mptr_array)

end subroutine get_ordered_array

subroutine field_conversion()

          use mod_model,only: field,nx,ny

          implicit none

          real(kind=8), allocatable :: field_natural(:),field_mptr(:)
          integer, allocatable ::mptr_array(:),ordered_mptr_array(:)
          integer :: dim_ens,mptr
          integer :: ii,jj,row_l,column_l,nx_l,ny_l,row,column,Ntot
          integer :: nxlow,nylow
          integer :: ind_natural,ind_mptr
!          integer :: order_mptr
!*************field_conversion***************
! change field from natural way to mptr way
!*********************************************


          dim_ens=size(field)
          allocate(field_natural(dim_ens))
          field_natural=field
          allocate(field_mptr(dim_ens))
          call get_ordered_array(mptr_array,ordered_mptr_array)
          Ntot=0
          do ii= 1,size(ordered_mptr_array)
            mptr=ordered_mptr_array(ii)
            nx_l = node(ndihi,mptr) - node(ndilo, mptr) + 1
            ny_l = node(ndjhi,mptr) - node(ndjlo, mptr) + 1
            Ntot=Ntot+nx_l*ny_l
            do jj=1,nx_l*ny_l
!            if (jj==1) then
!              print *,"here"
!            endif
              column_l=mod(jj-1,nx_l)+1
              row_l=(jj-1)/nx_l+1
              nxlow=node(ndilo,mptr)
              nylow=node(ndjlo,mptr)
              column=nxlow+column_l
              row=nylow+row_l
              ind_natural=(row-1)*nx+column
              ind_mptr=jj+(Ntot-nx_l*ny_l)
              field_mptr(ind_mptr)=field_natural(ind_natural)
            enddo
          enddo
          field=field_mptr

      end subroutine field_conversion
subroutine ind1d_to_coord2d(index_1d, coord_2d)

      implicit none

      integer, intent(in) :: index_1d

      real(kind=8), intent(inout) :: coord_2d(2)

      !Local variables
      integer, allocatable :: mptr_array(:), ordered_mptr_array(:)
      integer :: i,mptr,nx,ny,row_l,column_l,temp(1),ind
      real(kind=8) :: xlow,ylow,dx,dy
      integer :: cellnum , remaining_cells

! :::::::::::1d to 2d :::::::::::::::::::::
! get the 2d coordinate of 1d index
! ::::::::::::::::::::::::::::::::::::::::::
!       print *, "executing the oned to twod"

       call get_ordered_array(mptr_array,ordered_mptr_array)
       cellnum=0
       temp=minloc(numgrids)
       ind=temp(1)-1

       dx=hxposs(ind)
       dy=hyposs(ind)
       do i=1,size(ordered_mptr_array)
           mptr=ordered_mptr_array(i)
           nx=node(ndihi,mptr)-node(ndilo,mptr)+1
           ny=node(ndjhi,mptr)-node(ndjlo,mptr)+1
           cellnum=cellnum+nx*ny
           if (index_1d<=cellnum) then
               xlow=rnode(cornxlo,mptr)
               ylow=rnode(cornylo,mptr)
               remaining_cells=index_1d-(cellnum-nx*ny)
               column_l=(remaining_cells -1)/nx+1
               row_l=mod(remaining_cells -1,nx)+1
               coord_2d(1)=xlow+(row_l-0.5)*dx   !center of the cell
               coord_2d(2)=ylow+(column_l-0.5)*dy!center of the cell 
               return
           endif
        enddo
        end subroutine ind1d_to_coord2d

end module mapdomain
