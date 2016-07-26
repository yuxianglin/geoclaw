!*************************************************************************
!   > File Name: field_coversion.f90
!    > Author: Shawn Lin
!    > Mail: lin_yuxiang@utexas.edu
!    > Created Time: Mon 25 Jul 2016 05:31:46 PM CDT
! ************************************************************************
      subroutine field_conversion()
          use amr_module
          use mod_model,only: field,nx,ny
          use mapdomain,only: get_ordered_array


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
            if (jj==1) then
              print *,"here"
            endif
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
