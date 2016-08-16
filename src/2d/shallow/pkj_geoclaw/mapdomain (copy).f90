module mapdomain
        use amr_module
!        use amr_module, only: numgrids, rnode, node, lstart, ndihi, ndilo, &
!        ndjhi, ndjlo, mstart, cornxlo, cornylo, levelptr

        use sortarr

        implicit none


contains
        ! This subroutine gives the ordered mptr array for the domains going
        ! from left to right and upwards
subroutine get_ordered_array(mptr_array,ordered_mptr_array)
#ifdef USE_PDAF    
        use mod_assimilation,only: first_assimilation
#endif

        implicit none

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

        !subroutine allocate_space(ptr_location, ordered_mptr_array)
        !Allocates space for nx*ny cells and sets pointers for each domain
!        subroutine allocate_space(ordered_mptr_array, ptr_location)
!                integer, intent(in) :: ordered_mptr_array(:)
!                integer,intent(inout) :: ptr_location(:)
!
!                !Local variables
!                integer :: mptr1,i, nx, ny
!
!                !allocate(ptr_location(numgrids(1)))
!                ptr_location(1)=1
!
!                do i = 2,size(ordered_mptr_array)
!                    mptr1 = ordered_mptr_array(i-1)
!                    nx = node(ndihi,mptr1) - node(ndilo,mptr1) + 1
!                    ny = node(ndjhi,mptr1) - node(ndjlo,mptr1) + 1
!                    ptr_location(i) = ptr_location(i-1) + nx*ny
!                end do
!                print *,"ptr_location = ",ptr_location
!
!        end subroutine allocate_space
!
!        !subroutine check_domain(i_global, j_global,a)
!        subroutine check_domain(i_global, j_global, domain_num)
!                integer, intent(in) :: i_global
!                integer, intent(in) :: j_global
!                integer, intent(out) :: domain_num
!
!                integer :: domain_num_x
!                integer :: domain_num_y
!
!                !To make it generic, need a good subroutine to identify
!                !domain_num_y and domain_num_x from any number of cells in the
!                !local domains. Basically 50 must be replaced somehow
!                domain_num_y = ((j_global-1)/50) + 1
!                domain_num_x = ((i_global-1)/50) + 1
!                domain_num = domain_num_y + (domain_num_x-1)*2
!                print *,"Global_coord = ", j_global, i_global
!                print *, "domain coordinate - ",domain_num_x, domain_num_y
!                print *,"Domain number = ", domain_num
!        end subroutine check_domain
!
!        subroutine traverse_global(field_array, ordered_mptr_array, &
!                        total_array,global_to_totalarray_map, ptr_location)
!                !State values in the physical domain
!                real(kind=8), intent(in) :: field_array(:,:)
!                integer, intent(in) :: ordered_mptr_array(:)
!                real(kind=8), allocatable, intent(out) :: total_array(:)
!                integer, allocatable, intent(out) :: global_to_totalarray_map(:)
!                integer, allocatable, intent(out) :: ptr_location(:) !Pointer
!
!                ! Local variables
!                integer :: i_global, j_global
!                integer :: counter = 0
!                integer :: domain_num
!                real(kind=8) :: field_value
!
!                allocate(total_array(10000))
!                allocate(global_to_totalarray_map(10000))
!                allocate(ptr_location(numgrids(1)))
!                call allocate_space(ordered_mptr_array, ptr_location)
!                !For making it generic, 100 has to be replaced with total number
!                !of cells in x and y direction in the domain
!                do i_global = 1,100
!                    do j_global = 1,100
!
!                        counter = counter + 1
!                        field_value = field_array(i_global, j_global)
!                        call check_domain(i_global, j_global, domain_num)
!
!                        call fill_value(domain_num, field_value, counter, &
!                                ptr_location, total_array, &
!                                global_to_totalarray_map)
!                    end do
!                end do
!                print *,"muhaha"
!                total_nx = node(ndihi,mstart) - node(ndilo,mstart) + 1
!                total_ny = node(ndjhi,mstart) - node(ndjlo,mstart) + 1
!                print *,mstart, total_nx, total_ny
!                print *,"global2totalarray = ", global_to_totalarray_map
!                open(unit=24,file="total_array")
!                do i_global = 1,10000
!                    write(24,*) total_array(i_global)
!                end do
!                close(24)
!        end subroutine traverse_global
!
!        subroutine fill_value(domain_num, field_val, counter, &
!                        ptr_location, total_array, &
!                        global_to_totalarray_map)
!                integer, intent(in) :: domain_num
!                real(kind=8),intent(in) :: field_val
!                integer, intent(in) :: counter
!                integer,intent(inout) :: ptr_location(:)
!                real(kind=8), intent(inout) :: total_array(:)
!                integer, intent(inout) :: global_to_totalarray_map(:)
!
!                !real(kind=8), intent(inout) :: total_array(:)
!                global_to_totalarray_map(counter) = ptr_location(domain_num)
!                total_array(ptr_location(domain_num)) = field_val
!                ptr_location(domain_num) = ptr_location(domain_num) + 1
!                print *,domain_num,ptr_location(domain_num)
!
!        end subroutine fill_value
!        subroutine field_twod2oned(field,field_1d_mptr)
!                real

!        subroutine obtain_local_domain_width(ordered_mptr_array)
!          integer, intent(in) :: ordered_mptr_array(:)
!
!          integer :: i2, mptr1
!
!          do i2 = 1,size(ordered_mptr_array)
!                  mptr1 = ordered_mptr_array(i2)
!                  nx = node(ndihi,mptr1) - node(ndilo, mptr1) + 1
!                  ny = node(ndjhi,mptr1) - node(ndjlo, mptr1) + 1
!                  Ntot = nx*ny
!                  DO i_pkj = 1,Ntot
!                      READ(24,*) field((i2-1)*Ntot+i_pkj)
!                  !    print *,i_pkj
!                  ENDDO
!          ENDDO
!        end subroutine obtain_local_domain_width
subroutine field_conversion()

!          use amr_module
          use mod_model,only: field,nx,ny
!          use mapdomain,only: get_ordered_array


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
!      use amr_module
!      use mapdomain,only:get_ordered_array
      implicit none

      integer, intent(in) :: index_1d
!      integer, intent(in) :: numdomains_in_x, numdomains_in_y
      real(kind=8), intent(inout) :: coord_2d(2)

      !Local variables
      integer, allocatable :: mptr_array(:), ordered_mptr_array(:)
      integer :: i,mptr,nx,ny,row_l,column_l,temp(1),ind
      real(kind=8) :: xlow,ylow,dx,dy
      integer :: cellnum , remaining_cells
!      integer :: domain_num_row, domain_num_column
!      integer :: local_row, local_column
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
               coord_2d(1)=xlow+(row_l-0.5)*dx
               coord_2d(2)=ylow+(column_l-0.5)*dy
               return
           endif
        enddo
        end subroutine ind1d_to_coord2d
    subroutine replace_alloc(nvar,naux)

        use mod_model,only:field
        implicit none
        
        integer,intent(in)::nvar
        integer,intent(in)::naux

        integer,allocatable :: mptr_array(:)
        integer,allocatable :: ordered_mptr_array(:)
        integer :: ii,mptr,Ntot,j_pkj,i_pkj,ivar,iaux,i,j
        integer :: i_mod,j_mod,loc,locaux,mitot,mjtot,nx,ny 
        integer :: iadd,iaddaux
!        integer :: ind_alloc

        iadd(ivar,i,j)=loc+ivar-1+nvar*((j-1)*mitot+i-1)

        iaddaux(iaux,i,j)=locaux+iaux-1+naux*(i-1)+naux*mitot*(j-1)

        call get_ordered_array(mptr_array,ordered_mptr_array)   
        call field_conversion()
!        print *,field(5050)
        do ii = 1,size(ordered_mptr_array)
           mptr = ordered_mptr_array(ii)
           nx = node(ndihi,mptr) - node(ndilo, mptr) + 1
           ny = node(ndjhi,mptr) - node(ndjlo, mptr) + 1
           mitot   = nx + 2*nghost
           mjtot   = ny + 2*nghost
           loc     = node(store1, mptr)
           locaux  = node(storeaux,mptr)

!                  print *,"loc=",loc
!                  print *,"mptr =",mptr
!                  print *, "nx = ",nx
!                  print *, "ny = ",ny
!                  print *, "corners = ",corn1,corn2

          Ntot = nx*ny
          do j_pkj = nghost+1, mjtot-nghost
            do i_pkj = nghost+1, mitot-nghost
              do ivar=1,3 !nvar
                if (abs(alloc(iadd(ivar,i_pkj,j_pkj))) < 1d-90) then
                    alloc(iadd(ivar,i_pkj,j_pkj)) = 0.d0
                endif
              enddo
                        
                i_mod = i_pkj - nghost
                j_mod = j_pkj - nghost
!                ind_alloc=iadd(1,i_pkj,j_pkj)                
!               print *,ind_alloc
                alloc(iadd(1,i_pkj,j_pkj)) = &
                field((ii-1)*Ntot+i_mod+nx*(j_mod-1)) -&
                alloc(iaddaux(1,i_pkj, j_pkj))
            enddo
         enddo
       enddo
       deallocate(mptr_array)
       deallocate(ordered_mptr_array)
       deallocate(field)
     end subroutine replace_alloc

end module mapdomain
