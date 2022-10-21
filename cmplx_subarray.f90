program subarray
	use mpi
	implicit none

	integer					:: i, j
    	integer, parameter    			:: root = 0
    	integer 				:: rank, comsize
	integer, parameter			:: tag = 1
	integer					:: ierr, status(mpi_status_size)

	integer :: newtype
	integer, dimension(2)   		:: sizes, subsizes, starts	
	complex					:: global(4,4)
	
    	complex, allocatable, dimension (:,:) 	:: local
 		
	sizes    = (/4,4/)     ! size of global array
	subsizes = (/2,2/)     ! size of sub-region 
	starts   = (/0,0/)     ! indices of the starting point of the global_int array
                     	       

   	call MPI_Init(ierr)
    		call MPI_Comm_size(MPI_COMM_WORLD, comsize, ierr)
    		call MPI_Comm_rank(MPI_COMM_WORLD, rank, ierr)

!*************** Master's task *********************************
	
 		if (rank == root) then

		! initialise global matrix
			do i= 1, 2
				do j = 1, 2
					global(i,j) = (0,0)
				
				enddo
			enddo
			do i= 3, 4
				do j = 1, 2
					global(i,j) = (1,1)
				
				enddo
			enddo
			do i= 1, 2
				do j = 3, 4
					global(i,j) = (2,2)
				
				enddo
			enddo
			do i= 3, 4
				do j = 3, 4
					global(i,j) = (3,3)
				
				enddo
			enddo

		! print the global data
			write(*,*) " "
!			write(*,*)(global(i,:),i = 1,4)
			write(*,"(2(2('{',f3.1,',',f3.1,'}',2x),'|'))")(global(i,:),i = 1,4)

		! create new datatype
			call MPI_Type_create_subarray(2, sizes, subsizes, starts, MPI_ORDER_FORTRAN, MPI_COMPLEX, newtype, ierr)
			call MPI_Type_commit(newtype, ierr)
! stop
		! send parts of glabal matrix to childs
    			call MPI_Send(global(3,1), 1, newtype, 1, tag, MPI_COMM_WORLD, ierr)
    			call MPI_Send(global(1,3), 1, newtype, 2, tag, MPI_COMM_WORLD, ierr)
    			call MPI_Send(global(3,3), 1, newtype, 3, tag, MPI_COMM_WORLD, ierr)

		else


! **************************** child's task **************************************
			allocate(local(2,2))
    			call MPI_Recv(local, 4, MPI_COMPLEX, 0, tag, MPI_COMM_WORLD, status, ierr)

		! print the local data
			if (rank  == 1) then
				write(*,*) " matrix received by rank 1"
				do i = 1,2
					write(*,*) local(i,:)
				enddo
!				write(*,"(2('{',f3.1,',',f3.1,'}',2x))")(local(i,:),i = 1,2)
!				write(*,*)" "
!				call sleep(2)
 			endif			

!			elseif(rank == 2) then
!				write(*,*) " matrix received by rank 2"
!				write(*,*)(local(i,:),i = 1,2)
!				write(*,"(2('{',f3.1,',',f3.1,'}',2x))")(local(i,:),i = 1,2)
!				write(*,*)" "
!				call sleep(2)
!
!			elseif(rank == 3)then
!				write(*,*) " matrix received by rank 3"
!				write(*,*)(local(i,:),i = 1,2)
!				write(*,"(2('{',f3.1,',',f3.1,'}',2x))")(local(i,:),i = 1,2)
!				write(*,*)" "
!			endif
					
		endif

    	call MPI_Finalize(ierr)

end program subarray
