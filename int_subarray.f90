program subarray
	use mpi
	implicit none

	integer					:: i, j
    	integer, parameter    			:: root = 0
    	integer 				:: rank, comsize
	integer, parameter			:: tag1 = 1, tag2 = 2
	integer					:: ierr, status(mpi_status_size)

	integer :: newtype
	integer, dimension(2)   		:: sizes, subsizes, starts	
	integer					:: global(6,6)
	

	integer, allocatable, dimension (:,:)	:: local 		


	sizes    = (/6,6/)     ! size of global array
	subsizes = (/3,3/)     ! size of sub-region 
	starts   = (/0,0/)     ! indices of the starting point of the global array
                     	       

   	call MPI_Init(ierr)
    		call MPI_Comm_size(MPI_COMM_WORLD, comsize, ierr)
    		call MPI_Comm_rank(MPI_COMM_WORLD, rank, ierr)

!*************** Master's task *********************************
	
 		if (rank == root) then

		! allocate global array
!			allocate(global(6,6))	you can't intitialise an allocatable array using data statement

		! initialise global matrix
			data global(1:3,1:3) / 9*0  / ! 1st block		! here 6 is the number of occurences  of 0
  			data global(4:6,1:3) / 9*1  / ! 2nd block		! here 6 is the number of occurences  of 1
   			data global(1:3,4:6) / 9*2  / ! 3rd block		
   			data global(4:6,4:6) / 9*3  / ! 4th block

		! print the global data
			write(*,'(6(i1,2x))')(global(i,:),i = 1,6)
			write(*,*) " "

		! create new datatype

			call MPI_Type_create_subarray(2, sizes, subsizes, starts, MPI_ORDER_FORTRAN, MPI_INTEGER, newtype, ierr)
			call MPI_Type_commit(newtype, ierr)

		! send parts of glabal matrix to childs

    			call MPI_Send(global(4,1), 1, newtype, 1, tag2, MPI_COMM_WORLD, ierr)
    			call MPI_Send(global(1,4), 1, newtype, 2, tag2, MPI_COMM_WORLD, ierr)
    			call MPI_Send(global(4,4), 1, newtype, 3, tag2, MPI_COMM_WORLD, ierr)

		else

! **************************** child's task **************************************
			allocate(local(3,3))
  			call MPI_Recv(local, 9, MPI_INTEGER, 0, tag2, MPI_COMM_WORLD, status, ierr)
		! print the local data
			write(*,*) " "
			write(*,'(3(i1,2x))')(local(i,:),i = 1,3)
	
		endif

    	call MPI_Finalize(ierr)

end program subarray
