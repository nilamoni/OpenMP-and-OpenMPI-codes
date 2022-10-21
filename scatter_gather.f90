program scatter
	use mpi
	implicit none
	integer, dimension(8) :: global      ! only root has this
	integer, dimension(2) :: local       ! everyone has this
	integer, parameter    :: root = 0
	integer :: rank, comsize
	integer :: i, ierr

	call MPI_Init(ierr)
		call MPI_Comm_size(MPI_COMM_WORLD, comsize, ierr)
		call MPI_Comm_rank(MPI_COMM_WORLD, rank, ierr)

		if (rank == root) then
    			global = [ (i, i=1,8) ]
		endif

		if(rank == root) then
			write(*,"(/,'before scattering data by root',/) ")

			write(*,"('rank : ', i1,' global =',8(i1,2x))"), rank, global(:)
		else
			write(*,"('rank : ', i1,' local = ', 2(i1,2x))") rank, local(:)	
			write(*,"('rank : ', i1,' global = ', 8(i1,2x))") rank, global(:)			
		endif

		call MPI_Scatter(global, 2, MPI_INTEGER, &    	! send everyone 2 ints from global
                 local,  2, MPI_INTEGER, &    			! each proc recieves 2 into
                 root,                   &    			! sending process is root,
                 MPI_COMM_WORLD, ierr)        			! all procs in COMM_WORLD participate
		
		if(rank == root) then
			write(*,"(/,'after scattering data by root',/) ")

			write(*,"('rank : ', i1,' global =',8(i1,2x))"), rank, global(:)
		else
			write(*,"('rank : ', i1,' local = ', 2(i1,2x))") rank, local(:)	
			write(*,"('rank : ', i1,' global = ', 8(i1,2x))") rank, global(:)			
		endif

		call MPI_Gather (local,  2, MPI_INTEGER, &    	! everyone sends 2 ints from local
                global, 2, MPI_INTEGER, &    			! root receives 2 ints each proc into global
                root,                   &    			! receiving process is root,
                MPI_COMM_WORLD, ierr)        			! all procs in COMM_WORLD participate

		if(rank == root) then
			write(*,"(/,'after gathering data from childs by root',/) ")

			write(*,"('rank : ', i1,' global =',8(i1,2x))"), rank, global(:)
		else
			write(*,"('rank : ', i1,' local = ', 2(i1,2x))") rank, local(:)	
			write(*,"('rank : ', i1,' global = ', 8(i1,2x))") rank, global(:)			
		endif


	call mpi_finalize(ierr)

end program scatter



