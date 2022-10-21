program hello
	use mpi
	implicit none

	integer :: rank, num_procs, ierr, len, rc, no_process
  	character*(MPI_MAX_PROCESSOR_NAME) name

 
	! Initialize the MPI library:
  	call MPI_INIT(ierr)
  	if (ierr .ne. MPI_SUCCESS) then
    		 write(*,*)'Error starting MPI program. Terminating.'
     	call MPI_ABORT(MPI_COMM_WORLD, rc, ierr)
  	end if 

 	! Get the rank of the processor this thread is running on.  (Each
 	! processor has a unique rank.)
 	call MPI_COMM_RANK(MPI_COMM_WORLD, rank, ierr)

	! Get the number of processors this job is using:
  	call MPI_COMM_SIZE(MPI_COMM_WORLD, num_procs, ierr)
 	if (rank == 0) then 
		write(*,*)"Number of processes :", num_procs
		no_process = num_procs
  	end if	

 	! Get the name of this processor (usually the hostname)
 	 call MPI_GET_PROCESSOR_NAME(name, len, ierr)
 	 if (ierr .ne. MPI_SUCCESS) then
     		write(*,*)'Error getting processor name. Terminating.'
     		call MPI_ABORT(MPI_COMM_WORLD, rc, ierr)
  	 end if

  	write(*,"('hello this process no.',I3,' running on the node ', a)") rank, name

	call MPI_FINALIZE(ierr)
 
end program hello
