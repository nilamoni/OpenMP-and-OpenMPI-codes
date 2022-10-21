! This program demonstrates sending and receiving data elements from different processes using mpi

program send_receive 
	use mpi
	implicit none

!	dummy variables for loop indices
	integer		:: 	i,j

!	size of the arrays
      	integer   	::	arraysize
      	parameter(arraysize = 20)

! 	mpi related parameters 
	integer		::	ierr,rc, len		! an integer error parameter for the mpi subroutines      
	integer		::	master		! rank or id of the master or root or parent process
	parameter (master = 0)

	integer 	::	numtasks	! number of tasks or processes
	integer		::	taskid 		! task id or process id or rank of the processes 
	integer		::	dest		! rank/id of the destination process
	integer		::	source		! rank/id of the source process i.e. the rank of the process from which the receiver receives 
	
	integer		:: 	chunksize	! size of the part of the array that will be sent to different processes for doing its fft
	integer		:: 	offset		! starting array index of each chunk of the total array that needs to be sent
	integer		:: 	tag1,tag2	! tags for differentiating the datatypes while sending. tag1 is for sending integer type and 
						! tag2 is for sending character datatype
	character*(MPI_MAX_PROCESSOR_NAME) :: name	! name of the node where the code is running

	integer  	::	status(mpi_status_size)	! an argument for mpi_receive

!	arrays
      	integer, dimension(:), allocatable   	::	data, data_copy, data_fun
	character, dimension(:), allocatable	::	data_fun1

!	others

	integer, parameter 	::	delay = 5
	
	allocate(data(arraysize))					! every process will have a copy of this array
 

! ***** initializations *****
      	call mpi_init(ierr)
      		call mpi_comm_size(mpi_comm_world, numtasks, ierr)	! gives the number of processes
      		i = mod(numtasks, 4)
      		if (i .ne. 0) then
      	  		call mpi_abort(mpi_comm_world, rc, ierr)
          		stop
      		end if
     		call mpi_comm_rank(mpi_comm_world, taskid, ierr)
      		write(*,"('mpi task',i2,' has started...')") taskid
      		chunksize = (arraysize/numtasks)			! this variable is available for all process 
      		tag2 = 1
      		tag1 = 2

!***** master task only ******
      		if (taskid .eq. master) then		! we initialise the array using the master process

!       initialize the array
        		do i=1, arraysize 
          			data(i) 	= i
        		end do
			allocate(data_copy(arraysize))		! note: the array "data_copy" is allocated only in the master process
								! in other processes "data_copy" is still unallocated
			data_copy = data		
	
!       send each task its portion of the array - master keeps 1st part
        		offset = chunksize + 1
        		do dest=1, numtasks-1
          			call mpi_send(offset, 1, mpi_integer, dest, tag1, mpi_comm_world,ierr)	! what is the purpose of this call
          			call mpi_send(data(offset), chunksize, mpi_integer, dest,tag1, mpi_comm_world, ierr)
	 			call MPI_GET_PROCESSOR_NAME(name, len, ierr)
!				write(*,*)" "
		  		write(*,"('sent ',i2,' elements to task ',i2,' on node {',a12,'} offset value = ',i2)") &
			 		chunksize,dest,name,offset
!				do j=1, chunksize
!					write(*,'(5(i2,2x))',advance = 'no') data(offset+j-1)
!				enddo
!				write(*,*)" "
          			offset = offset + chunksize
        		end do

!       master does its part of the work
        		offset = 1
        		call update(data,offset, chunksize, taskid)
			write(*,*)" "
			write(*,"('data will be received from other processors after ',i2,' seconds',/)") delay 

!       wait to receive results from each task
        		do i=1, numtasks-1
          			source = i
! note: the recieve action below won't get executed until the source sends it
		 		call mpi_recv(offset, 1, mpi_integer, source, tag1,mpi_comm_world, status, ierr)
          			call mpi_recv(data(offset), chunksize, mpi_integer,source, tag1,mpi_comm_world, status, ierr)
				write(*, "('data received from task number ',i2,/)")source
				do j=1, chunksize
					write(*,'(5(i2,2x))',advance = 'no') data(offset+j-1)
				enddo
				write(*,*)" "
			
				if (source == 2) then
					allocate(data_fun(arraysize))		! similarly here, data_fun is being allocated only in the
										! master process
					data_fun = 1
					do j=1, chunksize
					 	data_fun(offset+j-1) = data(offset+j-1)
					enddo
				endif 
        		end do
			allocate(data_fun1(arraysize))
          		call mpi_recv(data_fun1(1), arraysize, mpi_character,2, tag2,mpi_comm_world, status, ierr)
			

!       display final matrix
			write(*,*)" "
			write(*,"('**************************** RESULTS ***************************')")
			write(*,"('********************** the initial matrix **********************')")
			write(*,'(20(i2,2x))') data_copy(:)

			write(*,"('********************** the modified matrix **********************')")
			write(*,'(20(i2,2x))') data(:)

			write(*,"('********************** the fun matrix number 1 **********************')")
			write(*,'(20(i2,2x))') data_fun(:)

			write(*,"('********************** the fun matrix number 2 **********************')")
			write(*,'(20(a,2x))') data_fun1(:)
      		end if


!***** non-master tasks only *****

      		if (taskid .gt. master) then

!       receive my portion of array from the master task
        		call mpi_recv(offset, 1, mpi_integer, master, tag1,mpi_comm_world, status, ierr)
        		call mpi_recv(data(offset), chunksize, mpi_integer, master,tag1, mpi_comm_world, status, ierr)

        		call update(data, offset, chunksize, taskid)

!       send my results back to the master

			call sleep(delay)
        		call mpi_send(offset, 1, mpi_integer, master, tag1,mpi_comm_world, ierr)
        		call mpi_send(data(offset), chunksize, mpi_integer, master,tag1, mpi_comm_world, ierr)
			
!			if(taskid == 1) then
!				write(*,*) data_fun1(2)			! This is gonna show error because in process 1 data_fun1 is still
!									! unallocated
!			endif	

			if(taskid == 2) then			
				allocate(data_fun1(arraysize))
				do i = 1, arraysize
					data_fun1(i) =  '$'
				enddo
   				call mpi_send(data_fun1(1), arraysize, mpi_character, master,tag2, mpi_comm_world, ierr)
				deallocate(data_fun1)
			endif
      		endif


	call mpi_finalize(ierr)

contains

	subroutine update(data, myoffset, chunksize, myid)
		implicit none
	
        	integer, intent(in)   	::	myoffset, chunksize, myid
       	 	integer, intent(inout) 	::	data(*)
		integer			::	i
	
		if(myid .gt. 0) then
        		do i=myoffset, myoffset + chunksize-1
          			data(i) = data(i) + 1 
        		end do
		else
      			do i=myoffset, myoffset + chunksize-1
          			data(i) = data(i) + 5 
        		end do
		endif
		return
	end subroutine update

end program send_receive

 

