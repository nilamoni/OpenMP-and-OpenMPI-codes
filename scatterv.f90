program scatterv
	use mpi
	implicit none
	character, dimension(9) :: global
	character, dimension(3) :: local
	integer, dimension(4)   :: counts
	integer, dimension(4)   :: displs
	integer, parameter      :: root = 0
	integer :: rank, comsize
	integer :: i, ierr

	call MPI_Init(ierr)
		call MPI_Comm_size(MPI_COMM_WORLD, comsize, ierr)
		call MPI_Comm_rank(MPI_COMM_WORLD, rank, ierr)

		if (rank == root) then
    			global = [ (achar(i+ichar('a')), i=0,8) ]	! the function ichar takes a character and returns a corresponding
									! integer
									! achar takes an integer and returns a corresponding character	
		endif
		local = ['-','-','-']
		
		counts = [2,2,2,3]
		displs = [0,2,4,6]

		mycounts = counts(rank+1)

		call MPI_Scatterv(global, counts, displs,         & 	! procs i gets counts(i) chars from displs(i)
                  MPI_CHARACTER,                  &
                  local, mycounts, MPI_CHARACTER, & 			! I get mycounts chars into
                  root,                           & 			! root rank does sending
                  MPI_COMM_WORLD, ierr)             			! all procs in COMM_WORLD participate



