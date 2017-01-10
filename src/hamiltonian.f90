program hamiltonian
use maths
use build_hamiltonian_routines
use i_o_routines
use strength
use hamiltonian_types
implicit none
	character(len=80) :: filename

	call factrl
	
	open(unit=12,file='config.dat',action='read',status='old')
	read(12,*)
	read(12,*)
	read(12,*)
	read(12,*)
	read(12,'(A)') filename
	read(12,*)
	read(12,*)
	read(12,*) field_strength
	close(12)
		
	call read_transition(transition,filename)
	call build_hamiltonian(transition%upper)
	call build_hamiltonian(transition%lower)
	
	call zeeman_strength(transition)

	call clean_all(transition%upper)
	call clean_all(transition%lower)

end program hamiltonian
