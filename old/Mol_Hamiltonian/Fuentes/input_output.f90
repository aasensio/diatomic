module i_o_routines
use hamiltonian_types
use maths
implicit none
contains

!----------------------------------------------------------------------
!----------------------------------------------------------------------
! Read the data from the file
!----------------------------------------------------------------------
!----------------------------------------------------------------------
	subroutine read_transition(tran,filename)
	type(type_transition) :: tran
	character(len=80) :: filename
		
		open(unit=2,file=filename,action='read',status='old')
		read(2,FMT='(11X,F)') tran%wavelength		
		read(2,*)
		read(2,*)
		read(2,*)
		read(2,FMT='(7X,F)') tran%upper%Lambda
		read(2,FMT='(2X,F)') tran%upper%S
		read(2,FMT='(2X,F)') tran%upper%J
		read(2,FMT='(2X,F)') tran%upper%N
		read(2,FMT='(5X,F)') tran%upper%B_rot
		read(2,FMT='(5X,F)') tran%upper%D_rot
		read(2,FMT='(5X,F)') tran%upper%H_rot
		read(2,FMT='(11X,F)') tran%upper%gamma_spin
		read(2,FMT='(2X,F)') tran%upper%A
		read(2,FMT='(4X,F)') tran%upper%A_D
		read(2,FMT='(3X,F)') tran%upper%g_L
		read(2,FMT='(3X,F)') tran%upper%g_S
		read(2,FMT='(3X,F)') tran%upper%g_r

		read(2,*)
		read(2,*)
		read(2,*)
		read(2,*)
		read(2,FMT='(7X,F)') tran%lower%Lambda
		read(2,FMT='(2X,F)') tran%lower%S
		read(2,FMT='(2X,F)') tran%lower%J
		read(2,FMT='(2X,F)') tran%lower%N
		read(2,FMT='(5X,F)') tran%lower%B_rot
		read(2,FMT='(5X,F)') tran%lower%D_rot
		read(2,FMT='(5X,F)') tran%lower%H_rot
		read(2,FMT='(11X,F)') tran%lower%gamma_spin
		read(2,FMT='(2X,F)') tran%lower%A
		read(2,FMT='(4X,F)') tran%lower%A_D
		read(2,FMT='(3X,F)') tran%lower%g_L
		read(2,FMT='(3X,F)') tran%lower%g_S
		read(2,FMT='(3X,F)') tran%lower%g_r
		
		
		close(2)
	end subroutine read_transition

!----------------------------------------------------------------------
!----------------------------------------------------------------------
! Fill the level structure with the variables passed as parameters
!----------------------------------------------------------------------
!----------------------------------------------------------------------
	subroutine fill_level_data(level,L,S,J,N,Brot,Drot,Hrot,gamma,A,AD,gL,gS,gr)
	type(type_hamiltonian) :: level
	character(len=80) :: filename
	real(kind=8) :: L,S,J,N,Brot,Drot,Hrot,gamma,A,AD,gL,gS,gr

		level%Lambda = L
		level%S = S
		level%J = J
		level%N = N
		level%B_rot = Brot
		level%D_rot = Drot
		level%H_rot = Hrot
		level%gamma_spin = gamma
		level%A = A
		level%A_D = AD
		level%g_L = gL
		level%g_S = gS
		level%g_r = gr

	end subroutine fill_level_data

end module i_o_routines