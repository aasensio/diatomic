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
	character(len=2) :: temp
	character(len=11) :: temp2
		
		open(unit=2,file=filename,action='read',status='old')
		read(2,FMT='(11X,F)') tran%wavelength		
		read(2,*)
		read(2,*)
		read(2,*)
		read(2,FMT='(7X,F)') tran%upper%Lambda
		read(2,FMT='(2X,F)') tran%upper%S
		read(2,FMT='(2X,F)') tran%upper%J
		read(2,FMT='(2X,F)') tran%upper%i_spin
		read(2,FMT='(4X,F)') tran%upper%parity
		read(2,FMT='(5X,F)') tran%upper%B_rot
		read(2,FMT='(5X,F)') tran%upper%D_rot
		read(2,FMT='(5X,F)') tran%upper%H_rot
		read(2,FMT='(11X,F)') tran%upper%gamma_spin
		read(2,FMT='(10X,F)') tran%upper%lambda_ss
		read(2,FMT='(2X,F)') tran%upper%A
		read(2,FMT='(4X,F)') tran%upper%A_D		
		read(2,FMT='(3X,F)') tran%upper%g_L
		read(2,FMT='(3X,F)') tran%upper%g_S
		read(2,FMT='(3X,F)') tran%upper%g_r
		! FALTA LEER PARITY_SIGMA_LEVEL		

		read(2,*)
		read(2,*) 
		read(2,*) 
		read(2,*) 
		read(2,FMT='(7X,F)') tran%lower%Lambda
		read(2,FMT='(2X,F)') tran%lower%S
		read(2,FMT='(2X,F)') tran%lower%J
		read(2,FMT='(2X,F)') tran%lower%i_spin
		read(2,FMT='(4X,F)') tran%lower%parity
		read(2,FMT='(5X,F)') tran%lower%B_rot
		read(2,FMT='(5X,F)') tran%lower%D_rot
		read(2,FMT='(5X,F)') tran%lower%H_rot
		read(2,FMT='(11X,F)') tran%lower%gamma_spin
		read(2,FMT='(10X,F)') tran%lower%lambda_ss
		read(2,FMT='(2X,F)') tran%lower%A
		read(2,FMT='(4X,F)') tran%lower%A_D
		read(2,FMT='(3X,F)') tran%lower%g_L
		read(2,FMT='(3X,F)') tran%lower%g_S
		read(2,FMT='(3X,F)') tran%lower%g_r		
!		tran%lower%parity_sigma_level = 1.d0
		
		close(2)
	end subroutine read_transition

end module i_o_routines
