module hamiltonian_types
	
	real(kind=8) :: fact2(0:301)
	integer :: nfac

	type type_ket_hund_a
		real(kind=8) :: Lambda, S, Sigma, Omega, J, M, N
	end type type_ket_hund_a
	
	type type_hamiltonian
		real(kind=8) :: Lambda, S, J, N
		integer :: hamiltonian_size
		real(kind=8), pointer :: matrix_zero_field(:,:), matrix(:,:)
		real(kind=8), pointer :: eigenval(:), eigenval_zerofield(:)
		real(kind=8), pointer :: eigenvec(:,:)
		type(type_ket_hund_a), pointer :: ket(:), ket_nofield(:), ket_purerotation(:), ket_m_ordered(:)
		real(kind=8) :: B_rot, D_rot, H_rot, gamma_spin, A, A_D, g_L, g_S, g_r
		real(kind=8) :: B_field
		real(kind=8), pointer :: active(:)
	end type type_hamiltonian

	type type_transition
		type(type_hamiltonian) :: upper, lower
		integer, pointer :: ind_up(:), ind_low(:)
		real(kind=8), pointer :: str(:)
		real(kind=8) :: wavelength
	end type type_transition
	
	type(type_transition) :: transition	
	real(kind=8) :: field_strength

	real(kind=8), parameter :: bohr_mag = 4.6686d-5

end module hamiltonian_types