module hamiltonian_types
	
	real(kind=8) :: fact2(0:301)
	integer :: nfac

	type type_ket_hund_a
		real(kind=8) :: Lambda, S, Sigma, Omega, J, M, N, i
	end type type_ket_hund_a
	
	type type_ket_hund_a_parity_conserving
		real(kind=8) :: Lambda, S, Sigma, Omega, J, M, i, parity
	end type type_ket_hund_a_parity_conserving

	type type_matrix_M
		real(kind=8), pointer :: matrix(:,:)
	end type type_matrix_M
	
	type type_hamiltonian
		real(kind=8) :: Lambda, S, J, N, i_spin, parity, parity_sigma_level
		integer :: hamiltonian_size
		real(kind=8), pointer :: eigenval(:), eigenval_zerofield(:), eigenval_purerotational(:)
		real(kind=8), pointer :: eigenvec(:,:)
		type(type_ket_hund_a), pointer :: ket(:), ket_nofield(:), ket_purerotation(:), ket_m_ordered(:)
		type(type_ket_hund_a_parity_conserving), pointer :: ket_par(:)
		real(kind=8) :: B_rot, D_rot, H_rot, gamma_spin, lambda_ss, A, A_D, g_L, g_S, g_r, M_max
		real(kind=8) :: B_field, Energy0
		real(kind=8), pointer :: active(:)
		integer :: n_M_blocks
		integer, pointer :: M_blocks_sizes(:), M_blocks_start(:), eigenvalues_interesting(:,:), original_order(:)
		type(type_matrix_M), pointer :: M_Hamiltonian(:), Rotation(:)
	end type type_hamiltonian

	type type_transition
		type(type_hamiltonian) :: upper, lower
		real(kind=8) :: wavelength
	end type type_transition
	
	type(type_transition) :: transition	
	real(kind=8) :: field_strength

	real(kind=8), parameter :: bohr_mag = 4.6686d-5

end module hamiltonian_types
