module build_hamiltonian_routines
use hamiltonian_types
use maths
implicit none
contains

!----------------------------------------------------------------------
!----------------------------------------------------------------------
! Spin-orbit coupling Hamiltonian
!----------------------------------------------------------------------
!----------------------------------------------------------------------
	function H_spin_orbit(level,ketl,ketr)
	real(kind=8) :: H_spin_orbit
	type(type_hamiltonian) :: level
	type(type_ket_hund_a) :: ketl, ketr
	real(kind=8) :: delta_Lambda, delta_S, delta_Sigma, delta_Omega, delta_J, delta_M, deltas
	real(kind=8) :: Lambda, S, Sigma, Omega, J, M, term1, term2

		delta_Lambda = delta(ketl%Lambda,ketr%Lambda)
		delta_S = delta(ketl%S,ketr%S)
		delta_Sigma = delta(ketl%Sigma,ketr%Sigma)
		delta_Omega = delta(ketl%Omega,ketr%Omega)
		delta_J = delta(ketl%J,ketr%J)
		delta_M = delta(ketl%M,ketr%M)
		
		deltas = delta_Lambda*delta_S*delta_Sigma*delta_Omega*delta_J*delta_M

		Lambda = ketr%Lambda
		S = ketr%S
		Sigma = ketr%Sigma
		Omega = ketr%Omega
		J = ketr%J
		M = ketr%M

		term1 = level%A * Lambda * Sigma
		term2 = level%A_D * Lambda * Sigma * (J*(J+1.d0) - Omega**2 + S*(S+1.d0) - Sigma**2)
		H_spin_orbit = deltas * (term1 + term2)

	end function H_spin_orbit

!----------------------------------------------------------------------
!----------------------------------------------------------------------
! Rotational Hamiltonian
!----------------------------------------------------------------------
!----------------------------------------------------------------------
	function H_rotational(level,ketl,ketr)
	real(kind=8) :: H_rotational
	type(type_hamiltonian) :: level
	type(type_ket_hund_a) :: ketl, ketr
	real(kind=8) :: delta_Lambda, delta_S, delta_Sigma, delta_Omega, delta_J, delta_M, deltas
	real(kind=8) :: Lambda, S, Sigma, Omega, J, M, Sigma_p, Omega_p, term1, term2
	real(kind=8) :: i, q

		delta_Lambda = delta(ketl%Lambda,ketr%Lambda)
		delta_S = delta(ketl%S,ketr%S)
		delta_Sigma = delta(ketl%Sigma,ketr%Sigma)
		delta_Omega = delta(ketl%Omega,ketr%Omega)
		delta_J = delta(ketl%J,ketr%J)
		delta_M = delta(ketl%M,ketr%M)
		
		deltas = delta_Lambda*delta_S*delta_J*delta_M

		Lambda = ketr%Lambda
		S = ketr%S
		Sigma = ketr%Sigma
		Sigma_p = ketl%Sigma
		Omega = ketr%Omega
		Omega_p = ketl%Omega
		J = ketr%J
		M = ketr%M

		term1 = level%B_rot * delta_Sigma * delta_Omega * (J*(J+1.d0) - Omega**2 + S*(S+1.d0) - Sigma**2)
		term2 = 0.d0
		do i = 0, 1
			q = -1 + 2*i
			term2 = term2 - 2.d0*level%B_rot * (-1.d0)**(J-Omega_p+S-Sigma_p) * &
				w3js(int(2*J),2,int(2*J),-int(2*Omega_p),int(2*q),int(2*Omega))*&
				w3js(int(2*S),2,int(2*S),-int(2*Sigma_p),int(2*q),int(2*Sigma))*&
				sqrt(J*(J+1.d0)*(2.d0*J+1.d0)*S*(S+1.d0)*(2.d0*S+1.d0))
		enddo
		H_rotational = deltas * (term1 + term2)

	end function H_rotational

!----------------------------------------------------------------------
!----------------------------------------------------------------------
! Rotational Centrifugal Distortion Hamiltonian
!----------------------------------------------------------------------
!----------------------------------------------------------------------
	function H_rotational_cd4(level,ketl,ketr)
	real(kind=8) :: H_rotational_cd4
	type(type_hamiltonian) :: level
	type(type_ket_hund_a) :: ketl, ketr
	real(kind=8) :: delta_Lambda, delta_S, delta_Sigma, delta_Omega, delta_J, delta_M, deltas
	real(kind=8) :: Lambda, S, Sigma, Omega, J, M, Sigma_p, Omega_p, Omega_pp, Sigma_pp, term1, term2, term3
	real(kind=8) :: i, q

		delta_Lambda = delta(ketl%Lambda,ketr%Lambda)
		delta_S = delta(ketl%S,ketr%S)
		delta_Sigma = delta(ketl%Sigma,ketr%Sigma)
		delta_Omega = delta(ketl%Omega,ketr%Omega)
		delta_J = delta(ketl%J,ketr%J)
		delta_M = delta(ketl%M,ketr%M)
		
		deltas = delta_Lambda*delta_S*delta_J*delta_M

		Lambda = ketr%Lambda
		S = ketr%S
		Sigma = ketr%Sigma
		Sigma_p = ketl%Sigma
		Omega = ketr%Omega
		Omega_p = ketl%Omega
		J = ketr%J
		M = ketr%M

		term1 = -level%D_rot * delta_Sigma * delta_Omega * (J*(J+1.d0) - Omega**2 + S*(S+1.d0) - Sigma**2)**2

		term2 = 0.d0
		do i = 0, 1
			q = -1 + 2*i
			Omega_pp = Omega - q
			Sigma_pp = Sigma - q
			term2 = term2 - 4.d0*level%D_rot*delta_Sigma*delta_Omega*&				
				w3js(int(2*J),2,int(2*J),-int(2*Omega),int(2*q),int(2*Omega_pp))**2 *&
				w3js(int(2*S),2,int(2*S),-int(2*Sigma),int(2*q),int(2*Sigma_pp))**2 *&
				(J*(J+1d0)*(2.d0*J+1.d0)*S*(S+1.d0)*(2.d0*S+1.d0))
		enddo

		term3 = 0.d0
		do i = 0, 1
			q = -1 + 2*i
			term3 = term3 + 2.d0*level%D_rot*(-1.d0)**(J-Omega_p+S-Sigma_p) * &
				w3js(int(2*J),2,int(2*J),-int(2*Omega_p),int(2*q),int(2*Omega))*&
				w3js(int(2*S),2,int(2*S),-int(2*Sigma_p),int(2*q),int(2*Sigma))*&
				sqrt(J*(J+1.d0)*(2.d0*J+1.d0)*S*(S+1.d0)*(2.d0*S+1.d0)) * &
				(2.d0*J*(J+1.d0)-Omega**2-Omega_p**2+2.d0*S*(S+1.d0)-Sigma**2-Sigma_p**2)
		enddo
		H_rotational_cd4 = deltas * (term1 + term2 + term3)

	end function H_rotational_cd4

!----------------------------------------------------------------------
!----------------------------------------------------------------------
! Spin-rotation coupling Hamiltonian
!----------------------------------------------------------------------
!----------------------------------------------------------------------
	function H_spin_rotation(level,ketl,ketr)
	real(kind=8) :: H_spin_rotation
	type(type_hamiltonian) :: level
	type(type_ket_hund_a) :: ketl, ketr
	real(kind=8) :: delta_Lambda, delta_S, delta_Sigma, delta_Omega, delta_J, delta_M, deltas
	real(kind=8) :: Lambda, S, Sigma, Omega, J, M, Sigma_p, Omega_p, term1, term2, term3
	real(kind=8) :: i, q

		delta_Lambda = delta(ketl%Lambda,ketr%Lambda)
		delta_S = delta(ketl%S,ketr%S)
		delta_Sigma = delta(ketl%Sigma,ketr%Sigma)
		delta_Omega = delta(ketl%Omega,ketr%Omega)
		delta_J = delta(ketl%J,ketr%J)
		delta_M = delta(ketl%M,ketr%M)
		
		deltas = delta_Lambda*delta_S*delta_J*delta_M

		Lambda = ketr%Lambda
		S = ketr%S
		Sigma = ketr%Sigma
		Sigma_p = ketl%Sigma
		Omega = ketr%Omega
		Omega_p = ketl%Omega
		J = ketr%J
		M = ketr%M

		term1 = level%gamma_spin * delta_Sigma * delta_Omega * (Omega*Sigma - S*(S+1.d0))

		term2 = 0.d0
		do i = 0, 1
			q = -1 + 2*i
			term2 = term2 + level%gamma_spin * (-1.d0)**(J-Omega_p+S-Sigma_p) * &
				w3js(int(2*J),2,int(2*J),-int(2*Omega_p),int(2*q),int(2*Omega))*&
				w3js(int(2*S),2,int(2*S),-int(2*Sigma_p),int(2*q),int(2*Sigma))*&
				sqrt(J*(J+1.d0)*(2.d0*J+1.d0)*S*(S+1.d0)*(2.d0*S+1.d0))
		enddo
		H_spin_rotation = deltas * (term1 + term2)

	end function H_spin_rotation


!----------------------------------------------------------------------
!----------------------------------------------------------------------
! Zeeman Hamiltonian
!----------------------------------------------------------------------
!----------------------------------------------------------------------
	function H_Zeeman(level,ketl,ketr)
	real(kind=8) :: H_Zeeman
	type(type_hamiltonian) :: level
	type(type_ket_hund_a) :: ketl, ketr
	real(kind=8) :: delta_Lambda, delta_S, delta_Sigma, delta_Omega, delta_J, delta_M, deltas
	real(kind=8) :: Lambda, S, Sigma, Omega, J, M, Sigma_p, Omega_p, J_p, term1, term2, term3
	real(kind=8) :: i, q, g_L, g_S, g_r

		delta_Lambda = delta(ketl%Lambda,ketr%Lambda)
		delta_S = delta(ketl%S,ketr%S)
		delta_Sigma = delta(ketl%Sigma,ketr%Sigma)
		delta_Omega = delta(ketl%Omega,ketr%Omega)
		delta_J = delta(ketl%J,ketr%J)
		delta_M = delta(ketl%M,ketr%M)
		
		deltas = delta_S*delta_M

		Lambda = ketr%Lambda
		S = ketr%S
		Sigma = ketr%Sigma
		Sigma_p = ketl%Sigma
		Omega = ketr%Omega
		Omega_p = ketl%Omega
		J = ketr%J
		J_p = ketl%J
		M = ketr%M
		
		g_L = level%g_L
		g_r = level%g_r
		g_S = level%g_S

		term2 = 0.d0
		do i = 0, 2
			q = -1 + i

			term1 = (g_L+g_r)*Lambda*delta_Sigma + (g_S+g_r)*(-1.d0)**(S-Sigma_p) * &
				w3js(int(2*S),2,int(2*S),-int(2*Sigma_p),int(2*q),int(2*Sigma)) * &
				sqrt(S*(S+1.d0)*(2.d0*S+1.d0))

			term2 = term2 + (-1.d0)**(J_p-M+J_p-Omega_p) * &
				sqrt((2.d0*J_p+1.d0)*(2.d0*J+1.d0)) * &
				w3js(int(2*J_p),2,int(2*J),-int(2*M),0,int(2*M))*&
				w3js(int(2*J_p),2,int(2*J),-int(2*Omega_p),int(2*q),int(2*Omega)) * term1
				
		enddo
		
		term2 = bohr_mag * level%B_field * delta_Lambda * term2

		term3 = -g_r * bohr_mag * level%B_field * M * delta_J*delta_Sigma*delta_Lambda
		
		H_Zeeman = deltas * (term2 + term3)

	end function H_Zeeman


!----------------------------------------------------------------------
!----------------------------------------------------------------------
! Builds the basis set and the Hamiltonian
!----------------------------------------------------------------------
!----------------------------------------------------------------------
	subroutine build_hamiltonian(level)
	type(type_hamiltonian) :: level
	real(kind=8) :: Lambda, S, Sigma, Omega, J, M, lande, landeb
	integer :: a, b, c, d, n, a_max, nrot
	real(kind=4) :: tiempo_antes, tiempo_despues
	real(kind=8), allocatable :: caca(:,:)

		print *, 'Lambda : ', level%Lambda
		print *, 'S : ', level%S
		print *, 'J : ', level%J
		print *, 'B_rot : ', level%B_rot
		print *, 'D_rot : ', level%D_rot
		print *, 'H_rot : ', level%H_rot
		print *, 'gamma_spin : ', level%gamma_spin
		print *, 'A : ', level%A
		print *, 'A_D : ', level%A_D
		print *, 'g_L : ', level%g_L
		print *, 'g_S : ', level%g_S
		print *, 'g_r : ', level%g_r 


!------------------------------------------------		
		print *, 'Generating basis set...'
! First count the number of elements of the basis in which the Hamiltonian is written
		n = 0
		S = level%S

		a_max = 2
		if (level%Lambda == 0.d0) a_max = 1

		do a = 1, a_max
			Lambda = (-1)**a * level%Lambda
			do b = 0, 2*S				
				Sigma = -S + b
				Omega = Lambda + Sigma					
				do c = 1, 3
					J = level%J + c - 2
					if (J >= 0.d0) then
						do d = 0, 2*J
							M = -J + d
							n = n + 1	
						enddo	
					endif
				enddo
			enddo
		enddo

		level%hamiltonian_size = n
		allocate(level%ket(n))
		allocate(level%ket_m_ordered(n))
		allocate(level%ket_nofield(n))
		allocate(level%ket_purerotation(n))
		allocate(level%matrix(n,n))
		allocate(level%matrix_zero_field(n,n))
		allocate(level%eigenvec(n,n))
		allocate(level%eigenval(n))
		allocate(level%eigenval_zerofield(n))
		allocate(level%active(n))

		print *, 'Size of the basis : ', level%hamiltonian_size
		
		n = 0
! Then generate all the basis
		S = level%S

		a_max = 2
		if (level%Lambda == 0.d0) a_max = 1

		do a = 1, a_max
			Lambda = (-1)**a * level%Lambda
			do b = 0, 2*S				
				Sigma = -S + b
				Omega = Lambda + Sigma					
				do c = 1, 3
					J = level%J + c - 2
					if (J >= 0.d0) then
						do d = 0, 2*J
							M = -J + d
							n = n + 1
							level%ket(n)%Lambda = Lambda
							level%ket(n)%S = S
							level%ket(n)%Sigma = Sigma
							level%ket(n)%Omega = Omega
							level%ket(n)%J = J
							level%ket(n)%M = M
						enddo	
					endif
				enddo
			enddo
		enddo

!----------------------------------------------------------------------
		print *, 'Ordering basis set...'
! Order the basis set by increasing value of M
		call order_basis_set(level)

! Generate the base for the no-field case
		level%ket_nofield = level%ket
		level%ket_purerotation = level%ket

		print *, 'Building pure rotational Hamiltonian...'
! Diagonalize the pure rotational Hamiltonian
		level%B_field = 0.d0
		do a = 1, level%hamiltonian_size
			do b = 1, level%hamiltonian_size				
				level%matrix_zero_field(a,b) = H_rotational(level,level%ket(a),level%ket(b))												
			enddo
		enddo

		print *, 'Diagonalizing pure rotational Hamiltonian...'
		tiempo_antes = second()
		call diagonalize_hamiltonian(level,level%matrix_zero_field,level%eigenval_zerofield,level%eigenvec)
		tiempo_despues = second()
		
! And then assign the rotational quantum numbers for each level
		call calculate_rotational_numbers(level)
! Order the pure rotation basis
		call order_basis_set_purerotation(level)

!		do a = 1, level%hamiltonian_size
!				write(*,FMT='(I3,1X,F4.1,1X,F4.1,1X,F4.1,1X,F4.1,1X,F4.1,1X,F4.1,1X,F4.1,1X,F12.5,1X,F12.5,1X,F12.5)') &
!					a, level%ket_nofield(a)%Lambda, &
!					level%ket_nofield(a)%S, level%ket_nofield(a)%Sigma, level%ket_nofield(a)%Omega, level%ket_nofield(a)%J, &
!					level%ket(a)%M, level%ket_nofield(a)%N, level%eigenval_zerofield(a)
!		enddo

!----------------------------------------------------------------------
! Diagonalize the complete zero-field Hamiltonian
		level%B_field = 0.d0
		do a = 1, level%hamiltonian_size
			do b = 1, level%hamiltonian_size				
				level%matrix_zero_field(a,b) = level%matrix_zero_field(a,b) + &
												H_rotational_cd4(level,level%ket(a),level%ket(b)) + &
												H_spin_orbit(level,level%ket(a),level%ket(b)) + &									
												H_rotational_cd4(level,level%ket(a),level%ket(b)) + &
												H_spin_rotation(level,level%ket(a),level%ket(b))
			enddo
		enddo

		print *, 'Diagonalizing zero-field Hamiltonian...'
		tiempo_antes = second()
		call diagonalize_hamiltonian(level,level%matrix_zero_field,level%eigenval_zerofield,level%eigenvec)
		tiempo_despues = second()


!----------------------------------------------------------------------
! Diagonalize the Hamiltonian with field
		print *, 'Building Hamiltonian...'
		level%B_field = field_strength
		do a = 1, level%hamiltonian_size
			do b = 1, level%hamiltonian_size				
				level%matrix(a,b) = level%matrix_zero_field(a,b) + &
									H_Zeeman(level,level%ket(a),level%ket(b))

			enddo
		enddo
	
		print *, 'Diagonalizing Hamiltonian...'
		tiempo_antes = second()
		call diagonalize_hamiltonian(level,level%matrix,level%eigenval,level%eigenvec)
		tiempo_despues = second()
	
! Order the basis set for the no-field and the field case
		call order_basis_set_field(level)
		call order_basis_set_zerofield(level)

!		do a = 1, level%hamiltonian_size
!			if (abs(level%ket_nofield(a)%J-level%J) <= 1.d-10 .and. abs(level%ket_purerotation(a)%N-level%N) <= 1.d-10 .and. &
!				level%ket(a)%Lambda == level%Lambda) then
!			if (abs(level%ket_nofield(a)%J-level%J) <= 1.d-10 .and. abs(level%ket_nofield(a)%N-level%N) <= 1.d-10) then
!				write(*,FMT='(I3,1X,F4.1,1X,F4.1,1X,F4.1,1X,F4.1,1X,F4.1,1X,F4.1,1X,F4.1,1X,F12.5,1X,F12.5,1X,F12.5)') &
!					a, level%ket(a)%Lambda, &
!					level%ket_nofield(a)%S, level%ket_nofield(a)%Sigma, level%ket_nofield(a)%Omega, level%ket_nofield(a)%J, &
!					level%ket(a)%M, level%ket_purerotation(a)%N, level%eigenval(a), level%eigenval_zerofield(a), &
!					level%eigenval(a)-level%eigenval_zerofield(a)
!				lande = (level%eigenval(a)-level%eigenval_zerofield(a)) / (bohr_mag * level%B_field * level%ket(a)%M)
!				write(*,FMT='(A,F5.1,1X,A,F5.1,1X,A,F5.1,1X,A,F12.7,1X,A,F12.7,1X,A,F12.7,1X,A,F12.7)') 'J=', &
!					level%ket_nofield(a)%J, 'N=',level%ket_purerotation(a)%N, 'M=',level%ket(a)%M, 'E=',level%eigenval(a), &
!					'E0=',level%eigenval_zerofield(a), 'DE=',level%eigenval(a)-level%eigenval_zerofield(a), 'g=', lande
!			endif
!		enddo

! Go through all the eigenvalues selecting those which belong to the zero-field J and N value and
! select one of the two-fold degenerate Lambda-doubling levels because Lambda-doubling is not included
		level%active = -9999		
		
		do a = 1, level%hamiltonian_size
			if (abs(level%ket_nofield(a)%J-level%J) <= 1.d-10 .and. abs(level%ket_purerotation(a)%N-level%N) <= 1.d-10 .and. &
				level%ket(a)%Lambda == level%Lambda) then

! Indicate that this eigenvalue is in the set we are interested in
					level%active(a) = level%ket(a)%M

					lande = (level%eigenval(a)-level%eigenval_zerofield(a)) / (bohr_mag * level%B_field * level%ket(a)%M)
					landeb = lande_caseb(level)
					write(*,FMT='(A,F5.1,1X,A,F5.1,1X,A,F5.1,1X,A,F12.7,1X,A,F12.7,1X,A,F12.7,1X,A,F12.7,1X,A,F12.7)') 'J=', &
						level%ket_nofield(a)%J, 'N=',level%ket_purerotation(a)%N, 'M=',level%ket(a)%M, 'E=',level%eigenval(a), &
						'E0=',level%eigenval_zerofield(a), 'DE=',level%eigenval(a)-level%eigenval_zerofield(a), 'g=', lande, &
						'g(b)=',landeb
					write(20,*) level%ket(a)%m, level%eigenval(a), level%eigenval(a)-level%eigenval_zerofield(a), &
						lande, landeb

			endif
		enddo

	end subroutine build_hamiltonian

!----------------------------------------------------------------------
!----------------------------------------------------------------------
! Calculate the rotational quantum numbers
! If we include only the rotational hamiltonian, the energy can be associated with a given value of N
!----------------------------------------------------------------------
!----------------------------------------------------------------------
	subroutine calculate_rotational_numbers(level)
	type(type_hamiltonian) :: level
	integer :: i

		do i = 1, level%hamiltonian_size
			level%ket(i)%N = (-level%B_rot + sqrt(level%B_rot**2 + 4.d0 * level%B_rot*level%eigenval_zerofield(i) + &
				4.d0 * level%B_rot**2 * level%Lambda**2)) / &
				(2.d0 * level%B_rot)
			level%ket_nofield(i)%N = level%ket(i)%N
			level%ket_purerotation(i)%N = level%ket(i)%N
		enddo
	end subroutine calculate_rotational_numbers

!----------------------------------------------------------------------
!----------------------------------------------------------------------
! Re-order the Hamiltonian to make it block-diagonal
!----------------------------------------------------------------------
!----------------------------------------------------------------------
	subroutine order_basis_set(level)
	type(type_hamiltonian) :: level
	real(kind=8), allocatable :: m_vec(:)
	integer, allocatable :: indx(:)
	integer :: i
	type(type_ket_hund_a), allocatable :: ket_temp(:)

		allocate(m_vec(level%hamiltonian_size))
		allocate(indx(level%hamiltonian_size))
		
! Order the basis set by their M value
		do i = 1, level%hamiltonian_size
			m_vec(i) = level%ket(i)%M
		enddo
		call indexx(m_vec,indx)
				
		allocate(ket_temp(level%hamiltonian_size))
		ket_temp = level%ket

		do i = 1, level%hamiltonian_size
			level%ket(i) = ket_temp(indx(i))
			level%ket_m_ordered(i) = ket_temp(indx(i))
		enddo

		deallocate(m_vec)
		deallocate(indx)
		deallocate(ket_temp)
	end subroutine order_basis_set

!----------------------------------------------------------------------
!----------------------------------------------------------------------
! Re-order the eigenvalues in ascending order
!----------------------------------------------------------------------
!----------------------------------------------------------------------
	subroutine order_basis_set_field(level)
	type(type_hamiltonian) :: level
	real(kind=8), allocatable :: m_vec(:), m_vec2(:,:)
	integer, allocatable :: indx(:)
	integer :: i
	type(type_ket_hund_a), allocatable :: ket_temp(:)

		allocate(m_vec(level%hamiltonian_size))
		allocate(m_vec2(level%hamiltonian_size,level%hamiltonian_size))
		allocate(indx(level%hamiltonian_size))
		
! Order the basis set by their eigenvalue
		m_vec = level%eigenval
		m_vec2 = level%eigenvec

		call indexx(m_vec,indx)
				
		allocate(ket_temp(level%hamiltonian_size))
		ket_temp = level%ket

		m_vec = level%eigenval

		do i = 1, level%hamiltonian_size
			level%eigenval(i) = m_vec(indx(i))
			level%eigenvec(:,i) = m_vec2(:,indx(i))
			level%ket(i) = ket_temp(indx(i))
		enddo

		deallocate(m_vec)
		deallocate(indx)
		deallocate(ket_temp)
		deallocate(m_vec2)
	end subroutine order_basis_set_field

!----------------------------------------------------------------------
!----------------------------------------------------------------------
! Re-order the Hamiltonian to make it block-diagonal
!----------------------------------------------------------------------
!----------------------------------------------------------------------
	subroutine order_basis_set_zerofield(level)
	type(type_hamiltonian) :: level
	real(kind=8), allocatable :: m_vec(:)
	integer, allocatable :: indx(:)
	integer :: i
	type(type_ket_hund_a), allocatable :: ket_temp(:)

		allocate(m_vec(level%hamiltonian_size))
		allocate(indx(level%hamiltonian_size))
		
! Order the basis set by their M value
		do i = 1, level%hamiltonian_size
			m_vec(i) = level%eigenval_zerofield(i)
		enddo
		call indexx(m_vec,indx)
				
		allocate(ket_temp(level%hamiltonian_size))
		ket_temp = level%ket_nofield

		m_vec = level%eigenval_zerofield

		do i = 1, level%hamiltonian_size
			level%eigenval_zerofield(i) = m_vec(indx(i))
			level%ket_nofield(i) = ket_temp(indx(i))
		enddo

		deallocate(m_vec)
		deallocate(indx)
		deallocate(ket_temp)
	end subroutine order_basis_set_zerofield

!----------------------------------------------------------------------
!----------------------------------------------------------------------
! Re-order the Hamiltonian to make it block-diagonal
!----------------------------------------------------------------------
!----------------------------------------------------------------------
	subroutine order_basis_set_purerotation(level)
	type(type_hamiltonian) :: level
	real(kind=8), allocatable :: m_vec(:)
	integer, allocatable :: indx(:)
	integer :: i
	type(type_ket_hund_a), allocatable :: ket_temp(:)

		allocate(m_vec(level%hamiltonian_size))
		allocate(indx(level%hamiltonian_size))
		
! Order the basis set by their M value
		do i = 1, level%hamiltonian_size
			m_vec(i) = level%eigenval_zerofield(i)
		enddo
		call indexx(m_vec,indx)
				
		allocate(ket_temp(level%hamiltonian_size))
		ket_temp = level%ket_purerotation

		m_vec = level%eigenval_zerofield

		do i = 1, level%hamiltonian_size
			level%eigenval_zerofield(i) = m_vec(indx(i))
			level%ket_purerotation(i) = ket_temp(indx(i))
		enddo

		deallocate(m_vec)
		deallocate(indx)
		deallocate(ket_temp)
	end subroutine order_basis_set_purerotation

!----------------------------------------------------------------------
!----------------------------------------------------------------------
! Diagonalize the Hamiltonian making use of the block-structure
!----------------------------------------------------------------------
!----------------------------------------------------------------------
	subroutine diagonalize_hamiltonian(level,matrix,eigenval,eigenvec)
	type(type_hamiltonian) :: level
	real(kind=8) :: matrix(:,:), eigenval(:), eigenvec(:,:)
	real(kind=8), allocatable :: m_vec(:)
	real(kind=8) :: m_max, m_local
	real(kind=8), allocatable :: submatrix(:,:), subeigenval(:), subeigenvec(:,:)
	integer, allocatable :: indx(:)
	integer :: i, n_m, start, finish, n, nrot
			
		allocate(m_vec(level%hamiltonian_size))
		
! Order the basis set by their M value
		do i = 1, level%hamiltonian_size
			m_vec(i) = level%ket(i)%M
		enddo

		m_max = minval(m_vec)
		n_m = 2*abs(m_max) + 1

		do i = 1, n_m

! Select the elements of the basis with M equal to an equal value of M
			m_local = m_max + i - 1
			n = count(m_vec == m_local)
			
			allocate(submatrix(n,n))
			allocate(subeigenval(n))
			allocate(subeigenvec(n,n))
			allocate(indx(n))
						
! Built a vector with the indexes of these elements
			indx = where_index(m_vec,m_local,n)

! Build the submatrix (because the Hamiltonian is block-diagonal) and diagonalize it
			submatrix = matrix(indx,indx)
			call jacobi(submatrix,subeigenval,subeigenvec,nrot)

! Put the eigenvalues and eigenvectors on the Hamiltonian structure
			eigenval(indx) = subeigenval
			eigenvec(indx,indx) = subeigenvec

			deallocate(submatrix)
			deallocate(subeigenval)
			deallocate(subeigenvec)
			deallocate(indx)
		enddo

		deallocate(m_vec)

	end subroutine diagonalize_hamiltonian


!----------------------------------------------------------------------
!----------------------------------------------------------------------
! Return the Lande factor in case (b)
!----------------------------------------------------------------------
!----------------------------------------------------------------------
	function lande_caseb(level)
	real(kind=8) :: lande_caseb
	type(type_hamiltonian) :: level
	real(kind=8) :: J, N, S, Lambda, term1, term2
		J = level%J
		N = level%N
		S = level%S
		Lambda = level%Lambda

		term1 = (J*(J+1.d0) + S*(S+1.d0) - N*(N+1.d0) ) / (J*(J+1.d0))
		term2 = Lambda**2 * (J*(J+1.d0) + N*(N+1.d0) - S*(S+1.d0) ) / &
			(2.d0*J*(J+1.d0)*N*(N+1.d0))

		lande_caseb = term1 + term2
		
	end function lande_caseb


!----------------------------------------------------------------------
!----------------------------------------------------------------------
! Clean all the structures
!----------------------------------------------------------------------
!----------------------------------------------------------------------
	subroutine clean_all(level)
	type(type_hamiltonian) :: level
		deallocate(level%ket)
		deallocate(level%ket_m_ordered)
		deallocate(level%ket_nofield)
		deallocate(level%ket_purerotation)
		deallocate(level%matrix)
		deallocate(level%matrix_zero_field)
		deallocate(level%eigenvec)
		deallocate(level%eigenval)
		deallocate(level%eigenval_zerofield)
		deallocate(level%active)
	end subroutine clean_all

end module build_hamiltonian_routines