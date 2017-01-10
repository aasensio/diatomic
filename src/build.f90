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
! Spin-orbit coupling Hamiltonian
!----------------------------------------------------------------------
!----------------------------------------------------------------------
	function H_spin_spin(level,ketl,ketr)
	real(kind=8) :: H_spin_spin
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

		term1 = 2.d0/3.d0 * level%lambda_ss * (3.d0*Sigma**2 - S*(S+1))
		H_spin_spin = deltas * term1

	end function H_spin_spin

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
	real(kind=8) :: Lambda, S, Sigma, Omega, J, M, lande, landeb, deltaE
	integer :: a, b, c, d, n, a_max, nrot, bb, cc, from, size, k, nSigma
	logical :: found1, found2
	real(kind=4) :: tiempo_antes, tiempo_despues
	real(kind=8), allocatable :: caca(:,:)
	integer, allocatable :: pepe(:)
	character(len=1) :: str_parity

	real(kind=8) :: m_min, m_max	

		print *, 'Lambda : ', level%Lambda
		print *, 'S : ', level%S
		print *, 'J : ', level%J
		print *, 'i : ', level%i_spin
		print *, 'B_rot : ', level%B_rot
		print *, 'D_rot : ', level%D_rot
		print *, 'H_rot : ', level%H_rot
		print *, 'gamma_spin : ', level%gamma_spin
		print *, 'lambda_ss : ', level%lambda_ss
		print *, 'A : ', level%A
		print *, 'A_D : ', level%A_D
		print *, 'g_L : ', level%g_L
		print *, 'g_S : ', level%g_S
		print *, 'g_r : ', level%g_r 


!------------------------------------------------		
		print *, 'Generating basis set without parity...'
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
		allocate(level%eigenvec(n,n))
		level%eigenvec = 0.d0
		allocate(level%eigenval(n))
		allocate(level%eigenval_zerofield(n))
		allocate(level%eigenval_purerotational(n))
		allocate(level%active(n))
		allocate(level%original_order(n))

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
		
!------------------------------------------------		
		print *, 'Generating basis set with definite parity...'
! First count the number of elements of the basis in which the Hamiltonian is written
		n = 0
		S = level%S

		Lambda = level%Lambda
		nSigma = Lambda + S - abs(Lambda-S)
		do b = 0, nSigma !2*S
			Sigma = -S + b
			Omega = abs(Lambda+Sigma)
			do c = 1, 3
				J = level%J + c - 2
				if (J >= 0.d0) then
					do d = 0, 2*J
						M = -J + d
						do a = 1, 2
							n = n + 1
						enddo
					enddo	
				endif
			enddo
		enddo	
				

		allocate(level%ket_par(n))

		print *, 'Size of the well-defined parity basis : ', n
		
		n = 0
! Then generate all the basis
		S = level%S

		Lambda = level%Lambda
		nSigma = Lambda + S - abs(Lambda-S)
		do a = 1, 2
			do b = 0, nSigma !2*S
				Sigma = -S + b
				Omega = abs(Lambda+Sigma)
				do c = 1, 3
					J = level%J + c - 2
					if (J >= 0.d0) then
						do d = 0, 2*J
							M = -J + d						
							n = n + 1
							level%ket_par(n)%Lambda = Lambda
							level%ket_par(n)%S = S
							level%ket_par(n)%Omega = Omega
							level%ket_par(n)%J = J
							level%ket_par(n)%M = M
							level%ket_par(n)%parity = (-1.d0)**a
						enddo	
					endif
				enddo
			enddo
		enddo
		

! Count the number of different values of M we have so that we can build one Hamiltonian
! for each value of M (the total Hamiltonian is diagonal in M)
		m_max = level%J + 1
		m_min = level%J - 1
		if (m_min < 0.d0) m_min = 0.d0

		print *, 'M_min : ', m_min
		print *, 'M_max : ', m_max
		print *, 'Number of M blocks : ', 2*m_max+1

		level%n_M_blocks = 2*m_max+1
		level%M_max = m_max
		allocate(level%M_blocks_sizes(level%n_M_blocks))
		allocate(level%M_blocks_start(level%n_M_blocks))
		level%M_blocks_sizes = 0

		allocate(level%M_Hamiltonian(level%n_M_blocks))
		allocate(level%Rotation(level%n_M_blocks))


		do a = 0, 2*m_max
			M = -m_max + a
			do b = 1, level%hamiltonian_size
				if (level%ket(b)%M == M) then
					level%M_blocks_sizes(a+1) = level%M_blocks_sizes(a+1) + 1
				endif
			enddo
			write(*,FMT='(A,F5.1,A,I4)') 'Size of block with M=', M, ' is : ', level%M_blocks_sizes(a+1)
			allocate(level%M_Hamiltonian(a+1)%matrix(level%M_blocks_sizes(a+1),level%M_blocks_sizes(a+1)))
			allocate(level%Rotation(a+1)%matrix(level%M_blocks_sizes(a+1),level%M_blocks_sizes(a+1)))
		enddo

		print *, 'Ordering basis set...'
! Order the basis set by increasing value of M and put the starting points of each block		
		call order_basis_set(level)

! Generate the base for the no-field and rotational case
		level%ket_nofield = level%ket
		level%ket_purerotation = level%ket

		print *, 'Building zero-field Hamiltonian...'		
!=====================================================================
! Diagonalize the complete zero-field Hamiltonian
!=====================================================================
		level%B_field = 0.d0
! Build the Hamiltonian for each value of M including everything except a magnetic field
		do a = 1, level%n_M_blocks
			bb = 0			
			do b = level%M_blocks_start(a), level%M_blocks_start(a)+level%M_blocks_sizes(a)-1
				bb = bb + 1
				cc = 0
				do c = level%M_blocks_start(a), level%M_blocks_start(a)+level%M_blocks_sizes(a)-1
					cc = cc + 1
					level%M_Hamiltonian(a)%matrix(bb,cc) = H_rotational(level,level%ket(b),level%ket(c)) + &
												H_rotational_cd4(level,level%ket(b),level%ket(c)) + &
												H_spin_orbit(level,level%ket(b),level%ket(c)) + &
												H_spin_spin(level,level%ket(b),level%ket(c)) + &
												H_spin_rotation(level,level%ket(b),level%ket(c))
				enddo
			enddo			
		enddo

		print *, 'Rotating basis set to well-defined parity...'
		call transform_hamiltonian_to_parity(level)

		print *, 'Diagonalizing zero-field Hamiltonian...'
		tiempo_antes = second()
		call diagonalize_M_hamiltonians(level,level%eigenval_zerofield,level%eigenvec)
		tiempo_despues = second()
		print *, 'Done in ', tiempo_despues-tiempo_antes, ' seconds.'

!		allocate(pepe(level%hamiltonian_size))		
!		call indexx(level%ket_par%J,pepe)
!		do a = 1, level%hamiltonian_size
!			print *, level%ket_par(pepe(a))%J, level%eigenval_zerofield(pepe(a))
!		enddo
!		deallocate(pepe)
!		pause
				
		print *, 'Assigning i value...'
		call assign_value_i(level)
				

!=====================================================================
! Diagonalize the Hamiltonian with field
!=====================================================================
		print *, 'Building magnetic Hamiltonian...'
		level%B_field = field_strength
! Build the Hamiltonian for each value of M including everything
		do a = 1, level%n_M_blocks
			bb = 0			
			do b = level%M_blocks_start(a), level%M_blocks_start(a)+level%M_blocks_sizes(a)-1
				bb = bb + 1
				cc = 0
				do c = level%M_blocks_start(a), level%M_blocks_start(a)+level%M_blocks_sizes(a)-1
					cc = cc + 1
					level%M_Hamiltonian(a)%matrix(bb,cc) = H_rotational(level,level%ket(b),level%ket(c)) + &
												H_rotational_cd4(level,level%ket(b),level%ket(c)) + &
												H_spin_orbit(level,level%ket(b),level%ket(c)) + &
												H_spin_spin(level,level%ket(b),level%ket(c)) + &
												H_spin_rotation(level,level%ket(b),level%ket(c)) + &												
												H_Zeeman(level,level%ket(b),level%ket(c))
				enddo
			enddo	
		enddo

		print *, 'Rotating basis set to well-defined parity...'
		call transform_hamiltonian_to_parity(level)
					
		print *, 'Diagonalizing magnetic Hamiltonian...'
		tiempo_antes = second()
		call diagonalize_M_hamiltonians(level,level%eigenval,level%eigenvec)
		tiempo_despues = second()
		print *, 'Done in ', tiempo_despues-tiempo_antes, ' seconds.'
				

!		allocate(pepe(level%hamiltonian_size))		
!		call indexx(level%ket_par%M,pepe)
!		do a = 1, level%hamiltonian_size
!			print *, level%ket_par(pepe(a))%M, level%ket_par(pepe(a))%i, level%ket_par(pepe(a))%parity, level%eigenval(pepe(a))
!		enddo
!		deallocate(pepe)
!		pause

!=====================================================================
! Go through all the eigenvalues selecting those which belong to the zero-field J and N value and
! select one of the two-fold degenerate Lambda-doubling levels because Lambda-doubling is not included
!=====================================================================
		level%active = -9999

		print *, 'Selecting levels...'				
		do j = 1, level%hamiltonian_size
					
			lande = (level%eigenval(j)-level%Energy0) / (bohr_mag * level%B_field * level%ket_par(j)%M)
			deltaE = level%eigenval(j) - level%Energy0
			
			if ((abs(lande) < 3.d0 .or. (abs(lande) > 1.d10 .and. abs(deltaE) < 1.d0)) .and. &
				level%ket_par(j)%i == level%i_spin .and. level%ket_par(j)%parity == level%parity) then
			
				landeb = lande_caseb(level)
				
				
!				found1 = .FALSE.
!				k = 1
!				do while (found1 == .FALSE. .and. k <= level%hamiltonian_size)				
!					if (level%active(1,k) == level%ket_par(j)%M) then
!						found1 = .TRUE.
!					endif
!					k = k + 1
!				enddo
				
!				found2 = .FALSE.
!				k = 1
!				do while (found2 == .FALSE. .and. k <= level%hamiltonian_size)				
!					if (level%active(2,k) == level%ket_par(j)%M) then
!						found2 = .TRUE.
!					endif
!					k = k + 1
!				enddo
								
!				if (found1) then
!					level%active(2,j) = level%ket_par(j)%M
!				endif
!				if (.not.found1) then
!					level%active(1,j) = level%ket_par(j)%M
!				endif

				if (level%ket_par(j)%parity == 1.d0) then
					str_parity = '+'
				else
					str_parity = '-'
				endif

				level%active(j) = level%ket_par(j)%M
								
!				if (.not.found1) then
									
					write(*,FMT='(A,F5.1,1X,A,F5.1,A1,1X,A,F5.1,1X,A,F12.7,1X,A,F12.7,1X,A,F12.7,1X,A,F12.7,1X,A,F12.7)') 'J=', &
						level%ket_nofield(j)%J, 'i=',level%ket_par(j)%i, str_parity, 'M=',level%ket_par(j)%M, 'E=',level%eigenval(j), &
						'E0=',level%Energy0, 'DE=',deltaE, 'g=', lande, &
						'g(b)=',landeb					
					write(20,*) level%ket_par(j)%m, level%eigenval(j), level%eigenval(j)-level%Energy0, &
						lande, landeb					
!				endif
			endif
		enddo
		

	end subroutine build_hamiltonian

!----------------------------------------------------------------------
!----------------------------------------------------------------------
! Locate a ket
!----------------------------------------------------------------------
!----------------------------------------------------------------------
	function locate_ket(level,Lambda,S,Sigma,Omega,J,M)
	integer :: locate_ket
	type(type_hamiltonian) :: level
	real(kind=8) :: Lambda, S, Sigma, Omega, J, M
	integer :: k
	logical :: found

		locate_ket = -99
		found = .FALSE.
		k = 1
		do while (.not.found .and. k <= level%hamiltonian_size)
			if (level%ket(k)%Lambda == Lambda .and. level%ket(k)%S == S .and. level%ket(k)%Sigma == Sigma .and. &
				 level%ket(k)%Omega == Omega .and. level%ket(k)%J == J .and. level%ket(k)%M == M) then
					found = .TRUE.
					locate_ket = k
			endif
			k = k + 1
		enddo
	end function locate_ket

!----------------------------------------------------------------------
!----------------------------------------------------------------------
! Locate a ket
!----------------------------------------------------------------------
!----------------------------------------------------------------------
	function locate_ket_parity(level,Lambda,S,Omega,J,M,parity)
	integer :: locate_ket_parity
	type(type_hamiltonian) :: level
	real(kind=8) :: Lambda, S, Omega, J, M, parity
	integer :: k
	logical :: found

		locate_ket_parity = -99
		found = .FALSE.
		k = 1
		do while (.not.found .and. k <= level%hamiltonian_size)
			if (level%ket_par(k)%Lambda == Lambda .and. level%ket_par(k)%S == S .and. level%ket_par(k)%parity == parity .and. &
				 level%ket_par(k)%Omega == Omega .and. level%ket_par(k)%J == J .and. level%ket(k)%M == M) then
					found = .TRUE.
					locate_ket_parity = k
			endif
			k = k + 1
		enddo
	end function locate_ket_parity

!----------------------------------------------------------------------
!----------------------------------------------------------------------
! Perform the rotations to a parity-conserving basis set
!----------------------------------------------------------------------
!----------------------------------------------------------------------
	subroutine transform_hamiltonian_to_parity(level)
	type(type_hamiltonian) :: level
	real(kind=8), allocatable :: submatrix(:,:), rotation(:,:)
	real(kind=8) :: J, M, S, Omega, Lambda, Sigma, factor, wrong
	integer :: i,n_m, start, finish, n, nrot, ind1, ind2, ind1_par, ind2_par, a, b, c, nSigma
	
		factor = 1.d0 / sqrt(2.d0)

		do i = 1, level%n_M_blocks
			n = level%M_blocks_sizes(i)
			
			start = level%M_blocks_start(i)
			finish = level%M_blocks_start(i) + n - 1
			
			wrong = -99 - start + 1
!			print *, 'Wrong : ', wrong

			allocate(submatrix(n,n))
			allocate(rotation(n,n))
			rotation = 0.d0

! Build the submatrix (because the Hamiltonian is block-diagonal) and diagonalize it
			submatrix = level%M_Hamiltonian(i)%matrix			
			M = level%ket_par(start)%M

			S = level%S
			Lambda = level%Lambda

			
			nSigma = Lambda + S - abs(Lambda-S)
			do b = 0, nSigma !2*S
!				Omega = abs(Lambda-S) + b
!				Sigma = Omega - Lambda
!				Sigma = -S + b
				Omega = abs(Lambda-S) + b !Lambda + Sigma
				Sigma = Omega - Lambda
				do c = 1, 3
					J = level%J + c - 2
						
					if (J >= 0.d0) then

						ind1 = locate_ket(level,Lambda,S,Sigma,Omega,J,M) - start + 1
!						write(*,FMT='(A2,F4.1,1X,A2,F4.1,1X,A2,F4.1,1X,A2,F4.1,1X,A2,F4.1,1X,A2,F4.1,1X,I4)') 'L=',Lambda, &
!							'S=', S, 'E=',Sigma, 'O=',Omega, 'J=',J, 'M=',M, ind1
						ind2 = locate_ket(level,-Lambda,S,-Sigma,-Omega,J,M) - start + 1
!						write(*,FMT='(A2,F4.1,1X,A2,F4.1,1X,A2,F4.1,1X,A2,F4.1,1X,A2,F4.1,1X,A2,F4.1,1X,I4)') 'L=',-Lambda, &
!							'S=', S, 'E=',-Sigma, 'O=',-Omega, 'J=',J, 'M=',M, ind2

						ind1_par = locate_ket_parity(level,Lambda,S,abs(Omega),J,M,1.d0) - start + 1
!						write(*,FMT='(A2,F4.1,1X,A2,F4.1,1X,A2,F4.1,1X,A2,F4.1,1X,A2,F4.1,1X,A2,F4.1,1X,I4)') 'L=',Lambda, &
!							'S=', S, 'O=',abs(Omega), 'p=', 1.d0, 'J=',J, 'M=',M, ind1_par
						ind2_par = locate_ket_parity(level,Lambda,S,abs(Omega),J,M,-1.d0) - start + 1
!						write(*,FMT='(A2,F4.1,1X,A2,F4.1,1X,A2,F4.1,1X,A2,F4.1,1X,A2,F4.1,1X,A2,F4.1,1X,I4)') 'L=',Lambda, &
!							'S=', S, 'O=',abs(Omega), 'p=', -1.d0, 'J=',J, 'M=',M, ind2_par
						
						if (ind1_par /= wrong .and. ind1 /= wrong) then
							rotation(ind1_par,ind1) = factor
						endif
						if (ind1_par /= wrong .and. ind2 /= wrong) then
							rotation(ind1_par,ind2) = (-1.d0)**(J-S) * factor
						endif
						if (ind2_par /= wrong .and. ind1 /= wrong) then
							rotation(ind2_par,ind1) = factor
						endif
						if (ind2_par /= wrong .and. ind2 /= wrong) then
							rotation(ind2_par,ind2) = -(-1.d0)**(J-S) * factor
						endif
					endif
						
				enddo
					
			enddo
				
!			print *, rotation
!			pause
!			print *, submatrix
!			pause
!			print *, matmul(transpose(rotation),matmul(submatrix,rotation))
!			if (level%Lambda == 0) then
!				pause
!			endif
			level%M_Hamiltonian(i)%matrix = matmul(transpose(rotation),matmul(submatrix,rotation))
			level%Rotation(i)%matrix = rotation
			
			deallocate(submatrix)
			
			deallocate(rotation)
			

		enddo	
	
	end subroutine transform_hamiltonian_to_parity
	
	
!----------------------------------------------------------------------
!----------------------------------------------------------------------
! Calculate the i index
!----------------------------------------------------------------------
!----------------------------------------------------------------------
	subroutine assign_value_i(level)
	type(type_hamiltonian) :: level
	integer :: i, n, k, l
	real(kind=8), allocatable :: energies(:), uniq_energies(:)
	real(kind=8) :: Jota, dif
	
		allocate(uniq_energies(int(2*level%S+1)))
				
		do i = 1, 3
			Jota = level%J + i - 2
			if (Jota >= 0.d0) then
				
				n = count(level%ket_par%J, Jota)				
				
				allocate(energies(n))
				
				l = 1
				do k = 1, level%hamiltonian_size
					if (level%ket_par(k)%J == Jota) then
						energies(l) = level%eigenval_zerofield(k)
						l = l + 1
					endif	
				enddo
								
				call uniq(energies,uniq_energies)
				
				deallocate(energies)
				
				write(*,FMT='(A2,F4.1,A6,I3)'), 'J=',Jota, ' -- n=', n
				write(*,FMT='(A22,3F)') 'Zero-field energies : ',uniq_energies
				
				do k = 1, level%hamiltonian_size
					if (level%ket_par(k)%J == Jota) then
						do l = 1, 2*level%S+1
							dif = abs(uniq_energies(l) - level%eigenval_zerofield(k))
							if (dif < 1.d-9) then
								level%ket_par(k)%i = l
							endif
						enddo
					endif	
				enddo
				
			endif
		enddo
		
		do k = 1, level%hamiltonian_size
			if (level%ket_par(k)%J == level%J .and. level%ket_par(k)%i == level%i_spin) then
				level%Energy0 = level%eigenval_zerofield(k)
			endif
		enddo
		
		print *, 'Selected zero-field energy : ', level%energy0
		
		deallocate(uniq_energies)
		
	end subroutine assign_value_i


!----------------------------------------------------------------------
!----------------------------------------------------------------------
! Re-order the Hamiltonian to make it block-diagonal
!----------------------------------------------------------------------
!----------------------------------------------------------------------
	subroutine order_basis_set(level)
	type(type_hamiltonian) :: level
	real(kind=8), allocatable :: m_vec(:), m_vec2(:)
	real(kind=8) :: M
	integer, allocatable :: indx(:), indx2(:)
	integer :: i, j
	type(type_ket_hund_a), allocatable :: ket_temp(:)
	type(type_ket_hund_a_parity_conserving), allocatable :: ket_temp2(:)

		allocate(m_vec(level%hamiltonian_size))
		allocate(indx(level%hamiltonian_size))
		allocate(m_vec2(level%hamiltonian_size))
		allocate(indx2(level%hamiltonian_size))
		
! Order the basis set by their M value
		do i = 1, level%hamiltonian_size
			m_vec(i) = level%ket(i)%M
			m_vec2(i) = level%ket_par(i)%M
		enddo		
		call indexx(m_vec,indx)
		call indexx(m_vec2,indx2)
				
		allocate(ket_temp(level%hamiltonian_size))
		allocate(ket_temp2(level%hamiltonian_size))
		
		ket_temp = level%ket
		ket_temp2 = level%ket_par

		do i = 1, level%hamiltonian_size
			level%ket(i) = ket_temp(indx(i))
			level%ket_m_ordered(i) = ket_temp(indx(i))
			level%ket_par(i) = ket_temp2(indx2(i))
		enddo

! Put the starting point of each M block
		level%M_blocks_start(1) = 1
		M = minval(m_vec)
		j = 2
		do i = 1, level%hamiltonian_size
			if (level%ket(i)%M /= M) then
				level%M_blocks_start(j) = i
				M = level%ket(i)%M
				j = j + 1
			endif
		enddo

		deallocate(m_vec)
		deallocate(indx)
		deallocate(ket_temp)
		deallocate(m_vec2)
		deallocate(indx2)
		deallocate(ket_temp2)
	end subroutine order_basis_set


!----------------------------------------------------------------------
!----------------------------------------------------------------------
! Diagonalize the different Hamiltonians associated with each value of M
!----------------------------------------------------------------------
!----------------------------------------------------------------------
	subroutine diagonalize_M_hamiltonians(level,eigenval,eigenvec)	
	type(type_hamiltonian) :: level
	real(kind=8) :: eigenval(:), eigenvec(:,:), caca(4,4)
	real(kind=8), allocatable :: submatrix(:,:), subeigenval(:), subeigenvec(:,:)
	integer :: i, j, n_m, start, finish, n, nrot
			

		do i = 1, level%n_M_blocks
			n = level%M_blocks_sizes(i)
			
			start = level%M_blocks_start(i)
			finish = level%M_blocks_start(i) + n - 1

			allocate(submatrix(n,n))
			allocate(subeigenval(n))
			allocate(subeigenvec(n,n))
						
! Build the submatrix (because the Hamiltonian is block-diagonal) and diagonalize it
			submatrix = level%M_Hamiltonian(i)%matrix
!			print *, submatrix
!			pause

			call jacobi(submatrix,subeigenval,subeigenvec,nrot)

!			print *, subeigenval
!			pause
	
			subeigenvec = matmul(subeigenvec,level%Rotation(i)%matrix)
									
!			caca = matmul(transpose(subeigenvec),matmul(level%M_Hamiltonian(i)%matrix,subeigenvec))
!			print *, caca
!			pause
			eigenval(start:finish) = subeigenval
			eigenvec(start:finish,start:finish) = subeigenvec

			deallocate(submatrix)
			deallocate(subeigenval)
			deallocate(subeigenvec)			

		enddo
		
		where (abs(eigenval) < 1.d-10)
			eigenval = 0.d0
		endwhere

	end subroutine diagonalize_M_hamiltonians


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
		N = J - level%S + level%i_spin - 1
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
	integer :: i
		deallocate(level%ket)
		deallocate(level%ket_m_ordered)
		deallocate(level%ket_nofield)
		deallocate(level%ket_purerotation)
		deallocate(level%eigenvec)
		deallocate(level%eigenval)
		deallocate(level%eigenval_zerofield)
		deallocate(level%active)
		do i = 1, level%n_M_blocks
			deallocate(level%M_Hamiltonian(i)%matrix)
			deallocate(level%Rotation(i)%matrix)
		enddo
		deallocate(level%M_Hamiltonian)
		deallocate(level%M_blocks_sizes)
	end subroutine clean_all

end module build_hamiltonian_routines
