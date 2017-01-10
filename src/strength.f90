module strength
use hamiltonian_types
use maths
use build_hamiltonian_routines
implicit none
contains

!------------------------------------------------
! Locates a given value in a vector
!------------------------------------------------
	function locate(a,b)
	integer :: locate
	real(kind=8), INTENT(IN) :: a(:), b
	integer :: location(1)
		locate = -1

		location = minloc(abs(a-b))
		if (location(1) /= 0) then
			locate = location(1)
		endif
		
	end function locate

!------------------------------------------------
! Returns the kronecker's delta
!------------------------------------------------
	function kronecker(a,b)
	real*8 :: kronecker
	real*8, INTENT(IN) :: a, b
		kronecker = 0.d0
		if (a == b) then
			kronecker = 1.d0
		endif
	end function kronecker

! ------------------------------------------------------------
! This function returns the matrix elements of the dipole moment in the Hund's a)
! eigenfunctions
! ------------------------------------------------------------	
	function q_o_m(Lambda_up,Omega_up,J_up,M_up,Lambda_low,Omega_low,J_low,M_low)
	real*8 :: q_o_m
	real*8, INTENT(IN) :: Lambda_up, Omega_up, J_up, M_up, Lambda_low, Omega_low, J_low, M_low
	real*8 :: q_m, q_o
	
		q_m = qm(J_up,M_up,J_low,M_low)
		q_o = qo(Lambda_up,Omega_up,J_up,Lambda_low,Omega_low,J_low)

		q_o_m = q_o * q_m
		
	end function q_o_m

! ------------------------------------------------------------
! This function returns the matrix elements of q_o
! ------------------------------------------------------------	
	function qm(J_up,M_up,J_low,M_low)
	real*8 :: qm
	real*8, INTENT(IN) :: J_up, M_up, J_low, M_low
	real*8 :: deltam, deltaj, J, M
	
		deltaj = J_up - J_low
		deltam = M_up - M_low
		
		J = J_low
		M = M_low
		qm = 0.d0
		
		if (deltam == 0.d0) then
			if (deltaj == 1.d0) then
				qm = dsqrt((J+1.d0)**2 - M**2)
			else if (deltaj == 0) then
				qm = M
			else if (deltaj == -1.d0) then
				qm = dsqrt(J**2 - M**2)
			endif
		endif
		
		if (dabs(deltam) == 1.d0) then
			if (deltaj == 1.d0) then
				qm = -deltam * dsqrt(0.5d0*(J+1.d0+M*deltam)*(J+2.d0+M*deltam))
			else if (deltaj == 0) then
				qm = dsqrt(0.5d0*(J-M*deltam)*(J+1.d0+M*deltam))
			else if (deltaj == -1.d0) then
				qm = deltam * dsqrt(0.5d0*(J-M*deltam)*(J-1.d0-M*deltam))
			endif
		endif
				
	end function qm

! ------------------------------------------------------------
! This function returns the matrix elements of q_o
! ------------------------------------------------------------	
	function qo(Lambda_up,Omega_up,J_up,Lambda_low,Omega_low,J_low)
	real*8 :: qo
	real*8, INTENT(IN) :: Lambda_up, Omega_up, J_up, Lambda_low, Omega_low, J_low
	real*8 :: deltao, deltaj, Omega, J, pi_eps
	
		deltao = Omega_up - Omega_low
		deltaj = J_up - J_low
		pi_eps = 1.d0 + kronecker(1.d0,Lambda_up+Lambda_low)
		
		J = J_low
		Omega = Omega_low
		qo = 0.d0
		
		if (deltao == 0.d0) then
			if (deltaj == 1.d0) then
				qo = 1.d0/(J+1.d0) * dsqrt(( (J+1.d0)**2 - Omega**2)/( (2.d0*J+1.d0)*(2.d0*J+3.d0)) )
			else if (deltaj == 0) then
				qo = Omega / (J*(J+1.d0))
			else if (deltaj == -1.d0) then
				qo = 1.d0/J * dsqrt(( J**2 - Omega**2)/((2.d0*J-1.d0)*(2.d0*J+1.d0)))
			endif
		endif
		
		if (dabs(deltao) == 1.d0) then
			if (deltaj == 1.d0) then
				qo = -deltao/(J+1.d0) * dsqrt(pi_eps*(J+1.d0+Omega*deltao)*(J+2.d0+Omega*deltao) /&
					(2.d0*(2.d0*J+1.d0)*(2.d0*J+3.d0)))
			else if (deltaj == 0) then
				qo = 1.d0/(J*(J+1.d0)) * dsqrt(0.5d0*pi_eps*(J-Omega*deltao)*(J+1.d0+Omega*deltao))
			else if (deltaj == -1.d0) then
				qo = deltao/J * dsqrt(pi_eps*(J-Omega*deltao)*(J-1.d0-Omega*deltao) /&
					(2.d0*(2.d0*J-1.d0)*(2.d0*J+1.d0)))
			endif
		endif
				
	end function qo

!----------------------------------------------------------------------
!----------------------------------------------------------------------
! Strength of the Zeeman components in the Hund's case (a) coupling
!----------------------------------------------------------------------
!----------------------------------------------------------------------
	function case_a_strength(Ju,Jl,Mu,Ml,Ou,Ol,Lu,Ll)
	real(kind=8) :: case_a_strength
	real(kind=8) :: Mu, Ml, Ju, Jl, Ou, Ol, qm, ql, Lu, Ll

		qm = Mu - Ml
		ql = Ol - Ou !Ll - Lu

! Falta un factor sqrt((2Ju+1)*(2Jl+1)) ??

!		case_a_strength = w3js(int(2.d0*Jl),int(2.d0*Ju),2,int(2.d0*Ml),-int(2.d0*Mu),int(2.d0*qm)) * &
!			w3js(int(2.d0*Jl),int(2.d0*Ju),2,int(2.d0*Ol),-int(2.d0*Ou),int(2.d0*qo)) * &
!			sqrt((2.d0*Ju+1.d0)*(2.d0*Jl+1.d0))
		case_a_strength = w3js(int(2.d0*Ju),int(2.d0*Jl),2,int(2.d0*Mu),-int(2.d0*Ml),-int(2.d0*qm)) * &
			w3js(int(2.d0*Ju),int(2.d0*Jl),2,int(2.d0*Ou),-int(2.d0*Ol),int(2.d0*ql)) * sqrt((2.d0*Jl+1.d0)*(2.d0*Ju+1.d0))

	end function case_a_strength

!----------------------------------------------------------------------
!----------------------------------------------------------------------
! Strength of the Zeeman components in the Hund's case (a) coupling
!----------------------------------------------------------------------
!----------------------------------------------------------------------
	function direction_cosine_matrix(Ju,Jl,Mu,Ml,Ou,Ol,Su,Sl,Lu,Ll)
	real(kind=8) :: direction_cosine_matrix
	real(kind=8) :: Ju,Jl,Mu,Ml,Ou,Ol,Su,Sl,Lu,Ll
		
		direction_cosine_matrix = kronecker(Su,Sl) * q_o_m(Lu,Ou,Ju,Mu,Ll,Ol,Jl,Ml)
		
!		print *, direction_cosine_matrix
!		direction_cosine_matrix = kronecker(Su,Sl) * case_a_strength(Ju,Jl,Mu,Ml,Ou,Ol,Lu,Ll)
!		print*, direction_cosine_matrix
!		pause
		if (IsNan(direction_cosine_matrix)) then
			direction_cosine_matrix = 0.d0
		endif

	end function direction_cosine_matrix

!----------------------------------------------------------------------
!----------------------------------------------------------------------
! Strength of the Zeeman components in the Hund's case (a) coupling
!----------------------------------------------------------------------
!----------------------------------------------------------------------
	function transition_strength(trans,up,low)
	type(type_transition) :: trans
	real(kind=8) :: transition_strength, str
	real(kind=8) :: Ju, Jl, Mu, Ml, Ou, Ol, Su, Sl, Lu, Ll
	integer :: l1, l2, up, low
		
! The Hamiltonian matrix is written with the basis ordered by M value. Therefore, we have to calculate
! the direction cosine in this very same basis set in order to get correct results

		str = 0.d0
		
		do l1 = 1, trans%upper%hamiltonian_size
			do l2 = 1, trans%lower%hamiltonian_size
				
				Ju = trans%upper%ket_m_ordered(l1)%J
				Jl = trans%lower%ket_m_ordered(l2)%J
				Mu = trans%upper%ket_m_ordered(l1)%M
				Ml = trans%lower%ket_m_ordered(l2)%M
				Ou = trans%upper%ket_m_ordered(l1)%Omega
				Ol = trans%lower%ket_m_ordered(l2)%Omega
				Su = trans%upper%ket_m_ordered(l1)%Sigma
				Sl = trans%lower%ket_m_ordered(l2)%Sigma
				Lu = trans%upper%Lambda
				Ll = trans%lower%Lambda

				str = str + trans%upper%eigenvec(l1,up) * &
					direction_cosine_matrix(Ju,Jl,Mu,Ml,Ou,Ol,Su,Sl,Lu,Ll) * trans%lower%eigenvec(l2,low)
					
			enddo
		enddo

		transition_strength = str

	end function transition_strength

!----------------------------------------------------------------------
!----------------------------------------------------------------------
! Count the number of Zeeman components and generate the index arrays
!----------------------------------------------------------------------
!----------------------------------------------------------------------
	subroutine count_zeeman_components(trans)
	type(type_transition) :: trans
	integer :: i, j, i2, j2, k, nJup, nJlow, ind_up, ind_low, ind_up_str, ind_low_str
	real(kind=8) :: Ju, Jl, Mu, Ml, Ou, Ol, Su, Sl, Lu, Ll, case_a_str, spli_up, spli_low, spli, m_up, m_low, str
	real(kind=8), allocatable :: direction_cosine(:,:), strength_matrix(:,:), caca(:,:)
	integer :: loc(2), l1, l2
		
		nJup = int(2.d0*trans%upper%J+1.d0)
		nJlow = int(2.d0*trans%lower%J+1.d0)

		k = 0
		do i = 1, trans%upper%hamiltonian_size
			do j = 1, trans%lower%hamiltonian_size
				if ( abs(trans%upper%ket(i)%M-trans%lower%ket(j)%M) <= 1.d0 .and. &
					trans%upper%active(i) /= -9999 .and. trans%lower%active(j) /= -9999) then
						k = k + 1
				endif
			enddo
		enddo
		print *, 'Number of Zeeman transitions : ', k

		open(unit=12,file='pattern.dat',action='write',status='replace')
		write(12,*) k

		k = 0
		do i = 0, 2*trans%upper%J
			m_up = -trans%upper%J + i
			do j = 0, 2*trans%lower%J
				m_low = -trans%lower%J + j

				if (dabs(m_up-m_low) <= 1.d0) then

					ind_up = locate(trans%upper%active(:),m_up)
					ind_low = locate(trans%lower%active(:),m_low)

					ind_up_str = ind_up !trans%upper%original_order(ind_up)
					ind_low_str = ind_low !trans%lower%original_order(ind_low)

! This has to be done because I am having problems with the parity of the functions
! This problems appears because my eigenfunctions still do not have a definite parity
! I have to take this into account. It is necessary to calculate the Hamiltonian and then perform combinations
! of the matrix elements according to the well-defined parity eigenfunctions of Hund's case (a)
! For the moment, this trick seems to work
					str = transition_strength(trans,ind_up_str,ind_low_str)

!					if (str**2 == 0.d0) then
!						ind_up_str = locate(trans%upper%active(2,:),m_up) !trans%upper%original_order(locate(trans%upper%active(2,:),m_up))
!						ind_low_str = locate(trans%lower%active(1,:),m_low) !trans%lower%original_order(locate(trans%lower%active(1,:),m_low))
!						str = transition_strength(trans,ind_up_str,ind_low_str)
!					endif
					
					spli_up = trans%upper%eigenval(ind_up)-trans%upper%Energy0
					spli_low = trans%lower%eigenval(ind_low)-trans%lower%Energy0
					spli = -(spli_up-spli_low)*(trans%wavelength*1.d-8)**2 * 1.d8

					write(*,FMT='(F5.1,1X,F5.1,1X,F13.8,3X,F13.8,3X,E13.7)') trans%upper%ket(ind_up)%M, &
							trans%lower%ket(ind_low)%M, spli, str**2
					write(12,FMT='(F5.1,1X,F5.1,1X,F13.8,3X,F13.8,3X,E13.7)') trans%upper%ket(ind_up)%M, &
							trans%lower%ket(ind_low)%M, spli, str**2
			
				endif
			enddo
		enddo
		close(12)
	
	end subroutine count_zeeman_components

!----------------------------------------------------------------------
!----------------------------------------------------------------------
! Strength of the Zeeman components
!----------------------------------------------------------------------
!----------------------------------------------------------------------
	subroutine zeeman_strength(trans)
	type(type_transition) :: trans

! First count the number of components of the Zeeman transition
		call count_zeeman_components(trans)
	
	end subroutine zeeman_strength

end module strength
