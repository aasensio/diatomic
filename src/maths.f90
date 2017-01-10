module maths
use hamiltonian_types
implicit none
contains

!----------------------------------------------------------------------
!----------------------------------------------------------------------
! Jacobi diagonalization of a symmetric real matrix
!----------------------------------------------------------------------
!----------------------------------------------------------------------

	subroutine jacobi(a,d,v,nrot)
		real(kind=8) :: a(:,:), d(:), v(:,:)
		integer :: nrot
	
		integer :: i, ip, iq, j, n
		real(kind=8) :: c, g, h, s, sm, t, tau, theta, thres, b(size(d)), z(size(d))
		
		n = size(d)
		
		v = 0.d0
		do i = 1, n
			v(i,i) = 1.d0
		enddo

		do ip = 1, n
			b(ip) = a(ip,ip)
			d(ip) = b(ip)
			z(ip) = 0.d0
		enddo

		nrot = 0

		do i = 1, 50
			sm = 0.d0
			do ip = 1, n-1
				do iq = ip+1,n
					sm = sm + abs(a(ip,iq))
				enddo
			enddo
			if (sm == 0.d0) return
			if (i < 4) then
				thres = 0.2d0 * sm / n**2
			else
				thres = 0.d0
			endif
			do ip = 1, n-1
				do iq = ip+1,n
					g = 100.d0 * abs(a(ip,iq))
					if ( (i>4) .and. (abs(d(ip))+g == abs(d(ip))) .and. (abs(d(iq))+g == abs(d(iq))) ) then
						a(ip,iq) = 0.d0
					else if (abs(a(ip,iq)) > thres ) then
						h = d(iq) - d(ip)
						if (abs(h)+g == abs(h)) then
							t = a(ip,iq) / h
						else
							theta = 0.5d0 * h / a(ip,iq)
							t = 1.d0 / (abs(theta)+sqrt(1.d0+theta**2))
							if (theta < 0.d0) t = -t
						endif
						c = 1.d0 / sqrt(1.d0+t**2)
						s = t*c
						tau = s / (1.d0+c)
						h = t * a(ip,iq)
						z(ip) = z(ip)-h
						z(iq) = z(iq)+h
						d(ip) = d(ip)-h
						d(iq) = d(iq)+h
						a(ip,iq) = 0.d0
						do j = 1, ip-1
							g = a(j,ip)
							h = a(j,iq)
							a(j,ip) = g - s*(h+g*tau)
							a(j,iq) = h + s*(g-h*tau)
						enddo
						do j = ip+1, iq-1
							g = a(ip,j)
							h = a(j,iq)
							a(ip,j) = g - s*(h+g*tau)
							a(j,iq) = h + s*(g-h*tau)
						enddo
						do j = iq+1, n
							g = a(ip,j)
							h = a(iq,j)
							a(ip,j) = g - s*(h+g*tau)
							a(iq,j) = h + s*(g-h*tau)
						enddo
						do j = 1, n
							g = v(j,ip)
							h = v(j,iq)
							v(j,ip) = g - s*(h+g*tau)
							v(j,iq) = h + s*(g-h*tau)
						enddo
						nrot = nrot+1
					endif
				enddo
			enddo
			do ip = 1, n
				b(ip) = b(ip)+z(ip)
				d(ip) = b(ip)
				z(ip) = 0.d0
			enddo
		enddo

	end subroutine jacobi

!----------------------------------------------------------------------
!----------------------------------------------------------------------
! Wigner 3-j symbol
!----------------------------------------------------------------------
!----------------------------------------------------------------------
	function w3js(j1,j2,j3,m1,m2,m3)
		integer :: m1, m2, m3, j1, j2, j3
		integer :: ia, ib, ic, id, ie, im, ig, ih, z, zmin, zmax, jsum
		real(kind=8) :: w3js, denom, cc, cc1, cc2
		complex(kind=8) :: ccc, cccc

      	w3js = 0.d0
      	if (m1+m2+m3 /= 0) goto 1000
      	ia = j1 + j2
      	if (j3 > ia) goto 1000
      	ib = j1 - j2
      	if (j3 < abs(ib)) goto 1000
      	jsum = j3 + ia
      	ic = j1 - m1
      	id = j2 - m2

      	if (abs(m1) > j1) goto 1000
			if (abs(m2) > j2) goto 1000
			if (abs(m3) > j3) goto 1000
			ie = j3 - j2 + m1
			im = j3 - j1 - m2
			zmin = max0(0,-ie,-im)
			ig = ia - j3
			ih = j2 + m2
			zmax = min0(ig,ih,ic)
			
			ccc = 0.d0
			denom = fact2(zmin/2) + fact2((ig-zmin)/2) + fact2((ic-zmin)/2) + fact2((ih-zmin)/2) + &
					fact2((ie+zmin)/2) + fact2((im+zmin)/2) 
			
			if (mod(zmin,4) /= 0) then
				cccc = -1.d0
				ccc = -denom + log(cccc)
			else
				ccc = -denom
			endif			

			do z = zmin+2, zmax, 2
				denom = fact2(z/2) + fact2((ig-z)/2) + fact2((ic-z)/2) + fact2((ih-z)/2) + &
					fact2((ie+z)/2) + fact2((im+z)/2)

				if (mod(z,4) /= 0) then					
					ccc = ccc + log(1.d0-exp(-denom-ccc))
				else
					ccc = ccc + log(1.d0+exp(-denom-ccc))
				endif
				
			enddo
			cc1 = fact2(ig/2) + fact2((j3+ib)/2) + fact2((j3-ib)/2) - fact2((jsum+2)/2)
      		cc2 = fact2((j1+m1)/2) + fact2(ic/2) + fact2(ih/2) + fact2(id/2) + fact2((j3-m3)/2) + fact2((j3+m3)/2)
			ccc = ccc + 0.5d0 * (cc1 + cc2)			

			if (real(ccc) < -1.d50) then
				ccc = 0.d0
			else
				ccc = exp(ccc)			
			endif

			if (mod(ib-m3,4) /= 0) ccc = -ccc
			w3js = ccc
			if (abs(w3js) < 1.d-8) w3js = 0.d0
1000 		return
      end function w3js

!----------------------------------------------------------------------
!----------------------------------------------------------------------
! Initialize factorials
!----------------------------------------------------------------------
!----------------------------------------------------------------------
	subroutine factrl
		integer :: i
		real(kind=8), parameter :: PI = 3.14159265359d0
		real(kind=8) :: fact_local

		nfac = 301      
		fact_local = 1.d0
		fact2(0) = 0.d0
      	do i=1,nfac
			fact_local = fact_local * i
			if (i < 170) then
				fact2(i) = log(fact_local)
			else
				fact2(i) = 0.5d0 * log(2.d0*PI*i) + i * log(float(i)) - float(i)
			endif
		enddo		
	end subroutine factrl

!----------------------------------------------------------------------
!----------------------------------------------------------------------
! Initialize factorials
!----------------------------------------------------------------------
!----------------------------------------------------------------------
	function delta(a, b)
	real(kind=8) :: a, b, delta
		delta = 0.d0
		if (a == b) delta = 1.d0
	end function delta

!----------------------------------------------------------------------
!----------------------------------------------------------------------
! Reverse of a vector
!----------------------------------------------------------------------
!----------------------------------------------------------------------
	function reverse(a)
	integer :: a(:), reverse(size(a))
	integer :: n, i
		n = size(a)
		do i = 1, n
			reverse(i) = a(n-i+1)
		enddo		
	end function reverse

!----------------------------------------------------------------------
!----------------------------------------------------------------------
! Return the number of elements in a vector equal to a given number
!----------------------------------------------------------------------
!----------------------------------------------------------------------
	function where_index(a,b,n)
	integer :: n
	real(kind=8) :: where_index(n)
	real(kind=8) :: a(:), b
	integer :: i, j, n2
	logical :: found
		n2 = size(a)
		j = 1
		i = 1
		found = .false.
		do while(i <= n2 .and. .not.found)
			if (a(i) == b) then
				where_index(j) = i
				j = j + 1
				if (j > n) then
					found = .true.
				endif				
			endif
			i = i + 1
		enddo
	end function where_index
	
!----------------------------------------------------------------------
!----------------------------------------------------------------------
! Return the number of elements in a vector equal to a given number
!----------------------------------------------------------------------
!----------------------------------------------------------------------
	subroutine uniq(a,b)
	real(kind=8) :: a(:), b(:), dif
	integer, allocatable :: indx(:)
	integer :: i, l, n
		
		n = size(a)
		
		allocate(indx(n))
		
		call indexx(a,indx)
		
		b(1) = a(indx(1))
		l = 2
		do i = 2, n
			dif = abs(a(indx(i)) - b(l-1))
			if (dif > 1.d-9) then
				b(l) = a(indx(i))
				l = l + 1
			endif
		enddo		
		
	end subroutine uniq	
	
!----------------------------------------------------------------------
!----------------------------------------------------------------------
! Return the number of elements in a vector equal to a given number
!----------------------------------------------------------------------
!----------------------------------------------------------------------
	function count(a,b)
	integer :: count
	real(kind=8) :: a(:), b
	integer :: i, l, n
		
		n = size(a)
		
		l = 0		
		do i = 1, n
			if (a(i) == b) then
				l = l + 1
			endif
		enddo
		
		count = l
		
	end function count	

!----------------------------------------------------------------------
!----------------------------------------------------------------------
! Return the time
!----------------------------------------------------------------------
!----------------------------------------------------------------------
	function second()
	real(kind=4) :: second
	integer :: time_array(8)
	real(kind=4) :: tiempo
		call date_and_time(values=time_array)
		tiempo = time_array(5)*3600.*1000.+time_array(6)*60.*1000.+time_array(7)*1000.+time_array(8)
		second = tiempo / 1000.
	end function second

!----------------------------------------------------------------------
!----------------------------------------------------------------------
! Sort an array in ascending order returning the index array in indx
!----------------------------------------------------------------------
!----------------------------------------------------------------------
	subroutine indexx(arr,indx)
	integer :: n,indx(:)
	real(kind=8) :: arr(:)
    integer, parameter :: M=7,NSTACK=50
    integer :: i,indxt,ir,itemp,j,jstack,k,l,istack(NSTACK)
    real(kind=8) :: a

	n = size(arr)
    do j=1,n
		indx(j)=j
	enddo
    jstack=0
    l=1
    ir=n
1   if(ir-l.lt.M)then
		do j=l+1,ir
			indxt=indx(j)
			a=arr(indxt)
			do i=j-1,1,-1
				if(arr(indx(i)).le.a)goto 2
				indx(i+1)=indx(i)
			enddo
			i=0
2			indx(i+1)=indxt
		enddo
        if(jstack.eq.0)return
        ir=istack(jstack)
        l=istack(jstack-1)
        jstack=jstack-2
     else
        k=(l+ir)/2
        itemp=indx(k)
        indx(k)=indx(l+1)
        indx(l+1)=itemp
        if(arr(indx(l+1)).gt.arr(indx(ir)))then
          itemp=indx(l+1)
          indx(l+1)=indx(ir)
          indx(ir)=itemp
        endif
        if(arr(indx(l)).gt.arr(indx(ir)))then
          itemp=indx(l)
          indx(l)=indx(ir)
          indx(ir)=itemp
        endif
        if(arr(indx(l+1)).gt.arr(indx(l)))then
          itemp=indx(l+1)
          indx(l+1)=indx(l)
          indx(l)=itemp
        endif
        i=l+1
        j=ir
        indxt=indx(l)
        a=arr(indxt)
3       continue
          i=i+1
        if(arr(indx(i)).lt.a)goto 3
4       continue
          j=j-1
        if(arr(indx(j)).gt.a)goto 4
        if(j.lt.i)goto 5
        itemp=indx(i)
        indx(i)=indx(j)
        indx(j)=itemp
        goto 3
5       indx(l)=indx(j)
        indx(j)=indxt
        jstack=jstack+2
        if(jstack.gt.NSTACK)pause 'NSTACK too small in indexx'
        if(ir-i+1.ge.j-l)then
          istack(jstack)=ir
          istack(jstack-1)=i
          ir=j-1
        else
          istack(jstack)=j-1
          istack(jstack-1)=l
          l=i
        endif
      endif
      goto 1
      end subroutine indexx
end module maths
