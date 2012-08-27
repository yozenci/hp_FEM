    subroutine cal_basis(phi,phi_x,phi_x_x,basis_no,dxdi,xi)
!!this subroutine gives the basis function phi
      implicit none
      integer,parameter :: dp = selected_real_kind(15)

      real (dp), intent (out) :: phi, phi_x,phi_x_x
      real (dp), intent (in) :: dxdi, xi
      real(dp)::legendre,legendredash
      integer, intent (in) :: basis_no

      if (basis_no==1) then 
        phi = 0.5_dp*(1.0_dp-xi)
        phi_x = -0.5_dp/dxdi
        phi_x_x = 0.0_dp
        
      else if (basis_no==2) then
        phi = 0.5_dp*(1.0_dp+xi)
        phi_x = 0.5_dp/dxdi
        phi_x_x = 0.0_dp
      else if(basis_no>2) then !this is a lobato shape function, integrated legendre polynomials

		phi = (legendre(basis_no-1,xi) - legendre(basis_no-3,xi)) /(2.0_dp*(real(basis_no,dp)-2.0_dp)+1.0_dp)
        phi_x = legendre(basis_no-2,xi)/dxdi
        phi_x_x = legendredash(basis_no-2,xi)/(dxdi*dxdi) 


	  else
        print *, 'hello world'
        stop
      end if
    end subroutine cal_basis
