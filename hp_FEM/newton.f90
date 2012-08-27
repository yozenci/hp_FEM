!newton iterator
subroutine newtonsolver(n,roots)
implicit none
integer,parameter::dp=selected_real_kind(15)

integer,intent(in)::n
real(dp),dimension(n),intent(out)::roots

integer::i
real(dp)::legendre,legendredash,error,tol,pi,x0,x1

pi=4.0_dp*atan(1.0_dp)
tol=1.0e-13_dp

do i=1,n
error=1.0_dp
x0 = real(-1.0_dp*cos((((2.0_dp*real(i,dp))-1.0_dp) /(2.0_dp*real(n,dp)))*pi),dp)
	do while(error>tol)
	x1 = x0 - legendre(n,x0)/legendredash(n,x0)
	error = abs(x1-x0)    
	x0=x1
  	end do

roots(i)=x1 !maybe it should be x0 depending on where the loop exits, does it complete a full loop or is it always testing the tolerance?
end do

end subroutine