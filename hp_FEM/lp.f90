! Recursive function to generate the Legendre Polynomials
recursive function legendre(n,x) result(out)

implicit none
integer, parameter :: dp = selected_real_kind(15)

integer, intent(in) :: n
real(dp), intent(in) :: x
real(dp) :: out

if (n==0) then
out = 1.0_dp
else if (n==1)then
out = x
else !Recursive statement
out = ((2.0_dp*(real(n-1,dp))+1.0_dp))/real(n,dp)*x*legendre(n-1,x) - real(n-1,dp)/real(n,dp)*legendre(n-2,x)
endif

end function legendre