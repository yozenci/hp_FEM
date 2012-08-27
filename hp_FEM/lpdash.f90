! Recursive function to generate derivatives of the Legendre Polynomials
recursive function legendredash(n,x) result(out)
implicit none
integer, parameter :: dp = selected_real_kind(15)

integer, intent(in) :: n
real(dp), intent(in) :: x
real(dp) :: out

  if (n==0) then
	out = 0.0_dp

  else if (n==1)then
	out = 1.0_dp

  else !Recursive statement
	out = ((2.0_dp*(real(n-1,dp))+1.0_dp))/real(n-1,dp)*x*legendredash(n-1,x) - real(n,dp)/real(n-1,dp)*legendredash(n-2,x)

  endif

  end function legendredash