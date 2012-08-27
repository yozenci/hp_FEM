!outputs gauss-quadrature weights for numerical integration
subroutine quadrature (weights,no_quad_points,roots)

implicit none
integer, parameter :: dp = selected_real_kind(15)

integer,intent(in)::no_quad_points
real(dp),intent(in),dimension(no_quad_points)::roots
real(dp),dimension(no_quad_points),intent(out)::weights


integer::i
real(dp)::legendredash

do i = 1, no_quad_points

weights(i) = (2.0_dp) /((1.0_dp - roots(i)*roots(i))*((legendredash(no_quad_points,roots(i)))&
*(legendredash(no_quad_points,roots(i)))))

end do

end subroutine
