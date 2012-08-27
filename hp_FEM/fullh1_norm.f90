subroutine fullh1norm(fullh1_norm,l2_norm,h1_norm)

implicit none
integer,parameter::dp=selected_real_kind(15)

real(dp),intent(in)::l2_norm,h1_norm
real(dp),intent(out)::fullh1_norm

fullh1_norm = sqrt(l2_norm**2 + h1_norm**2)

end subroutine