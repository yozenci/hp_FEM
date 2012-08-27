subroutine swap(a,b)
implicit none
integer,parameter::dp=selected_real_kind(15)

real(dp),intent(inout)::a,b
real(dp)::temp

temp = a
a=b
b= temp

end subroutine