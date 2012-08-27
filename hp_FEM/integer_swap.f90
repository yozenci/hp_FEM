subroutine integer_swap(a,b)
implicit none


integer,intent(inout)::a,b
integer::temp

temp = a
a=b
b= temp

end subroutine