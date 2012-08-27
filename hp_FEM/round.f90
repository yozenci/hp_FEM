subroutine round(x,intx)
implicit none
integer,parameter::dp=selected_real_kind(15)

real(dp),intent(in)::x
integer,intent(out)::intx

if(x - int(x) >= 0.5_dp) then
  !!round it up
  intx=int(x+1.0_dp)
end if
if (x - int(x) < 0.5_dp)then
	intx=int(x)
	
end if

end subroutine