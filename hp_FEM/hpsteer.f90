subroutine hpsteer(no_eles,refine_vec,smoothness_vec,pvec,no_new_elts)
implicit none
integer,parameter::dp=selected_real_kind(15)

integer,intent(in)::no_eles
integer,intent(out)::no_new_elts
integer,dimension(no_eles),intent(inout)::refine_vec,pvec
real(dp),dimension(no_eles),intent(in)::smoothness_vec

real(dp)::fudge_factor
integer::k

fudge_factor=0.5_dp !play around with this so you get exponential convergence on loglin plot
no_new_elts= 0  

do k =1,no_eles

if (refine_vec(k)==1)then

	if (smoothness_vec(k)>fudge_factor) then
  	pvec(k)=pvec(k)+1
  	refine_vec(k)=0
  	print*,"its smooth"


	else if (smoothness_vec(k)<=fudge_factor) then
	no_new_elts =no_new_elts+1   !note this is the number of mesh refine elements

	end if
		 
end if


end do


end subroutine