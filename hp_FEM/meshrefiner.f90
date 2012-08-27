!this refines the mesh, and the pvec respectively
subroutine mesh_refiner(initial_mesh,no_eles,no_refined_elts,refined_mesh,refinement_vec,pvec,refined_pvec)
implicit none
integer,parameter::dp=selected_real_kind(15)

integer,intent(in)::no_eles,no_refined_elts
real(dp),dimension(no_eles+1),intent(in)::initial_mesh
integer,dimension(no_eles),intent(in)::refinement_vec,pvec

real(dp),dimension(no_eles+1+no_refined_elts),intent(out)::refined_mesh
integer,dimension(no_eles+no_refined_elts),intent(out)::refined_pvec
!local variables
integer::i,counter
real(dp),dimension(no_refined_elts)::new_elts
integer,dimension(no_refined_elts)::new_pvec_elts


refined_mesh=0.0_dp
refined_pvec=0
do i=1,no_eles+1

 refined_mesh(i)=initial_mesh(i)

!here
if (i/=no_eles+1)then 
refined_pvec(i)=pvec(i)

end if

end do
!print*,refined_mesh this seems to be working fine, lets continue



counter=0
  do i=1,no_eles
	if (refinement_vec(i)==1) then
    counter=counter+1
    new_elts(counter)=(initial_mesh(i+1)+initial_mesh(i))/2.0_dp
    new_pvec_elts(counter)=pvec(i)  

	end if
   end do


  do i = 1,no_refined_elts
	refined_mesh(no_eles+1+i)=new_elts(i)
    refined_pvec(no_eles+i)=new_pvec_elts(i)
  end do



call sort(refined_mesh,refined_pvec, no_eles+1+no_refined_elts)

end subroutine