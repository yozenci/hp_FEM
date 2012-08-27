!this subroutine is called to determine which elements to refine
!the top 20 percent are refined (in terms of biggest residual
!and the vector of refinement is output from here
subroutine refinement_vector (refinement_vec,no_eles,local_error)

implicit none
integer,parameter::dp=selected_real_kind(15)


integer,intent(in)::no_eles
real(dp),dimension(no_eles),intent(in)::local_error
integer,dimension(no_eles),intent(out)::refinement_vec

integer::i,no_refined_elts
integer,dimension(no_eles)::elt_label
refinement_vec=0

	call round(real(no_eles,dp)*0.2_dp,no_refined_elts)!top 20 percent , outputs number of elts

	do i =1,no_refined_elts
		refinement_vec(i)=1
	end do  

	do i =1,no_eles
		elt_label(i)=i
    end do
	
call ref_vec_sort(refinement_vec,no_eles,local_error,elt_label)



    
end subroutine

