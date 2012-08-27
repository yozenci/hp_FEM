!this subroutine is used to sort the refinement vector
!according to the local error
!it works only in the subroutine REFINEMENT_VECTOR.F90
subroutine ref_vec_sort(refinement_vec,no_eles,local_error,elt_label)

implicit none
integer,parameter::dp=selected_real_kind(15)

real(dp),dimension(no_eles),intent(in)::local_error
integer,intent(in)::no_eles
integer,dimension(no_eles),intent(inout)::refinement_vec
integer,dimension(no_eles),intent(in)::elt_label
integer::i,j

		!print*,'local error before being sorted'
		do i=1,no_eles
       ! print*,local_error(i)
		end do

        

	  do i = 1,no_eles-1

		do j= i+1, no_eles
     		if (local_error(i)<local_error(j) ) then
	        call swap(local_error(i),local_error(j))   !sort the local error vector first , big to small (if x<y)
            call integer_swap(elt_label(i),elt_label(j))
            !call integer_swap(refinement_vec(i),refinement_vec(j))
			end if
       end do
       
      end do
		print*,'local error after being sorted'
		do i=1,no_eles
        !print*,local_error(i)
        
      !now we put it in order of elt_labeling with the refinement_vec
		end do
        !print*,'there'
   	  do i = 1,no_eles-1

		do j= i+1, no_eles
     		if (elt_label(i)>elt_label(j)) then 
  			 !sort the local error vector first , small to big (if x>y)
			call integer_swap(elt_label(i),elt_label(j))
            

            call integer_swap(refinement_vec(i),refinement_vec(j))
			end if
       end do
       
      end do   

end subroutine