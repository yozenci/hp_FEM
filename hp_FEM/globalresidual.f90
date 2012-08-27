subroutine global_residual(global_error,no_eles,local_error_vec)

   implicit none
   integer,parameter :: dp = selected_real_kind(15)

    integer,intent(in)::no_eles
    real(dp),dimension(no_eles),intent(in)::local_error_vec
	real(dp),intent(out)::global_error
    
   
	integer::i
    real(dp),dimension(no_eles)::local_errorsq
	
	do i = 1, no_eles
	local_errorsq(i)=local_error_vec(i)*local_error_vec(i)
	
	end do
    global_error=0.0_dp
    do i = 1, no_eles
	global_error = global_error+ local_errorsq(i) 
	
    end do  
    global_error=sqrt(global_error) !constant assumed as 1
end subroutine