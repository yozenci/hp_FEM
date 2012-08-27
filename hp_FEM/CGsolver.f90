 
  subroutine CGMethod(A,b,n,tol,x)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!Subroutine solves Ax=b using Conjugate Gradients!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    
    implicit none
    integer, parameter :: dp = selected_real_kind(15)
    !Direct subroutine variables 
    integer,intent(in)::n
   
    real(dp),intent(in)::tol
    real(dp),intent(in), dimension(n,n)::A   !remember to de-allocate all this
    real(dp), intent(in), dimension(n) :: b
    real(dp), intent(out), dimension(n) :: x
    
    !Implicit subroutine variables
    real(dp)::Alphak,Betak,TwoNormRk
    real(dp),dimension(n):: Rk,Rkone,Pk,APk ! Xk is the intial guess of the solution
    !End of declarations
    
    !!Begin calculations
 
    x = 0.0_dp !Guess, generally n dimensional
    
    Rk = b - matmul(A,x)
    
    Pk = Rk
    TwoNormRk = sqrt(dot_product(Rk,Rk))
    do while (TwoNormRk > tol) 									!when the 2 norm of the kth residual is less than tolerance the loop stops
       
		
	   APk = matmul(A,Pk)										!Expensive calculation so storing it once
       
	   Alphak = dot_product(Rk,Rk) / dot_product(Pk,Apk)		!1--Compute the step length	
       x = x + Alphak*Pk 										!2--Evaluate the approximate solution
       Rkone = Rk - Alphak*APk									!3--Compute the residual
       Betak = dot_product(Rkone,Rkone) / dot_product(Rk,Rk)	!4--Determine the improvement this step
       Pk = Rkone + Betak*Pk					        		!5--Evaluate the new search direction
       
       TwoNormRk = sqrt(dot_product(Rkone,Rkone))				! --generating two norm for tolerance comparison
       
	Rk = Rkone	
    


     
    end do
    
 
    
  end subroutine CGMethod