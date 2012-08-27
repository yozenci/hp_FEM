subroutine solve_ax_eq_b_plu_fact(A,b,n,L,U,x)
! ----------------------------------------------------------------
!  This subroutine computes the PLU factorisation of A and 
!  evaluates x using forward and backward substitution
! ----------------------------------------------------------------
implicit none
integer,parameter :: dp = selected_real_kind(15)
! Arguments

    integer, intent(in) :: n
    real(dp), dimension(n,n), intent(in) :: A
    real(dp), dimension(n,n), intent(out) :: L,U
    real(dp), dimension(n), intent(out) :: x
    real(dp), dimension(n), intent(in) :: b

! Local variables

    real(dp), dimension(:), allocatable :: pivrow, y, Pb
    real(dp) :: tmp
    integer :: i, j
    
    allocate(pivrow(n),y(n),Pb(n))

    x = 0.0_dp
    Pb = b
    L = 0.0_dp
    U = A

    do i = 1, n-1

       j = maxloc(abs(U(i:n,i)),1) + i - 1

! If j /= i, then swap rows.

       if ( j /= i ) then

! Swap rows of U.

          pivrow(i:n) = U(i,i:n)
          U(i,i:n) = U(j,i:n)
          U(j,i:n) = pivrow(i:n)

! Swap rows of b and store in Pb.

          tmp = Pb(i)
          Pb(i) = Pb(j)
          Pb(j) = tmp

! If i > 1 swap rows of L too.

          if ( i > 1 ) then
             pivrow(1:i-1) = L(i,1:i-1)
             L(i,1:i-1) = L(j,1:i-1)
             L(j,1:i-1) = pivrow(1:i-1)
          end if
       end if
       L(i,i) = 1.0_dp
       L(i+1:n,i) = U(i+1:n,i)/U(i,i)
       do j = i+1, n
          U(j,i:n) = U(j,i:n) - L(j,i)*U(i,i:n)
       end do
    end do
    L(n,n) = 1.0_dp
   
! Solve L*y = Pb

    y(1) = Pb(1)
    do i = 2, n
       y(i) = Pb(i) - dot_product(L(i,1:i-1),y(1:i-1))
    end do
   
! Solve U*x = y
   
    x(n) = y(n)/U(n,n)
    do i = n-1, 1, -1
       x(i) = ( y(i) - dot_product(U(i,i+1:n),x(i+1:n)) )/U(i,i)
    end do
    
    deallocate( pivrow, y, Pb )
    
  end subroutine solve_ax_eq_b_plu_fact