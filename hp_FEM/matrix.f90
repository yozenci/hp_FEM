module matrix
! -------------------------------------------------------------------
! This module defines some general routines used in all questions 
! -------------------------------------------------------------------

  

  

contains

! -------------------------------------------------------------------
  subroutine prtmat(a,n)
! -------------------------------------------------------------------
! This routine displays the matrix A in the terminal window
! -------------------------------------------------------------------
   implicit none
   integer,parameter::dp=selected_real_kind(15)
! Arguments

    integer, intent(in) :: n
    real(dp), dimension(n,n), intent(in) :: a
   
! Local variables
   
    integer :: columns_per_page
    integer :: number_of_pages
    integer :: ipage
    integer :: i

    columns_per_page = 10
    number_of_pages = (n-1)/columns_per_page + 1
    print '()'
    do ipage = 1, number_of_pages
       if ( (ipage-1)*columns_per_page+1 == n ) then
          print '("Column ",I4)',n
       else
          print '("Columns ",I4," to ",I4)',(ipage-1)*columns_per_page+1,min(n,ipage*columns_per_page)
       end if
       print '()'
       do i = 1, n
          print '(10(ES12.4))',a(i,(ipage-1)*columns_per_page+1:min(n,ipage*columns_per_page))
       end do
       print '()'
    end do
    
  end subroutine prtmat

! -------------------------------------------------------------------
  function infnorm(x,n)
! -------------------------------------------------------------------
! This routine computes the infinity norm of the a given vector
! -------------------------------------------------------------------   
! Arguments
	implicit none
   integer,parameter::dp=selected_real_kind(15)
    real(dp) :: infnorm

    integer, intent(in) :: n
    real(dp), dimension(n), intent(in) :: x
    
    infnorm = maxval(abs(x))
   
  end function infnorm
   
! -------------------------------------------------------------------
  subroutine large_pivots(A,n)
! -------------------------------------------------------------------
! This routine computes the matrix in question 6.   
! -------------------------------------------------------------------
implicit none
   integer,parameter::dp=selected_real_kind(15)
!  Arguments

    integer, intent(in) :: n
    real(dp), dimension(n,n), intent(out) :: A
   
! Local variables

    integer :: i

    a = 0.0_dp
    do i = 1, n
       A(i,i) = 1.0_dp
    end do
    do i = 1, n-1
       A(i,n) = 1.0_dp
    end do
    do i = 2, n
       A(i,1:i-1) = -1.0_dp
    end do

  end subroutine large_pivots
  
end module matrix
