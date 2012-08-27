subroutine sort(list,refined_pvec,n)
implicit none
integer,parameter::dp=selected_real_kind(15)
real(dp),dimension(n),intent(inout)::list
integer,dimension(n-1),intent(inout)::refined_pvec
integer::i,j,n
  do i = 1,n-1

	do j= i+1, n 
     if (list(i)>list(j) ) then
       call swap(list(i),list(j))
      if(j/=1)then
		call integer_swap(refined_pvec(i-1),refined_pvec(j-1))
      end if
end if
       end do


    end do

end subroutine