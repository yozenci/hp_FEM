subroutine dofstart(dof_start,dofdim,no_nodes,pvec)
implicit none
integer,parameter::dp=selected_real_kind(15)

integer,intent(in)::no_nodes
integer,dimension(no_nodes-1),intent(in)::pvec
integer,dimension(no_nodes),intent(out)::dof_start
integer,intent(out)::dofdim

integer::i

	!!dof start vector
	dof_start(1)=1

		do i =2,no_nodes
  			dof_start(i)=dof_start(i-1) +pvec(i-1) 
		end do

	dofdim = dof_start(no_nodes)


end subroutine