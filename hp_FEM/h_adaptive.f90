!!this is no longer for the problem -u''+u=1
!!now it is for -episilonu'' + u =1
program h_adaptive
implicit none
integer,parameter::dp=selected_real_kind(15)

real(dp),dimension(:),allocatable::mesh,F,U,local_error,refined_mesh
real(dp),dimension(:,:),allocatable::A,Ufac,Lfac
real(dp)::l2_norm,global_error,h1_norm
integer,dimension(:),allocatable::pvec,dof_start,refinement_vec
integer::no_nodes,no_eles,i,dofdim,pmax,no_refined_elts,tenref
real(dp),dimension(20)::h1vec,gevec
integer,dimension(20)::nodvec

print*,'how many elements do you want your mesh to have'
read*,no_eles

allocate(mesh(no_eles+1))

no_nodes=no_eles+1
allocate(pvec(no_eles),dof_start(no_nodes),local_error(no_eles),refinement_vec(no_eles))

pvec=1
pmax=maxval(pvec)

mesh(1)=0.0_dp

do i=2,no_nodes
	mesh(i)=mesh(i-1)+1.0_dp/real(no_eles,dp)
end do

!Start the dof_start vector
call dofstart(dof_start,dofdim,no_nodes,pvec)

allocate(A(dofdim,dofdim),F(dofdim),U(dofdim),Lfac(dofdim),Ufac(dofdim))
A=0.0_dp
F=0.0_dp
call FEM(A,F,dofdim,no_eles,mesh,pvec,pmax,dof_start)
!call CGMethod(A,F,dofdim,1.0e-15_dp,U)

call solve_ax_eq_b_lu_fact(A,F,dofdim,Lfac,Ufac,U)

U(1)=0.0_dp
U(dofdim)=0.0_dp
call l2norm(l2_norm,mesh,no_eles,dofdim,pvec,dof_start,U)
call h1norm(h1_norm,mesh,no_eles,dofdim,pvec,dof_start,U)
h1vec(tenref)=h1_norm
call localerror(local_error,no_eles,U,dofdim,pmax+3,pvec,mesh,dof_start,pmax+1)
call global_residual(global_error,no_eles,local_error)
gevec(tenref)=global_error
!Now refine the mesh according the refinement_vector
call refinement_vector (refinement_vec,no_eles,no_refined_elts,local_error)
allocate(refined_mesh(no_nodes+no_refined_elts))
call mesh_refiner(mesh,no_eles,no_refined_elts,refined_mesh,refinement_vec)

deallocate(mesh,pvec,dof_start,local_error,refinement_vec,A,F,U,Ufac,Lfac)
allocate(mesh(size(refined_mesh)))

print*,size(mesh)-1,'    is the number of elements in the next step'
print*,size(refined_mesh)-1
no_eles=size(mesh)-1
mesh=refined_mesh

deallocate(refined_mesh)




open(8,file='results.txt')

write(8,*) 'h1 vector'
write(8,*) h1vec

write(8,*) 'gevec'
write(8,*) gevec

write(8,*)'nodvec'
write(8,*)nodvec



end program