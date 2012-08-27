program hpFEM
use gaussian_elimination
implicit none
integer,parameter::dp=selected_real_kind(15)

integer::no_eles,no_nodes,pmax,dofdim,i,no_refined_elts,tenref,j,b
real(dp),dimension(:),allocatable::mesh,F,U,local_error,Uhvec,local_errorL2,refined_mesh,smoothness
integer,dimension(:),allocatable::pvec,dof_start,refinement_vec,refined_pvec
real(dp),dimension(:,:),allocatable::A,Ufac,Lfac
real(dp)::l2_norm,h1_norm,global_error,effectivity_index,fullh1_norm,global_errorL2
real(dp),dimension(8)::h1vec,errorvec
integer,dimension(8)::dofvec
print*,'how many elements do you want your mesh to have'
read*,no_eles


allocate(mesh(no_eles+1),pvec(no_eles))
do tenref=1,8
  
  !PAUSE
  no_nodes=no_eles+1
allocate(dof_start(no_nodes),local_error(no_eles),refinement_vec(no_eles),smoothness(no_eles))
!Set up mesh
if (tenref==1) then
	mesh(1)=0.0_dp
      do i = 2, no_nodes
        mesh(i) = mesh(i-1) + 1.0_dp/real(no_eles,dp)
      end do
      pvec=2

      
end if
     
      pmax = maxval(pvec)

call dofstart(dof_start,dofdim,no_nodes,pvec)
dofvec(tenref)=dofdim


allocate(A(dofdim,dofdim),F(dofdim),U(dofdim),Ufac(dofdim,dofdim),Lfac(dofdim,dofdim))
A=0.0_dp  
F=0.0_dp

call FEM(A,F,dofdim,no_eles,mesh,pvec,pmax,dof_start)

call solve_ax_eq_b_plu_fact(A,F,dofdim,Lfac,Ufac,U)

!!make boundary of U 0
U(1)=0.0_dp
U(dofdim)=0.0_dp

call l2norm(l2_norm,mesh,no_eles,dofdim,pvec,dof_start,U)
call h1norm(h1_norm,mesh,no_eles,dofdim,pvec,dof_start,U)
call fullh1norm(fullh1_norm,l2_norm,h1_norm)
h1vec(tenref)=fullh1_norm
call localerror(local_error,no_eles,U,dofdim,pmax+3,pvec,mesh,dof_start,pmax+1)
call global_residual(global_error,no_eles,local_error)

print*,'h1 norm ',fullh1_norm,'global error ' ,global_error
errorvec(tenref)=global_error
!Now refine the mesh according the refinement_vector
!write(8,*) 'mesh',tenref
!write(8,*) mesh

call smoothparam(no_eles,dof_start,mesh,pvec,dofdim,U,smoothness)

call refinement_vector (refinement_vec,no_eles,local_error)
call hpsteer(no_eles,refinement_vec,smoothness,pvec,no_refined_elts)

do i=1,no_eles
print*,refinement_vec(i),pvec(i),smoothness(i)
end do



allocate(refined_mesh(no_nodes+no_refined_elts),refined_pvec(no_eles+no_refined_elts))
call mesh_refiner(mesh,no_eles,no_refined_elts,refined_mesh,refinement_vec,pvec,refined_pvec)


deallocate(mesh,pvec,dof_start,local_error,refinement_vec,A,F,U,Ufac,Lfac,smoothness)
allocate(mesh(size(refined_mesh)))
allocate(pvec(size(refined_pvec)))
!print*,size(mesh)-1,'    is the number of elements in the next step'
!print*,size(refined_mesh)-1
no_eles=size(mesh)-1
mesh=refined_mesh
pvec=refined_pvec

if (tenref==8)then
  open(8,file='trimbal.txt')


write(8,*) 'pvec'
write(8,*) pvec

write(8,*)'mesh'
write(8,*) mesh

  end if


deallocate(refined_mesh,refined_pvec)


end do!!!!!!!!!!!!!!!!ends here




deallocate(mesh,F,U,local_error,local_errorL2,pvec,dof_start,A,Ufac,Lfac,Uhvec,smoothness)






!write(8,*)'global error'
!write(8,*) errorvec


print*,dofdim



end program

