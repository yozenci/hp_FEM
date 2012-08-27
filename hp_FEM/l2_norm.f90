!working out the l2 norm of the error u-uh , ||u-uh||_l2(0,1)
subroutine l2norm(l2_norm,mesh,no_eles,dofdim,pvec,dof_start,Ucoef)

implicit none
integer,parameter::dp=selected_real_kind(15)

real(dp),intent(out)::l2_norm
integer,intent(in)::no_eles,dofdim
real(dp),intent(in),dimension(no_eles+1)::mesh
integer,intent(in),dimension(no_eles)::pvec
integer,intent(in),dimension(no_eles+1)::dof_start
real(dp),dimension(dofdim),intent(in)::Ucoef

integer::i,qk,no_quad_points,counter,pmax,j
real(dp)::x_glob,dxdi,u_exact,phij,phij_x,phij_x_x,uh
real(dp),allocatable,dimension(:)::roots,weights,uexactvec
integer,dimension(:),allocatable::global_dof_nos

pmax=maxval(pvec)
allocate(global_dof_nos(pmax+1),uexactvec(no_eles))
l2_norm=0.0_dp

do i=1,no_eles
no_quad_points=pvec(i)+3
allocate(roots(no_quad_points),weights(no_quad_points))


call newtonsolver(no_quad_points,roots)
call quadrature(weights,no_quad_points,roots)



dxdi=(1.0_dp/2.0_dp)*(mesh(i+1)-mesh(i))

!cut	
  counter = 1
  global_dof_nos(counter) = dof_start(i)
  counter=counter+1
  global_dof_nos(counter) = dof_start(i+1)
  counter=counter+1
     do j =3,pvec(i)+1				
        global_dof_nos(counter) = dof_start(i)+j-2
        
    	counter=counter+1
     
  end do

!cut




	do qk=1,no_quad_points
		x_glob = (0.5_dp*(1.0_dp - roots(qk))*mesh(i)) + (0.5_dp*(1.0_dp+roots(qk))*mesh(i+1))
      !	u_exact = (-exp(10.0_dp*x_glob)/(exp(10.0_dp)-1.0_dp))+x_glob+(1.0_dp/(exp(10.0_dp)-1.0_dp))
		u_exact = (-exp(100.0_dp*x_glob)/(exp(100.0_dp)-1.0_dp))+x_glob+(1.0_dp/(exp(100.0_dp)-1.0_dp))
	!	u_exact = (-exp(x_glob) - exp(1.0_dp-x_glob) + 1.0_dp + exp(1.0_dp) ) / (1.0_dp + exp(1.0_dp) ) 
		uh=0.0_dp
if (qk==1)then
uexactvec(i)=u_exact
  end if
        
        do j=1,pvec(i)+1
          call cal_basis(phij,phij_x,phij_x_x,j,dxdi,roots(qk))
          uh=uh+Ucoef(global_dof_nos(j))*phij
			
        end do
	l2_norm = l2_norm + (u_exact-uh)**2 * weights(qk)*dxdi
    end do


deallocate(roots,weights)
end do
l2_norm=sqrt(l2_norm)
open(8,file='results.txt')
write(8,*) l2_norm
deallocate(global_dof_nos,uexactvec)
end subroutine