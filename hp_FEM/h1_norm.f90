!working out the h1 norm of the error u-uh , ||u-uh||_h1(0,1)
subroutine h1norm(h1_norm,mesh,no_eles,dofdim,pvec,dof_start,Ucoef)

implicit none
integer,parameter::dp=selected_real_kind(15)

real(dp),intent(out)::h1_norm
integer,intent(in)::no_eles,dofdim
real(dp),intent(in),dimension(no_eles+1)::mesh
integer,intent(in),dimension(no_eles)::pvec
integer,intent(in),dimension(no_eles+1)::dof_start
real(dp),dimension(dofdim),intent(in)::Ucoef

integer::i,qk,no_quad_points,counter,pmax,j
real(dp)::x_glob,dxdi,uprime_exact,phij,phij_x,phij_x_x,uprimeh
real(dp),allocatable,dimension(:)::roots,weights
integer,dimension(:),allocatable::global_dof_nos

pmax=maxval(pvec)
allocate(global_dof_nos(pmax+1))
h1_norm=0.0_dp

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

!uprime_exact = -(10.0_dp*exp(10.0_dp*x_glob))/(exp(10.0_dp)-1.0_dp) + 1.0_dp

uprime_exact = -(100.0_dp*exp(100.0_dp*x_glob))/(exp(100.0_dp)-1.0_dp) + 1.0_dp

!uprime_exact = -((-(1.0_dp - exp(1.0_dp)))*(exp(-x_glob)) - &
!((1.0_dp - exp(-1.0_dp))*exp(x_glob)))/ ( exp(-1.0_dp) - exp(1.0_dp) )
        !call cal_basis(phi,phi_x,phi_x_x,qk,dxdi,roots(qk))
		uprimeh=0.0_dp
        do j=1,pvec(i)+1
          call cal_basis(phij,phij_x,phij_x_x,j,dxdi,roots(qk))
          uprimeh=uprimeh+Ucoef(global_dof_nos(j))*phij_x
			
        end do
	h1_norm = h1_norm + (uprime_exact-uprimeh)**2 * weights(qk)*dxdi
    end do


deallocate(roots,weights)
end do
h1_norm=sqrt(h1_norm)
deallocate(global_dof_nos)
end subroutine