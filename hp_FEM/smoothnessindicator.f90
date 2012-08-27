!thomas wihlers smoothness indicator
!this uses the h1 seminorm and l2 norm of the 
subroutine smoothness_indicator(Ucoeffs,no_eles,mesh,dof_start,pvec,dofdim,siFk)

implicit none
integer,parameter::dp=selected_real_kind(15)

integer,intent(in)::no_eles,dofdim
real(dp),intent(in),dimension(no_eles+1)::mesh
real(dp),intent(in),dimension(dofdim)::Ucoeffs
integer,intent(in),dimension(no_eles+1)::dof_start
integer,dimension(no_eles),intent(in)::pvec
real(dp),dimension(no_eles),intent(out)::siFk


integer::j,k,counter,pmax,no_quad_points,qk
real(dp)::h,dxdi,coth_1,phij,phij_x,phij_x_x,l2_norm,uh,uh_x,h1_seminorm,infnorm
integer,dimension(:),allocatable::global_dof_nos
real(dp),allocatable,dimension(:)::roots,weights,esssupu
  
coth_1=1.0_dp /  tanh(1.0_dp)

pmax =maxval(pvec)  
allocate(global_dof_nos(pmax+1))
do k=1,no_eles
  l2_norm=0.0_dp
  h1_seminorm=0.0_dp
  
no_quad_points=pvec(k)+3
allocate(roots(no_quad_points),weights(no_quad_points),esssupu(no_quad_points))

esssupu=0.0_dp
  
call newtonsolver(no_quad_points,roots)
call quadrature(weights,no_quad_points,roots)

  dxdi = (1.0_dp/2.0_dp)*(mesh(k+1)-mesh(k))
h = mesh(k+1)-mesh(k)



!cut	
  counter = 1
  global_dof_nos(counter) = dof_start(k)
  counter=counter+1
  global_dof_nos(counter) = dof_start(k+1)
  counter=counter+1
     do j =3,pvec(k)+1				
        global_dof_nos(counter) = dof_start(k)+j-2
        
    	counter=counter+1
     
  end do

!cut
		do qk=1,no_quad_points
		uh=0.0_dp
		uh_x=0.0_dp
           do j=1,pvec(k)+1
             
          call cal_basis(phij,phij_x,phij_x_x,j,dxdi,roots(qk))
          uh=uh+Ucoeffs(global_dof_nos(j))*phij
		  uh_x=uh_x+Ucoeffs(global_dof_nos(j))*phij_x
        end do
       esssupu(qk) =uh* weights(qk)
	l2_norm = l2_norm + (uh)**2 * weights(qk)*dxdi
    h1_seminorm =  h1_seminorm + (uh_x)**2 * weights(qk)*dxdi 
    
    end do

infnorm = maxval(esssupu)**2
deallocate(roots,weights,esssupu)


print*,'smoothness= ',  infnorm*(1.0_dp /(coth_1*((1.0_dp/h)*l2_norm  + h*h1_seminorm )))

siFk(k)=infnorm*(1.0_dp /(coth_1*((1.0_dp/h)*l2_norm  + h*h1_seminorm )))



end do


print*,'smoothness= '
print*,siFk




end subroutine