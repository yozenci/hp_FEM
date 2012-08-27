!reworking the smoothness indicator
subroutine smoothparam(no_eles,dof_start,mesh,pvec,dofdim,Ucoef,smoothness)

implicit none
integer,parameter::dp=selected_real_kind(15)
  
real(dp),dimension(no_eles),intent(out)::smoothness

integer,intent(in)::no_eles,dofdim
integer,intent(in),dimension(no_eles)::pvec
integer,intent(in),dimension(no_eles+1)::dof_start
real(dp),intent(in),dimension(no_eles+1)::mesh
real(dp),dimension(dofdim),intent(in)::Ucoef
  
integer::k,counter,pmax,j,i,no_quad_points,qk
integer,dimension(:),allocatable::global_dof_nos  
real(dp)::phi,phi_x,phi_x_x,Uh,Uh_x,dxdi,h1semi,l2norm,coth_1,h
real(dp),dimension(:),allocatable::weights,roots,essup


pmax=maxval(pvec)
allocate(global_dof_nos(pmax+1))
coth_1 = 1.0_dp / tanh(1.0_dp)

  do k=1,no_eles
	no_quad_points = pvec(k)+3
    allocate(roots(no_quad_points),weights(no_quad_points),essup(no_quad_points))
	call newtonsolver(no_quad_points,roots)
	call quadrature(weights,no_quad_points,roots)	
	
	 dxdi = 0.5_dp*(mesh(k+1)-mesh(k))
	 h=(mesh(k+1)-mesh(k))
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
	essup=0.0_dp	
	h1semi=0.0_dp
    l2norm=0.0_dp
	do qk = 1,no_quad_points
			Uh = 0.0_dp
			Uh_x = 0.0_dp
		do i=1,pvec(k)+1
	    	call cal_basis(phi,phi_x,phi_x_x,i,dxdi,roots(qk))
			Uh = Uh + Ucoef(global_dof_nos(i))*phi	
			Uh_x = Uh_x + Ucoef(global_dof_nos(i))*phi_x
		end do
    essup(qk) = abs(Uh)
	l2norm = l2norm +	(Uh**2)*weights(qk)*dxdi	
	h1semi = h1semi +  (Uh_x**2)*weights(qk)*dxdi
	end do
	
deallocate(roots,weights)

	!print*,'coth'
	!print*, maxval(essup)**2* (1.0_dp/(coth_1*( (1.0_dp/h)*l2norm + h*h1semi ))	)
	if (abs(Uh -0.0_dp)<1.0e-10_dp)then
    smoothness(k)=1.0_dp
     
    else
	smoothness(k) = maxval(essup)**2* (1.0_dp/(coth_1*( (1.0_dp/h)*l2norm + h*h1semi ))	)
	end if
	!print*,'Uh = ',Uh
	
deallocate(essup)
  end do  

end subroutine