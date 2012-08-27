subroutine localerror(local_error,no_eles,u_solution,dofdim,no_quad_points,pvec,meshx,dof_start,pmaxplusone)
implicit none
integer,parameter::dp=selected_real_kind(15)

integer,intent(in)::no_eles,no_quad_points,pmaxplusone,dofdim
real(dp),dimension(no_eles),intent(out)::local_error
real(dp),dimension(dofdim),intent(in)::u_solution
integer,dimension(no_eles),intent(in)::pvec
real(dp),dimension(no_eles+1),intent(in)::meshx
integer,intent(in),dimension(no_eles+1)::dof_start

real(dp),dimension(no_quad_points)::psi,weight
integer::i,counter,qk,j,k
real(dp)::dxdi,xglob,phi,phi_x,phi_x_x,uh,uh_x,uh_x_x,f,phij_x_x,phij_x,phij,h,t1,t2
integer,dimension(pmaxplusone)::global_dof_nos



call newtonsolver(no_quad_points,psi)
call quadrature(weight,no_quad_points,psi)       
do i =1,no_eles
 dxdi = 0.5_dp*(meshx(i+1)-meshx(i))
 h =  meshx(i+1)-meshx(i)
	local_error(i)=0.0_dp
    !uh_x=0.0_dp
	!uh_x_x=0.0_dp
	!cut - building globaldofnos	
  	counter = 1
  	global_dof_nos(counter) = dof_start(i)
  	counter=counter+1
  	global_dof_nos(counter) = dof_start(i+1)
  	counter=counter+1
  	if (pvec(i)>1) then
     	do j =3,pvec(i)+1				
        	global_dof_nos(counter) = dof_start(i)+j-2
       		counter=counter+1     
  		end do
	end if
!cut - building globaldofnos	
	
	do qk=1,no_quad_points
		
  		
	uh=0.0_dp
    uh_x=0.0_dp
    uh_x_x=0.0_dp
  
  		do k=1,pvec(i)+1
			call cal_basis(phi,phi_x,phi_x_x,k,dxdi,psi(qk))
				
				uh = uh+u_solution(global_dof_nos(k))*phi
                
				uh_x = uh_x+u_solution(global_dof_nos(k))*phi_x
                uh_x_x = uh_x_x + u_solution(global_dof_nos(k))*phi_x_x
			!print*,'-uh_x_x + uh=',-uh_x_x	+uh 
		end do
	
		local_error(i) =local_error(i) + ((1.0_dp +0.01_dp*uh_x_x - uh_x)**2)*(h**2/real(pvec(i),dp)**2)*dxdi* weight(qk)
        !local_error(i) =local_error(i) + ((1.0_dp +uh_x_x - uh)**2)*(h**2/real(pvec(i),dp)**2)*dxdi* weight(qk)		
		!local_error(i) =local_error(i) + ((1.0_dp +0.1_dp*uh_x_x - uh_x)**2)*(h**2/real(pvec(i),dp)**2)*dxdi* weight(qk)

	end do
local_error(i)=sqrt(local_error(i))
end do

end subroutine

!sort out F