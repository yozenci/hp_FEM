!This subroutine outputs a system of equations
!AU=F, using the Finite Element Method. U is then
!used to get approximation Uh / Up  / Uhp

subroutine FEM(A,F,dofdim,no_eles,mesh,pvec,pmax,dof_start)

implicit none
integer,parameter::dp=selected_real_kind(15)

integer,intent(in)::dofdim,no_eles,pmax
real(dp),dimension(dofdim,dofdim),intent(out)::A
real(dp),dimension(dofdim),intent(out)::F
real(dp),dimension(no_eles+1),intent(in)::mesh
integer,dimension(no_eles),intent(in)::pvec
integer,dimension(no_eles+1),intent(in)::dof_start

integer::k,no_nodes,no_quad_points,qk,i,j,counter
real(dp)::dxdi,phi_i,phi_x_i,phi_x_x_i,phi_j,phi_x_j,phi_x_x_j
real(dp),dimension(:,:),allocatable::a_loc
real(dp),dimension(:),allocatable::roots,weight,lrhs
integer,dimension(:),allocatable::global_dof_nos



!initialise
no_nodes=no_eles+1
no_quad_points = pmax+3
allocate(roots(no_quad_points),weight(no_quad_points),global_dof_nos(pmax+1),lrhs(pmax+1),a_loc(pmax+1,pmax+1))
A=0.0_dp  
F=0.0_dp 


 
call newtonsolver(no_quad_points,roots)
call quadrature(weight,no_quad_points,roots)

	
do k = 1,no_eles
	dxdi = 0.5_dp*(mesh(k+1)-mesh(k))
    a_loc=0.0_dp

	!local stiffness matrix
	do qk=1,no_quad_points
		do i=1,pvec(k)+1
        	do j=1,pvec(k)+1

            call cal_basis(phi_i,phi_x_i,phi_x_x_i,i,dxdi,roots(qk))
            call cal_basis(phi_j,phi_x_j,phi_x_x_j,j,dxdi,roots(qk))
			
			a_loc(i,j) = a_loc(i,j) + (0.01_dp*phi_x_i*phi_x_j +phi_i*phi_x_j)*weight(qk)*dxdi 
			!a_loc(i,j) = a_loc(i,j) + (phi_x_i*phi_x_j +phi_i*phi_j)*weight(qk)*dxdi
			!a_loc(i,j) = a_loc(i,j) + (0.1_dp*phi_x_i*phi_x_j +phi_i*phi_x_j)*weight(qk)*dxdi             
            end do
        end do

    end do

	!Setting up global_dof_nos vector
	counter = 1
	global_dof_nos(counter) = dof_start(k)
	counter=counter+1
	global_dof_nos(counter) = dof_start(k+1)
	counter=counter+1
        
		if (pvec(k)>1) then
			do j =3,pvec(k)+1				
        		global_dof_nos(counter) = dof_start(k)+j-2
		    	counter=counter+1        
			end do
        end if
	
	!global stiffness matrix

    	do i = 1,pvec(k)+1
  			do j =1,pvec(k)+1        
	   			A(global_dof_nos(i),global_dof_nos(j)) = A(global_dof_nos(i),global_dof_nos(j)) + a_loc(i,j)            
			end do
		end do


	!local right hand side
        lrhs = 0.0_dp
			
		do qk=1,no_quad_points
			do i=1,pvec(k)+1
            
        	    call cal_basis(phi_i,phi_x_i,phi_x_x_i,i,dxdi,roots(qk))
				lrhs(i) = lrhs(i) + (phi_i)*weight(qk)*dxdi
			             
	        end do
  		end do
	
	!global rhs, f
   	 do i = 1,pvec(k)+1
 	
    	F(global_dof_nos(i)) = F(global_dof_nos(i)) + lrhs(i)
	
	 end do


          
end do


!Dirichlet Boundary Conditions, u(0) = u(1) = 0
A(:,1) = 0.0_dp
A(1,:) = 0.0_dp
A(:,dofdim) = 0.0_dp
A(dofdim,:) = 0.0_dp
A(1,1)=1.0_dp
A(dofdim,dofdim)=1.0_dp

deallocate(roots,weight,global_dof_nos,lrhs,a_loc)
end subroutine