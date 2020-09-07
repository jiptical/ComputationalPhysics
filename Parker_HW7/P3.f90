!P3.f90
!This program simulates a vibrating membrane by numerically solving the PDE	
Program Membrane
Implicit None
Integer,Parameter:: Nx = 15
Integer,Parameter:: Ny = 15
Integer,Parameter:: Nt = 50
Double Precision, Parameter:: pi = 3.14159265359d0
Double Precision u(Nx,Ny), unew(Nx,Ny), uold(Nx,Ny)
Integer i, j, k
Double Precision dx, dt, rho, Tension, c, cp, a, L, eta, time 
Double Precision x ,y
Open(unit=1, File='P3.dat',status='replace')
!constants and initial values
a = 0.0d0
L = pi
dt = 0.1d0
dx = (L-a)/float(Nx-1)
rho = 390.0d0
Tension = 180.0d0/dx
c = sqrt(Tension/rho)
cp = dx/dt
time = 0.0d0
eta = c*c/(cp*cp)
Write(*,*)eta
!Initial conditions(time=0) + Boundary Conditions
Do i = 1, Nx
	x = a + float(i-1)*dx
	Do j = 1, Ny
		y = a + float(j-1)*dx
		uold(i,j) = sin(2.0d0*x)*sin(y)
		Write(1,*)x,y,uold(i,j)
	End Do
End Do
!Calculate the first time step:
Do i = 2, Nx-1
	x = a + float(i-1)*dx
	Do j = 2, Ny-1
		y = a + float(j-1)*dx
		u(i,j) = uold(i,j) + (eta/2.0d0)*(uold(i+1,j)+uold(i-1,j)+uold(i,j+1)+uold(i,j-1) - 4.0d0*uold(i,j))
	End Do
End Do
!Write the first time step to file
Do i = 1, Nx
    x = a + float(i-1)*dx
    Do j = 1, Ny
        y = a + float(j-1)*dx
        Write(1,*) x, y, u(i,j)
    End Do
End Do
!This loop gets the values of the membrane height (u) at succesive values of time 
Do k = 2, Nt
     Do i = 2, Nx-1
        x = a + float(i-1)*dx
		Do j = 2, Ny-1
			y = a + float(j-1)*dx
			unew(i,j) =  2.0d0*u(i,j) - uold(i,j) + eta*(u(i+1,j)+u(i-1,j)+u(i,j+1)+u(i,j-1)-4.0d0*u(i,j))
		End Do
	End Do       
    !Set u to the new values
	Do i = 2, Nx-1
		Do j = 2, Ny-1
			uold(i,j) = u(i,j)
			u(i,j) = unew(i,j)
		End Do
	End Do
	!Add these values to the file for the time corresponding to k
   	Do i = 1, Nx
		x = a + float(i-1)*dx
		Do j = 1, Ny
			y = a + float(j-1)*dx
			Write (1,*) x, y, u(i,j)
		End Do
	End Do
End Do
End Program Membrane
