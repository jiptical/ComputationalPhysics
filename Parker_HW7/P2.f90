!   P2.f90
!   Problem 2:Heat equation: Two identical aluminum bars 0.25 m long are placed in contact 
!	along one of their ends with their other ends kept at 0C. One is kept in heat bath at 100oC, 
!	and the other at 50oC. The sides of the bars are insulated. 
!	Solve heat equation for the two bars. 
!	C = 0.113 cal/C.g, K = 0.12 cal.C.g.s, and rho = 7.8 g/cm3 for aluminum.
!	Plot temperature as a function of distance along the rods and time (surface plot).



Program ProblemTwo
Implicit None
Integer, Parameter:: Nx = 100
Integer, Parameter:: Nt = 1000
Double Precision T(Nx), Tnew(Nx)
Integer i,j
Double Precision x, time, dx, dt, a, b, tf
Double Precision T0, T1, T2, K, Cp, rho, S
Open(unit=1, File='p2.dat', status='replace')

a = 0.0d0
b = 50.0d0
tf = 100
dx = (b-a)/float(Nx)
dt = tf/float(Nt)
T0 = 0.0d0
T1 = 50.0d0
T2 = 100.0d0
K = 0.12d0
Cp = 0.113d0
rho = 7.8d0
S = K/(Cp*rho)

!Initialize the mesh
Do i = 1,Nx
	T(i) = 0.0d0
End Do 
!Assign Initial conditions
Do i = 2,Nx-1
	If (i .le. 50)Then
		T(i) = T1
	Else If (i .gt. 50)Then
		T(i) = T2
	End If
End Do

Do j = 1,Nt
	time = j*dt
	Do i = 2, Nx-1
        x = a + float(i)*dt
		TNew(i) = T(i) + S*(dt/(dx*dx))*(T(i+1)+T(i-1)-2*T(i))
	End Do
	Do i = 2, Nx-1
		T(i) = Tnew(i)
	End Do
	If (mod(j,10) .eq. 0)Then
	Do i = 1,Nx
		Write(1,*)time,(a+float(i-1)*dx),T(i)
	End Do
	End If
End Do       
End Program
