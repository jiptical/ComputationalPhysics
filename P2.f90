Program Problem2
Implicit None
Integer i,N
Double Precision trapez,der,a,b,act,err
a = -4.0d0
b = 4.0d0
act = 0.999936658
open(unit=1, file='trapez.dat', status='replace', action='write')
Do i = 1,91,2
N = (10+i)
der = trapez(N,a,b)
err = abs(der-act)
Write(1,*)N,err
End Do

End Program
!---------------------------------------------------------
!Compute the value of the given function
Function f(x)
Implicit None
Double Precision f,x,pi
pi = acos(-1.0d0)
f = (1/sqrt(2*pi))*exp((-x**2)/2)
Return
End Function f

!---------------------------------------------------------
!Function to carry out the trapezoidal rule calculation
Function trapez(N,a,b)
Implicit None
Integer i,N
Double Precision f, dx, a, b, trapez, x
trapez = 0.0d0
dx = (b-a) / float(N-1)
!sum over the midpoints
Do i = 2, N-1
	x = a + dx * float(i-1)
	trapez = trapez +f(x) * dx
End Do
!sum the end points
trapez = trapez + 0.5d0*(f(a)+f(b))*dx
return
End Function trapez

!---------------------------------------------------------
