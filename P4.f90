Program Problem4

Implicit None
Integer N
Double Precision a, pi, est, act, err, err2, simpsonf,simpsong, foo1, foo2

a = 0
pi = acos(-1.0d0)
N = 3
err = 10d0
act = -2.0d0*pi

open(unit=4, file='p4.dat', status='replace', action='write')

Write(4,*)"----------N---------est--------------error"

Do While (err >= 1d-5)
	est = simpsonf(N,a,pi)
	err = abs(est-act)
	N = N+2
End Do
Write(4,*)N,est,err

!set the actual value to match the second funtion
act = pi**2 - 4
N = 3
err2 = 10d0
Write(4,*)"The actual value is:",act
Do While (err2 >= 1d-5)
	est = simpsong(N,a,pi)
	err2 = abs(est-act)
	N = N+2
End Do
Write(4,*)N,est,act,err
End Program
!---------------------------------------------------
Function f(x)
Implicit None
Double Precision f,x
f = (x**2)*(cos(x))
Return
End Function f

Function g(x)
Implicit None
Double Precision g,x
g = (x**2)*sin(x)
Return
End Function g


!Simpson's Rule Method
Function simpsonf(N,a,b)
Implicit None
Double Precision f,x, dx, a, b, simpsonf
Integer i,N

simpsonf = 0.0d0
dx = ((b-a) / float(N-1))
Do i=2, (N-1), 2
x = a + dx * float(i-1)
simpsonf = simpsonf + 4.0d0 * f(x)
End Do
! loop for odd points
Do i=3, (N-1), 2
x = a + dx * float(i-1)
simpsonf = simpsonf + 2.0d0 * f(x)
End Do
! add the endpoints
simpsonf = simpsonf + f(a) + f(b)
simpsonf = simpsonf * dx/3.0d0
Return
End Function simpsonf

Function simpsong(N,a,b)
Implicit None
Double Precision g,x, dx, a, b, simpsong
Integer i,N

simpsong = 0.0d0
dx = ((b-a) / float(N-1))
Do i=2, (N-1), 2
x = a + dx * float(i-1)
simpsong = simpsong + 4.0d0 * g(x)
End Do
! loop for odd points
Do i=3, (N-1), 2
x = a + dx * float(i-1)
simpsong = simpsong + 2.0d0 * g(x)
End Do
! add the endpoints
simpsong = simpsong + g(a) + g(b)
simpsong = simpsong * dx/3.0d0
Return
End Function simpsong
