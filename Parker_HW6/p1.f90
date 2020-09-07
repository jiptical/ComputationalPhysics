Program Problem1

Implicit None
Integer i
Integer, Parameter:: N = 251
Double Precision a,b
Double Precision, Parameter:: dt = 0.04 !step size
Double Precision theta(N), omega(N),t(N)
External d2theta, d1theta
open(unit=1, file='p1.dat', status='replace')
!time interval
a = 0.00
b = 10.00

Do i = 1,N
    If (i == 1) Then
        !initial conditions
        theta(i) = 0.001d0
        omega(i) = 0.01d0
        t(i) = a
        Write(1,10)t(i),theta(i)
    Else
        t(i) = t(i-1) + dt    
        call euler(d2theta,d1theta,theta(i-1),omega(i-1),dt,theta(i),omega(i))
        Write(1,10)t(i),theta(i)
    End If
End Do

10 FORMAT(3(f10.5))

End Program
!-------------------------------------------------------
!function definitions
Function d2theta(theta, t)
Implicit None
Double precision, Parameter:: g=9.8
Double Precision, Parameter:: l=1.0
Double precision, Intent(in):: theta
Double Precision t,d2theta
    d2theta= -(g/l) * theta
return
End Function

Function d1theta(theta, t)
Implicit None
Double Precision omega, theta, t, d1theta
    d1theta = omega 
return
End Function
!------------------------------------------------------

Subroutine euler(d2theta,d1theta,theta_i,omega_i,dt,theta,omega)
Implicit None
Double Precision d2theta,d1theta,theta_i,omega_i,dt
Double Precision theta,omega
External d1theta, d2theta
    omega = omega_i + d2theta(theta_i,omega_i)*(dt)
    theta = theta_i + omega_i*(dt)
    return
End Subroutine