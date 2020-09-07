Program Problem2


Implicit None
Integer i
Integer, Parameter:: N = 251
Double Precision, Parameter:: dt = 0.04 !step size
Double Precision theta(N), omega(N), t(N)
External d2theta, d1theta, rk4
open(unit=1, file='p2.dat', status='replace')


Do i = 1,N
    If (i == 1) Then
        !initial conditions
        theta(i) = 0.001d0
        omega(i) = 0.01d0
        t(i) = 0.00
        Write(1,10)t(i),theta(i)
    Else
        t(i) = t(i-1) + dt    
        call rk4(d1theta,d2theta,t(i-1),theta(i-1),omega(i-1),dt,theta(i),omega(i))
        Write(1,10)t(i),theta(i)
    End If
End Do 
10 FORMAT(2(f15.5))

End Program 
!-----------------------------------------------------
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
    
    Function d1theta(omega, t)
    Implicit None
    Double Precision theta, t, d1theta,omega
        d1theta = omega 
    return
    End Function
!------------------------------------------------------
!Runge Kutta 
Subroutine rk4(d1theta,d2theta,t,theta_i,omega_i,dt,theta,omega)
    Implicit None
    Integer i
    Double Precision, Intent(in):: theta_i,omega_i,t,dt
    Double Precision k1theta,k1omega,k2theta,k2omega,k3theta,k3omega,k4theta,k4omega,d1theta,d2theta
    Double Precision, Intent(out):: theta,omega
    External d1theta, d2theta
    k1theta = dt * d1theta(omega_i)
    k1omega = dt * d2theta(theta_i)
    k2theta = dt * d1theta(omega_i+k1omega/2.0) 
    k2omega = dt * d2theta(theta_i+k1theta/2.0) 
    k3theta = dt * d1theta(omega_i+k2omega/2.0) 
    k3omega = dt * d2theta(theta_i+k2theta/2.0) 
    k4theta = dt * d1theta(omega_i+k3omega)
    k4omega = dt * d2theta(theta_i+k3theta)
    theta = theta_i + (k1theta + 2.0*(k2theta+k3theta) + k4theta)/6.0 
    omega = omega_i + (k1omega + 2.0*(k2omega+k3omega) + k4omega)/6.0
return
end Subroutine
    

