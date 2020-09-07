Program h4p1

!This program uses Secant method to find the root of a function f(x)
Implicit None
Integer i, N
Double Precision a, b, f,Df, Tol, PN, PN_1,PN_2, h

a = 0.0d0
b = 1.0d0
Tol = 1.0d-10
N = 100
h = (b-a)/1000.0d0
PN_2 = (a+b)/2.0d0
PN_1 = PN_2 + h

Do i = 1,  N

   Df = (f(PN_1)-f(PN_2))/(PN_1 - PN_2)	!numerical derivative using backward difference method
   PN = PN_1 - f(PN_1)/Df           !compute PN
   IF((Abs(f(PN)) .LE. 1.0d-20) .OR. (Abs(PN-PN_1) .LT. TOL)) THEN  !see if f(p)~0, or the two points are too close
      Write(*,*)'Procedure was successful ', 'P = ',PN, 'i = ',i
      Exit
   Else
      PN_2 = PN_1
      PN_1 = PN
      h = PN_1 - PN_2
   End If
End Do
If(i == N)Then
   Write(*,*)'Procedure was unsuccessful'
End if
End Program h4p1
!----------------------------------------------------------------------------------
Function f(x)
Implicit None
Double Precision k,rho,alpha,e, V,f
Double Precision x
k = 8.99d9
alpha = 1090.0
e = 1.6022d-19
rho = 0.330d-10
f = -k * (e / x) + (alpha * exp(-x/rho))
Return
End Function f

