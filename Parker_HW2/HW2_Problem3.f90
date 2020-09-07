Program Homework2_Problem3
Implicit None

Double Precision m1,m2,r
Double Precision gForce

m1 = 1.5d3
m2 = 5.972d24
r = 4.5d4
Write(*,'(Es12.5E2)')gForce(m1,m2,r)

End Program

!-------------------------------------
Function gForce(mass1,mass2,distance)
Implicit None
!Declare variables
Double Precision, Parameter::G = 6.67408d-11
Double Precision mass1,mass2,distance
Double Precision gForce
!write(*,'(Es12.5E2)')G
!Calculation of force
gForce = G*mass1*mass2 / (distance**2)
Return
end Function gForce
