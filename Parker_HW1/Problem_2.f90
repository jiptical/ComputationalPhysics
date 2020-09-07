program hw1_p1

!this program is the solution to HW1 problem 1
!It takes the input, L, as the length of a simple pendulum
!Then computes the period using the formula: T=2*pi*sqrt(L/g)

implicit none

double precision T,L
double precision, parameter:: pi = 3.14159, g= 9.80665 
!acceleration due to gravity for a body near Earth's surface

!Get L from the user
write(*,*)'What is the length of the simple pendulum in meters?'
read(*,*)L

!Calculate the period
T = 2*pi*SQRT(L/g)

!Write the result to the screen
write(*,*)'The period of the pendulum is:',T,' seconds.'

end
