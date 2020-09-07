PROGRAM HW2_Problem1

Implicit None

!Declare the variables
Integer i,j
Integer td
Double Precision tr
Real, Parameter::Earth_Radius = 6371
Double Precision pi
Double Precision, Dimension(1:3)::eps = (/ 0.0, 0.2, 0.7 /)
Double Precision, Dimension(0:12)::r
Double Precision, External::erbit
!Double Precision r

!The value of pi
pi = acos(-1.0)

!Write the data to a file

open(unit=1, file='rad.dat', status='replace',Action='WRITE')

!Loop to calculate the height of the satellite at
!angles from 0 to 360 degrees in 30 degree intervals.
Do i = 1,3

	Do j = 0, 12

		td = j * 30 				!angle in degrees
		tr = td * (pi/180)      	!convert to radians
		!r = Erbit(eps(i),tr)     	!compute height
		r(j)= erbit(eps(i),tr)
		Write(1,'(I5,f12.5)')td,r(j)
	End Do
Write(1,'(A,F4.3)')'When epsilon is: ', eps(i)
Write(1,'(A,F7.3)')'The maximum distance is: ', maxval(r)+Earth_Radius
Write(1,'(A,F7.3)')'The minimum distance is: ', minval(r)+Earth_Radius
End Do



End Program HW2_Problem1
!--------------------------------------------------------
!This function calculates the height of a satellite
!orbiting 15k units above a center of mass.
FUNCTION Erbit(epsilon,theta)

Implicit None

Double Precision, PARAMETER::P = 15000.0000
Double Precision erbit,epsilon,theta

erbit = P / (1 - (epsilon * cos(theta)))

Return

End Function Erbit
