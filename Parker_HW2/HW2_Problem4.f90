Program HW2_Problem4
!This program reads data from two separate files, and calls a subroutine
!to calculate the least-squares fit line of the form y = (m*x) + b
Implicit None

!Declare the variables
Integer i,n
Double Precision, Dimension(1:20):: x,y		!We need 2 arrays for x and y data

!Open the data files
open(unit=1, file = 'xlsf.dat', status='old', action='Read')
open(unit=2, file = 'ylsf.dat', status='old', action='Read')

!By manually counting the number of data points
n = 20

!Read the x,y data into separate arrays
Read(1,*)(x(i),i=1,n)
Read(2,*)(y(i),i=1,n)

!call to subroutine
call eqnofline(x,y,n)
End Program HW2_Problem4
!---------------------------------------------
Subroutine EqnOfLine(x,y,n)
Implicit None
Integer, Intent(in):: n
Double Precision x_ave,y_ave,m,b
Double Precision, Intent(in):: x(n),y(n)

!This condition is neccessary to achieve the correct values.
!If (count(y) .NEQ. count(x)) Then
!Write(*,*)'The input arrays have different numbers of elements, check that the
!correct data has been given'
!Exit
!End If

!Get the average of each dataset
y_ave = sum(y) / n
x_ave = sum(x) / n

!Calculate m. I used the dot_product(x,y) intrinsic function to simplify.
m = ( dot_product(x,y) - (sum(x) * y_ave) ) / ( dot_product(x,x) - (sum(x)*x_ave) )
b = y_ave - (m * x_ave)

!print the results to the screen
Write(*,*)'The slope is      :',m
Write(*,*)'The y-intercept is:',b

End Subroutine EqnOfLine
