Program Problem3b
!This program is a solution to problem 3b
!I decided to use functions outside the main program so that it looks nicer

Implicit None

Double Precision x,y,s,act,error,choose
Integer factorial,i,n

 y = 0.20000000000
 x = 0.10000000000

!Create a text file to write the results for plotting in MATLAB
open(unit=1, file='ApproxVsError.dat', status='replace',Action='WRITE')

!The outer loop is used to change the value of the exponent
Do n = 1,10

! Reset 's' so the sum is not carried over from previous iterations
s = 0d15

!Inner loop calculates each term in the series
!and adds it the the running total
    Do i = 0,n
       s = s + (x**(n-i)) * (y**i) *(choose(n,i))

    End Do

!Calculate the result using intrinsic Fortran functions
act = (x+y)**n

!Find the relative
error = abs(act-s)/act

!Write the results into the text file
Write(1,*)n, error

End Do

End Program Problem3b




Function choose(n,k)
!This function calculates n Choose K
! also called the 'binomial distribution'

Implicit None

Integer factorial,n,k
Double Precision choose
If  (k .GT. n )Then
choose = 0
Else If (k .EQ. n) Then
choose = 1
Else
choose  = factorial(n) / (factorial(k)*factorial(n-k))
End If
Return
End Function choose


Function factorial(n)
!This function computes the factorial of an integer N, gives the value of N.

Implicit none

Integer j, n, factorial
factorial = 1

Do j = 1,n
 If (j .LE. 1) Then
  factorial = 1
 Else
  factorial = factorial * j
 End If
End Do
Return
End Function factorial
