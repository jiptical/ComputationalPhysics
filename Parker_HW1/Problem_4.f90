Program ProblemFour
!This program computes the harmonic mean and rms of a given dataset.

Implicit None
Real rms, hm
Integer i
Integer, Parameter::n=5
Real, Dimension(n)::x = (/ 82, 17, 112, 19, 131 /) !The given dataset

rms = 0.0
hm = 0.0
! The control to exit the program on the last element of x is built in to the do loop
! The if statement checks for negative values and exits the program before the rest
! of the calculations.
Do  i = 1, n
 If ( x(i) .LT. 0 ) Then
    EXIT
 Else
 rms = rms + ( x(i) )**2 / n
 hm = hm + (1.0 / x(i))
 End If
End Do

!final calculations
rms = sqrt(rms)
hm = n / hm

Write(*,*)'The rms value for this dataset is: ',rms
Write(*,*)'The harmonic mean is: ',hm


End Program ProblemFour