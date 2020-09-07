Program Problem1
Implicit None
Integer i
Integer,Parameter:: N=100
Double Precision x,dx, f(0:N-1), der
dx = 0.05
!Build the array of function values
!(*,*)'The values of sin(x) are:'
Do i = 0, N-1
   x = i*dx
   f(i)=sin(x)
   !Write(*,'(f9.3,f9.3)')x,f(i)
End Do
Write(*,*)'  x       six(x)     cos(x)'
!Calculate the derivate and print the values to the screen
Do i = 0, N
	If (i .eq. N) Then
		der = (f(i)-f(i-1)) / dx
		Write(*,'(f9.3,f9.3)')i*dx,der
	Else
		der = (f(i+1) - f(i)) / dx
		Write(*,'(f9.3,f9.3,f9.3)')i*dx,f(i),der
	End If
End Do
x = acos(-1d0)
Write(*,*)x
End Program
