Program Finite_Difference
Implicit none
Integer,Parameter:: N = 100
Double Precision U(N,N), UNew(N,N)
Integer i, j, iter 
Double Precision x,y, h, a, b, c, d, f
Open(unit=1, File='p1.dat')

a = 0.0d0
b = 50.0d0
c = 0.0d0
d = 50.0d0

h = (b-a)/float(N)
Do i = 1, N
   Do j = 1, N
      U(i,j)=0.0d0
   End do   
End Do

Do i = 30, 70
      U(i,30) = -150.0d0
	  U(i,70) = 50.0d0
End Do

Do iter=1, 5000
     Do i = 2, N-1
        Do j = 2,N-1
		If ((j .eq. 30 ).and.(i .ge. 30).and.(i .le. 70)) Then
			UNew(i,j) = U(i,j)
		Else If ((j .eq. 70 ).and.(i .ge. 30).and.(i .le. 70)) Then
			UNew(i,j) = U(i,j)
		Else
			UNew(i,j)=0.25d0*(U(i+1,j)+U(i-1,j)+U(i,j+1)+U(i,j-1))
		End If

        End do     
     End do       
   
     Do i = 1, N
        Do j = 1, N
           U(i,j) = UNew(i,j)
        End do
     End Do
End Do


Do i=1, N
	x = a + float(i-1)*h
	Do j=1, N
        y = c + float(j-1)*h  
        Write (1,*) x, y, U(i,j)
     End Do
End Do

End
