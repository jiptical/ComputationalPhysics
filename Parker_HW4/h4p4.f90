Program Inv_Gauss_El_Piot
!  	This program uses Gaussian Elimination with partial Pivoting to
!	find the inverse of a Matrix A
Implicit none
Integer, Parameter:: N=3
Integer, Parameter:: M=2*N
Integer I, J, P, K, L, KK,NROW(N), N2, N1, I1
Double Precision A(N,N), B(N,N), Comb(N,2*M), X(N,N), C(N,N),D(N,N), XM, SUM

! Declare the Matices given in problem 4
A(1,:) = [-11,-4,9]
A(2,:) = [1,-8,1]
A(3,:) = [5,7,5]

C(1,:) = [33,4,-9]
C(2,:) = [-9,18,81]
C(3,:) = [12,-17,-15]


WRITE(*,*)'Matrix A is = '

! We must create a NxN Identity Matrix to carry out the neccessary operations
B = 0.0d0
Do I = 1, N
	B(I,I) = 1.0d0
End Do
Comb(1:N,1:N) = A
Comb(1:N,N+1:M) = B

!WRITE(*,*)'Matrix Comb, which is a combination of A and B is = '
!WRITE(*,5) ((Comb(I,J),J=1,M),I=1,N)

Call GEP(Comb,N,M,NROW)

!WRITE(*,*)'The reduced system is'
!WRITE(*,5) ((Comb(I,J),J=1,M),I=1,N)


Do I = 1, N
	Do J = 1, N
		A(I,J) = Comb(I,J)
		B(I,J) = Comb(I,J+N)
	End Do
End Do
!
!WRITE(*,*)'Matrix A after applying GEP is'
!WRITE(*,4) ((A(I,J),J=1,N),I=1,N)
!
!WRITE(*,*)'Matrix B after applying GEP is'
!WRITE(*,4) ((B(I,J),J=1,N),I=1,N)


!Backward substitution to get X(N,N)
Do I = 1, N
	X(N,I) = B(NROW(N),I)/A(NROW(N),N)
	Do J = N-1, 1, -1
		X(J,I) = B(NROW(J),I)
		Do K = J+1, N
			X(J,I) = X(J,I) - A(NROW(J),K) * X(K,I)
		End Do
		X(J,I) = X(J,I)/A(NROW(J),J)
	End Do
End Do

Write(*,*) 'PROCEDURE COMPLETED SUCCESSFULLY. The inverse of Matrix A is'
WRITE(*,4)((X(I,J),J=1,N),I=1,N)
D = matmul(X,C)
Write(*,*)((D(I,J),J=1,N),I=1,N)


4     FORMAT(2(1X,ES15.8))
5     FORMAT(4(1X,ES15.8))
END Program Inv_Gauss_El_Piot

!---------------------------------------------------------------
Subroutine GEP(A,N,M,NROW)
Implicit none
Integer N, M, I, J, P, K, L, KK,NROW(N), IMAX, JP, I1, J1, NCOPY, N2, N1
Double Precision A(N,M), X(N), C, XM, SUM, AMAX

!	INITIALIZE ROW POINTER
DO I=1,N
	NROW(I)=I
End DO
!	Elimination process begins 
DO I=1,N-1
!   Look for the largest element in column I
   	IMAX=NROW(I)
   	AMAX=ABS(A(IMAX,I))
   	!IMAX=I
   	DO 	P=I+1,N
		JP=NROW(P)
		IF(ABS(A(JP,I)).GT.AMAX) THEN
			 AMAX=ABS(A(JP,I))
			 IMAX=P
		END IF
	End Do
!  If the largest element is zero then stop
   IF(AMAX.LT.1.0d-20) THEN
		WRITE(*,*)'THE PRECEDING SYSTEM HAS NO UNIQUE SOLUTION'
		Stop
   END IF
!  If the row with index Imax is difference than the row with indes I
!	interchange the rows	
   IF(NROW(I).NE.NROW(IMAX)) THEN
		NCOPY=NROW(I)
		NROW(I)=NROW(IMAX)
		NROW(IMAX)=NCOPY
   END IF
	!	For j = i+1, .... N, Perform (Equation(j) - mji*Equation(i)) -> Equation(j)      
   I1=NROW(I)
   DO J=I+1,N
		J1=NROW(J)
		XM=A(J1,I)/A(I1,I)         !Set XM = m(NROW(j),i) = a(NROW(j),i)/a(NROW(i),i) 
		! Perform (Equation(NROW(j)) - m(NROW(j),i)*Equation(NROW(i))) -> Equation(NROW(j))  
		DO K=I+1,M
	      	A(J1,K)=A(J1,K)-XM*A(I1,K)
		End Do
!               Make sure that XM is not SAVED IN A(J1,I)
        A(J1,I)=0.0
	End Do
End Do
!	If a(n,n) is zero, then there is no solution
N1=NROW(N)
IF(ABS(A(N1,N)).LT.1.0E-20) THEN
   WRITE(*,*)'THE PRECEDING SYSTEM HAS NO UNIQUE SOLUTION'
   Stop
END IF
END Subroutine GEP
