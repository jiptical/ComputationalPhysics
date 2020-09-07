Program h4p3
!  	GAUSSIAN ELIMINATION WITH Partial pioting, uses BACKWARD SUBSTITUTION ALGORITHM 
!  	TO SOLVE THE N BY M LINEAR SYSTEM
!  	INPUT:   NUMBER OF UNKNOWNS AND EQUATIONS N; 
!  	AUGMENTED MATRIX A = (A(I,J)) WHERE 1<=I<=N AND 1<=J<=N+1.
!  	OUTPUT:  SOLUTION X(1),X(2),...,X(N) OR A MESSAGE THAT THE LINEAR
!   SYSTEM HAS NO UNIQUE SOLUTION.
Implicit none
Integer, Parameter:: N=3
Integer, Parameter:: M=N+1
Integer I, J, P, K, L, KK,NROW(N), N2, N1
Double Precision A(N,M), X(N), C, XM, SUM

A(1,:) = [3.03,-12.1,14.0,-119.0]
A(2,:) = [-3.03,12.1,-7.0,120.0]
A(3,:) = [6.11,-14.2,21.0,-139.0]

WRITE(*,*)'The original system is'
WRITE(*,4) ((A(I,J),J=1,M),I=1,N)

Call GEP(A,N,M,NROW)
!     START BACKWARD SUBSTITUTION
N1=NROW(N)
! xn = a(Nrow(n),n+1)/a(NROW(n),n)
X(N)=A(N1,N+1)/A(N1,N)
!	For i = n-1,....1 Perform xi = [a(NROW(i),n+1)-Sum(a(NROW(i),j)*xj]/a(NROW(i),i)
DO K=1,N-1
	I=(N-1)-K+1
	N2=NROW(I)
	SUM=0.0
	DO KK=I+1,N
	   SUM=SUM-A(N2,KK)*X(KK)
	End Do
	X(I)=(A(N2,N+1)+SUM)/A(N2,I)
End Do
WRITE(*,*)'The reduced system is'
WRITE(*,4) ((A(I,J),J=1,M),I=1,N)
Write(*,*) 'PROCEDURE COMPLETED SUCCESSFULLY'
WRITE(*,7)(X(I),I=1,N)
4     FORMAT(5(1X,ES18.7))
7     FORMAT(1X,'THE SOLUTION VECTOR IS',/,4(1X,ES18.7))
END Program h4p3

!--------------------------------------------------------------------------------------
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
   	IMAX=NROW(I)    !start with ith row.
   	AMAX=ABS(A(IMAX,I))    !A(IMAX,I) gives diagonal elements of A
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
		NCOPY=NROW(I)             !copy the ith row number temporarily to NCOPY
		NROW(I)=NROW(IMAX)		 !Notice that we are only using row numbers, not the rows	
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
