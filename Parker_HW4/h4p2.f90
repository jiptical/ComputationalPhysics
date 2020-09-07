Program h4p2

Implicit None
Integer, Parameter:: N=5
Integer, Parameter:: M=N+1
Integer I,J,K,KK
Double Precision A(N,M),X(N), SUM

A(1,:) = [1,1,-1,1,-1,2]
A(2,:) = [2,2,1,-1,1,4]
A(3,:) = [3,1,-3,-2,3,8]
A(4,:) = [4,1,-1,4,-5,16]
A(5,:) = [16,-1,1,-1,-1,32]

WRITE(*,*)'The original system is'
WRITE(*,4) ((A(I,J),J=1,M),I=1,N)

Call Gauss_El(A,N,M)

!   START BACKWARD SUBSTITUTION

! xn = a(n,n+1)/a(n,n)
X(N) = A(N,N+1)/A(N,N)
!	For i = n-1,....1 Perform xi = [a(i,n+1)-Sum(aij*xj]/aii
DO K=1,N-1
  	I = (N-1)-K+1
   	SUM = 0.0
   	DO  KK=I+1,N
	  	SUM = SUM-A(I,KK)*X(KK)
   	End Do
	X(I) = (A(I,N+1)+SUM)/A(I,I)
End Do

WRITE(*,6)((A(I,J),J=1,M),I=1,N)
Write(*,*) 'PROCEDURE COMPLETED SUCCESSFULLY'
WRITE(*,7)(X(I),I=1,N)

4     FORMAT(5(1X,ES10.3))
6     FORMAT(1X,'THE REDUCED SYSTEM:',/,(5(1X,ES10.3)))
7     FORMAT(1X,'THE SOLUTION VECTOR IS',/,4(1X,ES10.3))

End Program h4p2

!----------------------------------------------------------------------------------
Subroutine Gauss_El(A,N,M)
Implicit none
Integer N, M, I, J, P, JJ, K, L, XM
Double Precision A(N,M), C
!ELIMINATION PROCESS

DO I=1,N-1
		!  Look for the non-zero element in the column
		P = I
100     IF (ABS(A(P,I)).GE.1.0d-20 .OR. P.GT.N) GOTO 200
			P = P+1
   			GOTO 100
200 	IF(P.EQ.N+1)THEN
			WRITE(*,*)'THE PRECEDING SYSTEM HAS NO UNIQUE SOLUTION'
            STOP
    	END IF
	!	If P is different from I, that is, the diagonal element is zero then
	! 	Exchange row P and I.
   		IF(P.NE.I) THEN
			DO  JJ=1,M
			 	C = A(I,JJ)
			 	A(I,JJ) = A(P,JJ)
              	A(P,JJ) = C
            End Do
   		END IF
   		
		!	For j = i+1, .... N, Perform (Equation(j) - mji*Equation(i)) -> Equation(j)
   
   		DO J=I+1,N
			!	XM = mji = aji/aii
			XM = A(J,I)/A(I,I)
			! Perform (Equation(j) - mji*Equation(i)) -> Equation(j)
			DO K=I+1,M
            	A(J,K) = A(J,K)-XM*A(I,K)
			End DO
			!	Make sure that XM is not saved in A(J,I)
         	A(J,I) = 0
	 	End Do
End Do
!	If a(n,n) is zero, then there is no solution
IF(ABS(A(N,N)).LT.1.0d-20) THEN
   	WRITE(*,*)'THE PRECEDING SYSTEM HAS NO UNIQUE SOLUTION'
	STOP
END IF
END Subroutine Gauss_El
