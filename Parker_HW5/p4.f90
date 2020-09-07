Program Problem4

Implicit None

Integer, Parameter:: n = 3
Integer, Parameter:: lda = n
Integer, Parameter:: ldvl = n
Integer, Parameter:: ldvr = n
Integer, Parameter:: lwmax = 1000
Integer i, j, info, lwork
Double Precision A(lda,n), VL(ldvl,n), VR(ldvr,n), WR(n), WI(n), WORK(lwmax)

!Matrix A
A(1,:) = [-1,5,-2]
A(2,:) = [4,2,-8]
A(3,:) = [-5,-6,1]
Write(*,3)((A(i,j),j=1,n),i=1,n)

!Query for optimal value of lwork
lwork = -1
call dgeev('v', 'v', n, A, lda, WR,WI,VL,ldvl,VR,ldvr,WORK,lwork,info)
lwork = min( lwmax, Int(WORK(1)) )

!Find the eigenvalues/vectors
call dgeev('v', 'v', n, A, lda, WR,WI,VL,ldvl,VR,ldvr,WORK,lwork,info)

!Check for convergence
If (info.gt.0) Then
Write(*,*)'Failure: Like what you are.   ;)'
Stop
End If


3 FORMAT(3(F5.1))
!     Print eigenvalues.
!Write(*,*)''
Write(*,*)'Eigen Values are'
!Write(*,*)WR
!Write(*,*)''
!Write(*,*)WI
CALL PRINT_EIGENVALUES(N, WR, WI )

!     Print right eigenvectors.
Write(*,*)''
Write(*,*)'Right eigenvectors are'
!Write(*,*)VR(:,1)
write(*,*)
!write(*,*)VR(:,2)
CALL PRINT_EIGENVECTORS(N, WI, VR, LDVR )

Write(*,*)''
Write(*,*)'Left eigenvectors are'
CALL PRINT_EIGENVECTORS(N, WI, VL, LDVL)

4     FORMAT(3(1X,F6.2))
END


SUBROUTINE PRINT_EIGENVALUES( N, WR, WI )
INTEGER  N, J
DOUBLE PRECISION WR( * ), WI( * )

DO J = 1, N
    IF( WI( J ).EQ. 0) THEN
            WRITE(*,9998,ADVANCE='NO') WR( J )
    ELSE
            WRITE(*,9999,ADVANCE='NO') WR( J ), WI( J )
    END IF
END DO
      WRITE(*,*)
9998 FORMAT( 11(:,1X,F6.2) )
9999 FORMAT( 11(:,1X,'(',F6.2,',',F6.2,')') )
END


SUBROUTINE PRINT_EIGENVECTORS(N, WI, V, LDV )
INTEGER N, LDV, I, J
DOUBLE PRECISION WI( * ), V( LDV, * )
      
DO I = 1, N
      J = 1
      DO WHILE( J.LE.N )
            IF( WI( J ).EQ. 0) THEN
               WRITE(*,5,ADVANCE='NO') V( I, J )
               J = J + 1
            ELSE
               WRITE(*,6,ADVANCE='NO') V( I, J ), V( I, J+1 )
               WRITE(*,6,ADVANCE='NO') V( I, J ), -V( I, J+1 )
               J = J + 2
            END IF
      END DO
      WRITE(*,*)
END DO
5 FORMAT( 11(:,1X,F6.2) )
6 FORMAT( 11(:,1X,'(',F6.2,',',F6.2,')') )
END


