Program Matrix_mult



IMPLICIT NONE
INTEGER, PARAMETER :: n = 2
REAL, DIMENSION(n,n) :: A, B, C
INTEGER :: i, j, k
A = reshape((/2.0,19.0,3.0,-3.0/),(/2,2/))
B = reshape((/13.0,10.0,-4.0,3.0/),(/2,2/))

!WRITE(*,*)A(1,1),A(1,2)
!WRITE(*,*)A(2,1),A(2,2)
!WRITE(*,*)B(1,1),B(1,2)
!WRITE(*,*)B(2,1),B(2,2)


DO i = 1, n
	DO j = 1, n
		DO k = 1, n
		   C(i,j) = C(i,j) + A(i,k) * B(k,j)
		END DO
	END DO
END DO

DO i = 1,n
WRITE(*,*)(C(i,j),j=1,n)
END DO


END PROGRAM Matrix_mult