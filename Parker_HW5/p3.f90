Program Problem3

Implicit None
Character, Parameter:: jobz = 'V'
Character, Parameter:: uplo = 'U'
Integer, Parameter:: n = 8
Integer, Parameter:: lda = n
Integer, Parameter:: lwmax = 1000
Double Precision:: A(lda,n),W(n),WORK(lwmax)
Integer info, lwork, i, j


!Assign values to A, This particular A is symmetric.
A(1,:) = [4.29 ,5.09 ,3.58 ,0.00 ,7.38 ,0.52 ,2.82 ,7.88]
A(2,:) = [5.09 ,0.26 ,2.73 ,7.03 ,6.78 ,2.44 ,1.03 ,2.42]
A(3,:) = [3.58 ,2.73 ,5.49 ,1.58 ,3.92 ,3.70 ,5.36 ,6.46]
A(4,:) = [0.00 ,7.03 ,1.58 ,7.33 ,5.97 ,0.16 ,3.91 ,7.51]
A(5,:) = [7.38 ,6.78 ,3.92 ,5.97 ,1.77 ,1.93 ,2.13 ,5.02]
A(6,:) = [0.52 ,2.44 ,3.70 ,0.16 ,1.93 ,8.35 ,5.54 ,3.71]
A(7,:) = [2.82 ,1.03 ,5.36 ,3.91 ,2.13 ,5.54 ,3.88 ,0.42]
A(8,:) = [7.88 ,2.42 ,6.46 ,7.51 ,5.02 ,3.71 ,0.42 ,0.95]

Write(*,*)'Matrix A = '
Write(*,4)((A(I,J),J=1,N),I=1,N)

!Query the subroutine for the optimal workspace
lwork = -1
call dsyev( jobz, uplo, n, A, lda, W, WORK, lwork, info)
lwork = min(lwmax, Int(WORK(1)))


!Compute the eigenvalues and  eigenvectors
call dsyev( jobz, uplo, n, A, lda, W, WORK, lwork, info)


!Convergence and illegal argument check
IF (info.GT.0) THEN
	WRITE(*,*)'The algorithm failed to compute eigenvalues.'
	WRITE(*,*)'Off-diagonal elements of an intermediate tridiagonal form did not converge to zero.'
	STOP
ELSE IF (info.lt.0) THEN
	WRITE(*,*)'One or more inputs to  subroutine "DSYEV" had an illegal value.'
	STOP
END IF

!Print the eigenvalues of A
Write(*,*)''
Write(*,*) 'Eigenvalues in the ascending order are: '
Write(*,'(F15.5)')( W(j), j = 1, n )

!Print the eigenvectors of A
Write(*,*)''
WRITE(*,*)'The matrix containing EigenVectors stored columnwise is:'
Write(*,4)((A(i,j),j=1,n),I=1,n)




4     FORMAT(8(1X,F15.5))
end Program



