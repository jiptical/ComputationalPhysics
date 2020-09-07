Program Problem1

Implicit None

integer, Parameter:: s=4
integer m,n, lda,ldb,ldc
Complex ALPHA, BETA
complex:: k = (1.0, 1.0)
Complex:: i = (0.0, 1.0)
Complex A(s,s), B(s,s), C(s,s)
Character*1 SIDE,UPLO
SIDE = 'L'
UPLO = 'U'
ALPHA = (1,0)
BETA = (0,0)
A(1,:) = [(1,0),-1*i,i,i]
A(2,:) = [i,(2,0),(0,0),(0,0)]
A(3,:) = [-1*i,(0,0),(3,0),(0,0)]
A(4,:) = [-1*i,(0,0),(0,0),(4,0)]
B(1,:) = [(2,0),(0,0),-1*i,(0,0)]
B(2,:) = [(0,0),(4,0),i,(1,0)]
B(3,:) = [i,-1*i,(6,0),i]
B(4,:) = [(0,0),(1,0),-1*i,(8,0)]



!Print the Marices to the screen
Do m = 1,s
Write(*,4)(A(m,n),n=1,s)
End Do
Do m = 1,s
Write(*,4)(B(m,n),n=1,s)
End Do

call chemm (SIDE,UPLO,s,s,ALPHA,A,S,B,s,BETA,C,s)

Do m = 1,s
Write(*,4)(C(m,n),n=1,s)
End Do
4 FORMAT(8(1X,F12.1))
end Program

