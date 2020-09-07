Program Problem1

Implicit None
COMPLEX alpha,beta
INTEGER incx,incy,lda,m,n,j,k
CHARACTER trans
COMPLEX A(4,4),x(4),y(4)
Complex:: i = (0, 1)
trans = 'c'
m=4
n=4
alpha = (1,1)
lda = 4
incx = 1
beta = (0,0)
incy = 1




!Give a matrix with all complex elements
A(1,:) = [(1,2), -1*i, i, i]
A(2,:) = [i, (2,4), (1,1), (1,1)]
A(3,:) = [-1*i, (1,1), (3,1), (0,5)]
A(4,:) = [-1*i, (1,1), (3,2), (4,1)]

!Give a vector with complex elements
x = [alpha,beta,2*alpha,alpha]



!Print the Marices to the screen
Write(*,*)'The Matrix A:'
Do k = 1,m
Write(*,'(4("(", F4.1, " ", SP, F5.1, "i)  "))')(A(k,j),j=1,n)
End Do

Write(*,*)'The vector x: '
Do k = 1,m
Write(*,'(15X,F5.1,SP,F5.1,"i")')(x(k))
End Do

!Call to cgemv to do the operation ---->  y = alpha * A**H * x + beta*y
call cgemv(trans,m,n,alpha,A,lda,x,incx,beta,y,incy)


Write(*,*)'The vector y: '
Do k = 1,m
Write(*,'(15X,F5.1,SP,F5.1,"i")')(y(k))
End Do



end Program

