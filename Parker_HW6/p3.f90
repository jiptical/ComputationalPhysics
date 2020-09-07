Program Problem3
!   This program is a modified version of the example RK4 which implements the RK3 method 
!   to solve a second order differential equation: d^2y/dx^2 - 5*dy/dx + 6*y = sin(3*x)
 
implicit none
integer N, dim, i                                   
Parameter(dim=2, N=10001)		!dim = number of ODEs
double precision x, dx              
double precision fun(dim), y(dim)
EXTERNAL rk3                                                  
EXTERNAl derives 
open(unit=1, file='p3.dat', status='replace')                             
Write(1,20)'|     x    |','|   dy/dx   |'

dx = 0.01d0
x = -dx                                     
y(1) = 0.0d0 !dy/dx
y(2) = 1.0d0 !y(x)

do i = 1, N                                     
     x = x + dx
     call derives(x,y,fun)
     call rk3(y,fun,dim,x,dx,y,derives)
	 write(1,10)x,y(1)
end do  

10 FORMAT((f15.8),(1x,e15.8))        
20 Format(3(a15))
end Program
!-------------------------------------------------------------–------------------------------------------------------------
!The following subroutine has the right hand side of all equations
SUBROUTINE derives(x,y,fun)
implicit none
integer dim, i
parameter(dim=2)
double precision x, y(dim), fun(dim)


fun(1) = y(2)
fun(2) = sin(3.0d0*x) - 6.0d0*y(1) + 5.0d0*y(2)

end Subroutine
!--------------------------------------------------------------------------------------------------------------------------
SUBROUTINE rk3(y,fun,dim,x,dx,yout,derivs) 
INTEGER dim, i
double precision dx,x,fun(dim),y(dim),yout(dim), k1(dim), k2(dim), k3(dim), yt(dim), dyt(dim)  
EXTERNAL derivs 

do i = 1, dim     
	k1(i) = dx * fun(i) 				                    ! k1 = h*f(tn,yn)
  	yt(i) = y(i) + k1(i)/2.0d0 			                    ! Compute y(n) + k1/2 and store it in yt
end do  

call derivs(x+(dx/2.0d0),yt,dyt)    			            ! Get f(tn+h/2,yn+k1/2) and store in dyt.
do i = 1, dim
	k2(i) = dx * dyt(i)				                        ! k2 = h *  f(tn+h/2,yn+k1/2)	
  	yt(i)= y(i) - k1(i) + (2.00d0)*k2(i)			            ! Compute y(n) + k2/2 and store it in yt
end do  

call derivs(x+dx,yt,dyt)     			                    ! Get f(tn+h/2,yn+k2/2) and store in dyt.
do i = 1, dim			
	k3(i) = dx * dyt(i)	
    yt(i) = y(i) + k3(i) 			                        ! k3 = h *  f(tn+h/2,yn+k2/2)
	yout(i) = y(i) + (( k1(i) + 4.0d0*k2(i) + k3(i) ) / 6.0d0)
end do  

END Subroutine

