!This is Shooting method to solve a boundary value problem. The method is basically a
!combination of one algorithm for solving initial value problems and one algorithm for
!root finding problem. I have used RK4 and Secant method here but you can use other methods.
implicit none
integer N, M, dim, i,j                                   
Parameter(dim=1, N=3651, M=100)
double precision h, Tol, v
double precision fun(dim), Y1(dim), y(dim,N),YR, YL,x,  XL, XR, x0, P0, P1, P2, D, F0, F1
EXTERNAL Derives
open(unit=1, file='p4.dat',status='replace')
10 Format((f9.2),(f15.3))
20 Format(2(A12))
30 Format((A),(I1),(A),(es15.8))

v = 2.0d5
YL = 1.0d6
YR = 1.6d6
XL = 0.0d0
XR = 1.0d0
Tol = 1.0d-6

h = (XR - XL)/float(N-1)
y(1,1) = 1.0d6
P0 = 0.1
P1 = P0 + h
Do j = 1, M
   do i = 1, N-1                                     
      x = XL + h*float(i-1)
      Y1(1) = y(1,i)
     
      call Derives(x,Y1,fun,P0)
      call rk4(Y1,fun,dim,x,h,Y1,Derives,P0)
      y(1,i+1) = Y1(1)
      
   end do
   F0 = y(1,N) - YR
   
   do i = 1, N-1
      x = XL + h*float(i-1)
      Y1(1) = y(1,i)
      
      call Derives(x,Y1,fun,P1)
      call rk4(Y1,fun,dim,x,h,Y1,Derives,P1)
      y(1,i+1) = Y1(1)
      
   end do
   F1 = y(1,N) - YR

   D = F1 - F0

   if(ABS(D) .LT. Tol)then
      write(*,30)'It took ',j,' iterations to get lambda = ',P0
      Write(1,20)'Time(days)','Population'
    do i = 1, N
         write(1,10)XL+h*float(i-1)*365,y(1,i)
      end do
      Stop
   else
      P2 = P1 - F1 * (P1 - P0)/D
      P0 = P1
      P1 = P2
   End if   
end do
end


SUBROUTINE Derives(t,y,fun,P0)
implicit none
integer dim, i
parameter(dim=1)
double precision t, y(dim), fun(dim), v, P0
v = 2.0d5
fun(1) = P0*Y(1) + v
end


SUBROUTINE rk4(y,fun,dim,t,h,yout,Derivs,P0)
INTEGER dim, i
double precision h,t,fun(dim),y(dim),yout(dim), k1(dim), k2(dim), k3(dim), k4(dim), yt(dim),dyt(dim),P0
EXTERNAL derivs 

do i = 1, dim     
	k1(i) = h * fun(i) 				! k1 = h*f(tn,yn)
  	yt(i)=y(i)+ k1(i)/2.0d0 			! Compute y(n) + k1/2 and store it in yt
end do  
call derivs(t+h/2.0d0,yt,dyt,P0)    			! Get f(tn+h/2,yn+k1/2) and store in dyt.
do i = 1, dim
	k2(i) = h * dyt(i)				! k2 = h *  f(tn+h/2,yn+k1/2)	
  	yt(i)=y(i)+k2(i)/2.0d0 			! Compute y(n) + k2/2 and store it in yt
end do  
call derivs(t+h/2.0d0,yt,dyt,P0)     			! Get f(tn+h/2,yn+k2/2) and store in dyt.
do i = 1, dim			
	k3(i) = h * dyt(i)	 			! k3 = h *  f(tn+h/2,yn+k2/2)
  	yt(i)=y(i)+k3(i) 				! Compute y(n) + k3 and store it in yt
end do  
call derivs(t+h,yt,dyt,P0)   			! Get f(tn+h,yn+k3) and store in dyt.
do i = 1, dim     
	k4(i) = h * dyt(i)				! k4 = h *  f(tn+h,yn+k3)			 
	yout(i)=y(i)+(k1(i)+2.0d0*k2(i)+2.0d0*k3(i)+k4(i))/6.0d0  !  Accumulate increments with proper weights.
end do 
END 
