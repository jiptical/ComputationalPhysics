Program Problem3
Implicit None
Integer i,N
Double Precision simpson,gauss,der,a,b,act,err
a = -4.000000000000000d0
b = 4.0000000000000000d0
act = 0.9999366575163338d0

open(unit=2, file='gauss.dat', status='replace', action='Write')
open(unit=3, file='simpson.dat', status='replace', action='Write')



!The Gaussian Quadrature Method
Do i = 1,91,2
N = (10+i)
der = gauss(N,a,b)
err = abs(der-act)
Write(2,*)N,err
End Do


!The Simpson's Method
Do i = 1,91,2
N = (10+i)
der = simpson(N,a,b)
err = abs(der-act)
Write(3,*)N,err
End Do

End Program

!---------------------------------------------------------
!Compute the value of the given function
Function f(x)
Implicit None
Double Precision f,x,pi
pi = acos(-1.0d0)
f = (1/sqrt(2*pi))*exp((-x**2)/2)
Return
End Function f

!Simpson's Rule Method
Function simpson(N,a,b)
Implicit None
Double Precision f,x, dx, a, b, simpson
Integer i,N
simpson = 0.0d0
dx = ((b-a) / float(N-1))
Do i=2, (N-1), 2
x = a + dx * float(i-1)
simpson = simpson + 4.0d0 * f(x)
End Do
! loop for odd points
Do i=3, (N-1), 2
x = a + dx * float(i-1)
simpson = simpson + 2.0d0 * f(x)
End Do
! add the endpoints
simpson = simpson + f(a) + f(b)
simpson = simpson * dx/3.0d0
Return
End Function simpson

!Gaussian Quadrature Method
Function gauss(N,a,b)
Implicit None
Integer i, N ,job
Double Precision x(1000),w(1000)
Double Precision f,a,b,gauss
gauss =0.0d0
job=0
call gauss_points(N, job, a, b, x, w)
Do i = 1, N
Gauss = Gauss + f(x(i)) * w(i)
End Do

Return
End Function gauss

!-------------------------------------------------------------
Subroutine gauss_points(npts,job,a,b,x,w)
!Points and weights for Gaussian quadrature
!rescale rescales the gauss-legendre grid points and weights
!npts     number of points
!job = 0  rescalling uniformly between (a,b)
!      1  for integral (0,b) with 50% points inside (0, ab/(a+b))
!      2  for integral (a,inf) with 50% inside (a,b+2a)
!x, w     output grid points and weights.
!it uses (l+1)P_l+1(x)=(2l+1)P_l(x)-lP_l-1(x)   to compute legendre polynomials.

integer npts,job,m,i,j
Double Precision x(npts),w(npts),a,b,xi
Double Precision t,t1,pp,p1,p2,p3,l
Double Precision eps,pi
parameter (pi = 3.14159265358979323846264338328, eps = 3.0E-14)

m=(npts+1)/2
Do i=1,m
t=cos(pi*(i-0.25d0)/(npts+0.5d0))
1000    continue
p1=1.0d0
p2=0.0d0
l=0.0d0
Do j=1,npts
p3=p2
p2=p1
l=l+1.0d0
p1=((2.0d0*l-1.0d0)*t*p2-(l-1.0d0)*p3)/l
End Do
pp=npts*(t*p1-p2)/(t*t-1.0d0)
t1=t
t=t1-p1/pp

if(abs(t-t1).gt.eps) goto 1000

x(i)=-t
x(npts+1-i)=t
w(i)=2.0d0/((1.0d0-t*t)*pp*pp)
w(npts+1-i)=w(i)
End Do

! rescale the grid points
if (job.eq.0) then
!     scale to (a,b) uniformly
do i=1,npts
x(i)=x(i)*(b-a)/2.0d0+(b+a)/2.0d0
w(i)=w(i)*(b-a)/2.0d0
End Do
elseif (job.eq.1) then
! scale to (0,b) with 50% points inside (0,ab/(a+b))
do i=1,npts
xi=x(i)
x(i)=a*b*(1.0d0+xi)/(b+a-(b-a)*xi)
w(i)=w(i)*2.0d0*a*b*b/((b+a-(b-a)*xi)*(b+a-(b-a)*xi))
End Do
elseif (job.eq.2) then
! scale to (a,inf) with 50% points inside (a,b+2a)
do i=1,npts
xi=x(i)
x(i)=(b*xi+b+a+a)/(1.0d0-xi)
w(i)=w(i)*2.0d0*(a+b)/((1.0d0-xi)*(1.0d0-xi))
End Do
else
pause 'Wrong value of job'
endif

Return
End Subroutine gauss_points

!--------------------------------------------
