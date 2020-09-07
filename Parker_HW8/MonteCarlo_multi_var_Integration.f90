Program Mult_Int
!	This program performs multiple integral using Monte-Carlo
Implicit None
Double Precision u, v, w, x, y, z
Double Precision a1, b1, a2, b2, a3, b3
Double Precision a4, b4, a5, b5, a6, b6
Double Precision ran2, sum, int
Integer i, j, k, N
Integer seed1, seed2, seed3, seed4, seed5, seed6
!	Bounds of Integration
Parameter (N = 100000000)
Parameter (a1 = -1.0d0 ,b1 = 1.0d0)
Parameter (a2 = -5.0d0 ,b2 = 4.0d0)
Parameter (a3 = 2.0d0 ,b3 = 3.0d0)
Parameter (a4 = 1.0d0 ,b4 = 4.0d0)
Parameter (a5 = -1.0d0 ,b5 = 5.0d0)
Parameter (a6 = 0.0d0 ,b6 = 2.0d0)
!	Random Number Seeds
seed1 = -43
seed2 = -55
seed3 = -92
seed4 = -71
seed5 = -23
seed6 = -43
!	Initialize summation
sum = 0
!	Monte Carlo Technique accuracy increases with N
Do i = 1, N                          		
	u = a1 + ((b1-a1)*ran2(seed1))			
	v = a2 + ((b2-a2)*ran2(seed2))			
	w = a3 + ((b3-a3)*ran2(seed3))		
	x = a4 + ((b4-a4)*ran2(seed4))		
	y = a5 + ((b5-a5)*ran2(seed5))
	z = a6 + ((b6-a6)*ran2(seed6))
	sum = sum + (u**6)*(v**3)*(w**3)*(x**3)*(y)*(z**2)	
End Do
!	Final calulation of Integral
int = (b1-a1)*(b2-a2)*(b3-a3)*(b4-a4)*(b5-a5)*(b6-a6)*sum/float(N)
Write(*,*)int
End Program Mult_Int
!--------------------------------------------------------------------------------------
FUNCTION ran2(idum)
INTEGER idum,IM1,IM2,IMM1,IA1,IA2,IQ1,IQ2,IR1,IR2,NTAB,NDIV
double precision ran2,AM,EPS,RNMX
PARAMETER (IM1=2147483563,IM2=2147483399,AM=1./IM1,      & 
&     IMM1=IM1-1,IA1=40014,IA2=40692,IQ1=53668,         &
&     IQ2=52774,IR1=12211,IR2=3791,NTAB=32,             &
&     NDIV=1+IMM1/NTAB,EPS=1.2e-7,RNMX=1.-EPS)
INTEGER idum2,j,k,iv(NTAB),iy
SAVE iv,iy,idum2
DATA idum2/123456789/, iv/NTAB*0/, iy/0/
if (idum.le.0)then
   idum=max(-idum,1) 
   idum2=idum
   do  j=NTAB+8,1,-1 
      k=idum/IQ1
      idum=IA1*(idum-k*IQ1)-k*IR1
      if (idum.lt.0) idum=idum+IM1
      if (j .le. NTAB) iv(j)=idum
   end do
   iy=iv(1)
end if
k=idum/IQ1
idum=IA1*(idum-k*IQ1)-k*IR1 
if (idum.lt.0) idum=idum+IM1
k=idum2/IQ2
idum2=IA2*(idum2-k*IQ2)-k*IR2
if (idum2.lt.0) idum2=idum2+IM2
j=1+iy/NDIV
iy=iv(j)-idum2 
iv(j)=idum
if(iy.lt.1)iy=iy+IMM1
ran2=min(AM*iy,RNMX) 
END
