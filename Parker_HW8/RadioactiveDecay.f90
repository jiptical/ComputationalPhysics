Program Radioactive_Decay
!	Uses random number generator to solve radioactive decay problem

Implicit None


Integer i, N
Double Precision lambda, ran2, dN
Double Precision seed,time

N = 1000
time = 0.0d0	!initial time 
seed = -100	!seed for ran2
lambda = 0.01

! Open data files for writing data
! overwrite if file exists
Open(unit = 1,file='p11.dat',status='replace')
Open(unit = 2,file='p12.dat',status='replace')
Open(unit = 3,file='p13.dat',status='replace')


Do While(N.GT.0)
	dN = 0
	Do i = 1, N
		If(ran2(seed) .LT. lambda)Then
		dN = dN + 1
		End If
	End Do
	time = time + 1.0
	N = N - dN
	Write(1,*)time,N
End Do

N = 100000
time = 0.0d0
Do While(N.GT.0)
	dN = 0
	Do i = 1, N
		If(ran2(seed) .LT. lambda)Then
		dN = dN + 1
		End If
	End Do
	time = time + 1.0
	N = N - dN
	Write(2,*)time,N
End Do

N = 1000000
time = 0.0d0
Do While(N.GT.0)
	dN = 0
	Do i = 1, N
		If(ran2(seed) .LT. lambda)Then
		dN = dN + 1
		End If
	End Do
	time = time + 1.0
	N = N - dN
	Write(3,*)time,N
End Do
End Program
!-----------------------------------------------

FUNCTION ran2(idum)
INTEGER idum,IM1,IM2,IMM1,IA1,IA2,IQ1,IQ2,IR1,IR2,NTAB,NDIV
REAL ran2,AM,EPS,RNMX
PARAMETER (IM1=2147483563,IM2=2147483399,AM=1./IM1,      & 
&     IMM1=IM1-1,IA1=40014,IA2=40692,IQ1=53668,          &
&     IQ2=52774,IR1=12211,IR2=3791,NTAB=32,              &
&     NDIV=1+IMM1/NTAB,EPS=1.2e-7,RNMX=1.-EPS)
INTEGER idum2,j,k,iv(NTAB),iy
SAVE iv,iy,idum2
DATA idum2/123456789/, iv/NTAB*0/, iy/0/
if (idum.le.0)then			!Initializing
   idum=max(-idum,1) 		!prevents idum = 0	
   idum2=idum
   do  j=NTAB+8,1,-1 			!load the shuffle table after 8 warmups
      k=idum/IQ1
      idum=IA1*(idum-k*IQ1)-k*IR1
      if (idum.lt.0) idum=idum+IM1
      if (j .le. NTAB) iv(j)=idum
   end do
   iy=iv(1)
end if
k=idum/IQ1					!start here when not initializing 
idum=IA1*(idum-k*IQ1)-k*IR1 !computes idum=(IA1*idum) % IM1
if (idum.lt.0) idum=idum+IM1
k=idum2/IQ2
idum2=IA2*(idum2-k*IQ2)-k*IR2	!computes idum2=(IA2*idum) % IM2
if (idum2.lt.0) idum2=idum2+IM2
j=1+iy/NDIV
iy=iv(j)-idum2 		!here idum is shuffled, idum and idum2 are combined to generate output 
iv(j)=idum
if(iy.lt.1)iy=iy+IMM1
ran2=min(AM*iy,RNMX) 
END
