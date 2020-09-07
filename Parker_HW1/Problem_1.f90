Program HW2_Problem4
!we need 2 arrays for x and y data
Implicit none

Integer i,n
Double Precision, Allocatable:: x(:),y(:)
n = 18
open(unit=1, file = 'xlsf.dat', status='old', action='Read')
open(unit=2, file = 'ylsf.dat', status='old', action='Read')


	


