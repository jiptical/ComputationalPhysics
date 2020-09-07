clear all
load p1.dat;
N=100; 
x=p1(:,1);
xarray=reshape(x,N,N);
y=p1(:,2);
yarray=reshape(y,N,N); 
z=p1(:,3);
zarray=reshape(z,N,N); 
surf(xarray,yarray,zarray,'linestyle','none') 
xlabel('X','fontsize',14)
ylabel('Y','fontsize',14) 
zlabel('V (mV)','fontsize',14)
shading interp;%camlight;
lighting phong 
grid on;
box on;