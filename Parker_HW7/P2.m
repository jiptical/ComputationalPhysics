clear all
load p2.dat;
%%
N = 100;
t = p2(:,1);
tarray = reshape(t,N,N);
X = p2(:,2);
xarray = reshape(X,N,N);
T = p2(:,3);
zarray = reshape(T,N,N);

surf(tarray,xarray,zarray,'linestyle','none')
xlabel('Time','fontsize',14)
ylabel('X','fontsize',14) 
zlabel('Temp (^o C)','fontsize',14)
shading interp;
%camlight; 
lighting phong;
grid on;
box on;
%%
clear all
load P3.dat;

N = 15;
t = P3(226:450,1);
tarray = reshape(t,N,N);
X = P3(226:450,2);
xarray = reshape(X,N,N);
T = P3(226:450,3);
zarray = reshape(T,N,N);

surf(tarray,xarray,zarray,'linestyle','none')
xlabel('X','fontsize',14)
ylabel('Y','fontsize',14) 
zlabel('Z','fontsize',14)
shading interp;
%camlight; 
lighting phong;
grid on;
box on;
