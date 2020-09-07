clear Workspace
%figure(1)
FigHandle = figure;
set(FigHandle, 'Position', [500, 500, 500, 500]);
load P3.dat;
%%
N=15; 
L = pi;
numframes=50; 
A=moviein(numframes); % create the movie matrix 
set(gca,'NextPlot','replacechildren') 
%axis equal % fix the axes 

 for i=1:numframes 
     x=P3(N^2*(i-1)+1:N^2*i,1);
     xarray=reshape(x,N,N);
     y=P3(N^2*(i-1)+1:N^2*i,2);
     yarray=reshape(y,N,N);
     z=P3(N^2*(i-1)+1:N^2*i,3);
     zarray=reshape(z,N,N);
     surf(xarray,yarray,zarray,'linestyle','none')
     xlabel('X (m)','fontsize',14)
     ylabel('Y (m)','fontsize',14) 
     zlabel('Z (m)','fontsize',14)
     axis([0 L 0 L -1 1])
     shading interp;
     lighting phong
     grid on;
     box on;
     A(:,i)=getframe;
    
 end 
 %%
movie(A,1,6) % Play the MATLAB movie 
% %save movie.mat A % save the MATLAB movie to a file 
v = VideoWriter('P4','MPEG-4');
v.FrameRate = 12;
v.Quality = 100;
open(v);
writeVideo(v,A);
close(v)
