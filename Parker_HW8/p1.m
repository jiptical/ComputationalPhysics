clear all
clear figs
clc

load p11.dat
load p12.dat
load p13.dat

x1=p11(:,1);
y1=p11(:,2);
x2=p12(:,1);
y2=p12(:,2);
x3=p13(:,1);
y3=p13(:,2);

ly1 = log10(y1);
ly2 = log10(y2);
ly3 = log10(y3);
%%
figure(1)
hold on
plot(x1,ly1,'b.-')
plot(x2,ly2,'r.-')
plot(x3,ly3,'g.-')
title('Plot for Problem 1')
xlabel('time')
ylabel('log_{10}(N)')
legend('N_0=1000','N_0=100000','N_0=1000000')
hold off

figure(2)
hold on
semilogy(x1,y1,'b.-')
semilogy(x2,y2,'r.-')
semilogy(x3,y3,'g.-')
title('Plot for Problem 1')
xlabel('time')
ylabel('N')
ylim([0 10^6])
legend('N_0=1000','N_0=100000','N_0=1000000')
hold off