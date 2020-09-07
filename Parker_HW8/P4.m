%% Problem 4
clear all
% load data
load HW84.dat
xdat = HW84(:,1);
ydat = HW84(:,2);

ft = fittype( 'a*exp(-b*x)*cos(c*x)', 'independent', 'x', 'dependent', 'y' );
opts = fitoptions( 'Method', 'NonlinearLeastSquares' );
opts.Display = 'Off';
opts.Lower = [5 -Inf -Inf -Inf];
opts.StartPoint = [10 0.808157185957016 0.619061087093806];
opts.Upper = [15 Inf Inf Inf];

% Fit model to data.
[fitresult, gof] = fit( xdat, ydat, ft, opts );

% Plot fit with data.
figure( 'Name', 'Problem 4 fit' );
h = plot( fitresult, xdat, ydat );
legend( h, 'Data', 'Problem 4 fit');
% Labels
title(' Problem 4: Fit vs Data')
xlabel( 'x');
ylabel( 'y');


