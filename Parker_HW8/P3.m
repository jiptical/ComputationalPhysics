clear all
%% Load the Data
load HW83-1.dat
xData = HW83_1(:,1);
yData = HW83_1(:,2);

%% Set up fittype and options.
ft = fittype( 'a*exp(-(x-b)^2 /c^2)+d', 'independent', 'x', 'dependent', 'y' );
opts = fitoptions( 'Method', 'NonlinearLeastSquares' );
opts.Display = 'Off';
opts.Lower = [0 -10 -10 -10];
opts.StartPoint = [10 0.991697238815374 0.975755338023859 0.4387];
opts.Upper = [15 10 10 10];

%% Fit model to data.
[fitresult, gof] = fit( xData, yData, ft, opts );

%% Plot fit with data.
figure( 'Name', 'Problem 3 Fit' );
h = plot( fitresult, xData, yData );
legend( h, 'Data', 'Problem 3 Fit');
% Labels
title('Problem 3: Fit vs Data')
xlabel( 'x');
ylabel( 'y');