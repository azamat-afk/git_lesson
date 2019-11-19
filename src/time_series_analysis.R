R Code and Output
#Time series analysis
#Plot data
time_unemp=ts(data = unemp_f$Unemployment, start = c(1948, 1), end=c(1981, 12),
              frequency = 12)
plot(time_unemp, main="US Monthly Employment Figures for Females, Aged 20 Years and
     Over from 1948-1981", ylab="Unemployment, in 1000", type="l")
#Decomposition using STL
time_unemp %>%
  stl(t.window=13, s.window="periodic", robust=TRUE) %>%
  autoplot()
acf(time_unemp, lag.max=72) #decays slowly
pacf(time_unemp, lag.max=72) #1 lag
#acf(diff(time_unemp), lag.max=72) #4 lags
y1 <- diff(diff(time_unemp), 12)
acf(y1, lag.max=72)
pacf(y1, lag.max=72) #3 lags
#Alternative code for STL
plot(dog <- stl(time_unemp, "per"))
#First model (1,0,0) (0,0,1)
u1001 <- arima(y1, order=c(1, 0, 0), seasonal=list(order=c(0, 0, 1), period=12))
u1030 <- arima(y1, order=c(1, 0, 0), seasonal=list(order=c(3, 0, 0), period=12))
u1001
##
## Call:
## arima(x = y1, order = c(1, 0, 0), seasonal = list(order = c(0, 0, 1), period =
12))
##
## Coefficients:
## ar1 sma1 intercept
## -0.1287 -0.7945 0.3391
## s.e. 0.0503 0.0338 0.9102
##
## sigma^2 estimated as 7549: log likelihood = -2329.99, aic = 4667.98
u1030
##
#Second model (1,0,0) (3,0,0)
## Call:
## arima(x = y1, order = c(1, 0, 0), seasonal = list(order = c(3, 0, 0), period =
12))
##
## Coefficients:
## ar1 sar1 sar2 sar3 intercept
## -0.1184 -0.7347 -0.5293 -0.2542 0.8832
## s.e. 0.0012 0.0014 0.0023 0.0013 1.6349
##
## sigma^2 estimated as 7846: log likelihood = -2335.44, aic = 4682.89
8
# Fit without the mean
u1001a <- arima(y1, order=c(1, 0, 0), seasonal=list(order=c(0, 0, 1), period=12),
                include.mean=F)
u1001a
##
## Call:
## arima(x = y1, order = c(1, 0, 0), seasonal = list(order = c(0, 0, 1), period =
12),
## include.mean = F)
##
## Coefficients:
## ar1 sma1
## -0.1284 -0.7939
## s.e. 0.0503 0.0338
##
## sigma^2 estimated as 7552: log likelihood = -2330.06, aic = 4666.12
#Fitting SARIMA model
Female_Unemployment=time_unemp
sarima(Female_Unemployment, 1,1,0, 0,1,1, 12)
## $fit
##
## Call:
## stats::arima(x = xdata, order = c(p, d, q), seasonal = list(order = c(P, D,
## Q), period = S), include.mean = !no.constant, optim.control = list(trace =
trc,
## REPORT = 1, reltol = tol))
##
## Coefficients:
## ar1 sma1
## -0.1284 -0.7939
## s.e. 0.0503 0.0338
##
## sigma^2 estimated as 7552 (86.9): log likelihood = -2330.06, aic = 4666.12
##
## $degrees_of_freedom
## [1] 393
##
## $ttable
## Estimate SE t.value p.value
## ar1 -0.1284 0.0503 -2.5541 0.011
## sma1 -0.7939 0.0338 -23.4718 0.000
##
## $AIC
## [1] 9.939437
##
## $AICc
## [1] 9.944485
##
## $BIC
## [1] 8.959101
Forecast for 24 months (SARIMA model)
9
sarima.for(Female_Unemployment, 24, 1, 1, 0, 0, 1, 1, 12)
## $pred
## Jan Feb Mar Apr May Jun Jul
## 1982 3304.678 3262.778 3144.609 3045.767 3051.044 3234.649 3305.520
## 1983 3488.993 3446.225 3328.167 3229.312 3234.590 3418.195 3489.066
## Aug Sep Oct Nov Dec
## 1982 3410.034 3406.141 3329.901 3293.212 3168.555
## 1983 3593.580 3589.687 3513.447 3476.758 3352.101
##
## $se
## Jan Feb Mar Apr May Jun Jul
## 1982 86.90511 115.28396 138.73465 158.66906 176.37480 192.45728 207.29593
## 1983 285.73146 300.37158 314.41768 327.85185 340.75811 353.19291 365.20458
## Aug Sep Oct Nov Dec
## 1982 221.14110 234.16912 246.50956 258.26102 269.50054
## 1983 376.83357 388.11428 399.07624 409.74504 420.14302
Holt-Winter's Method (forecast for 24 months and 80, 95% PI)
un <- window(time_unemp,start=1948)
fit1 <- hw(un,h=24, seasonal="additive")
fit1
## Point Forecast Lo 80 Hi 80 Lo 95 Hi 95
## Jan 1982 3322.013 3207.017 3437.009 3146.142 3497.884
## Feb 1982 3283.024 3132.162 3433.887 3052.300 3513.748
## Mar 1982 3176.644 2996.930 3356.358 2901.795 3451.493
## Apr 1982 3091.574 2887.033 3296.116 2778.756 3404.393
## May 1982 3094.292 2867.622 3320.962 2747.630 3440.954
## Jun 1982 3259.367 3012.539 3506.194 2881.877 3636.857
## Jul 1982 3285.217 3019.754 3550.680 2879.227 3691.208
## Aug 1982 3327.220 3044.342 3610.097 2894.596 3759.843
## Sep 1982 3287.206 2987.922 3586.489 2829.490 3744.921
## Oct 1982 3176.864 2862.023 3491.704 2695.357 3658.370
## Nov 1982 3137.281 2807.614 3466.947 2633.099 3641.462
## Dec 1982 3040.386 2696.528 3384.244 2514.501 3566.271
## Jan 1983 3372.492 3011.228 3733.755 2819.987 3924.996
## Feb 1983 3333.503 2959.239 3707.768 2761.115 3905.891
## Mar 1983 3227.123 2840.291 3613.955 2635.515 3818.731
## Apr 1983 3142.053 2743.047 3541.059 2531.826 3752.280
## May 1983 3144.771 2733.948 3555.594 2516.472 3773.070
## Jun 1983 3309.845 2887.534 3732.157 2663.976 3955.715
## Jul 1983 3335.696 2902.197 3769.194 2672.717 3998.674
## Aug 1983 3377.698 2933.292 3822.105 2698.037 4057.359
## Sep 1983 3337.684 2882.629 3792.740 2641.737 4033.632
## Oct 1983 3227.342 2761.879 3692.806 2515.477 3939.207
## Nov 1983 3187.759 2712.113 3663.405 2460.321 3915.197
## Dec 1983 3090.865 2605.247 3576.483 2348.177 3833.553
autoplot(un) +
  autolayer(fit1, series="HW additive forecasts", PI=TRUE) +
  xlab("Year") +
  ylab("Female Unemployment (thousands)") + guides(colour=guide_legend(title=
                                                                         "Forecast"))
Smoothing parameters of Additive Holt-Winter's Model
fit1$model;
10
## Holt-Winters' additive method
##
## Call:
## hw(y = un, h = 24, seasonal = "additive")
##
## Smoothing parameters:
## alpha = 0.8491
## beta = 1e-04
## gamma = 0.1131
## Initial states:
## l = 466.1683
## b = 4.0841
## s=-129.9595 -15.2181 -32.7112 42.6908 75.2465 31.6693
## 21.9217 -88.9058 -45.6138 10.355 55.3836 75.1414
##
## sigma: 89.7317
##
## AIC AICc BIC
## 6139.683 6141.253 6207.875
Calculating Ljung-Box statistic of Additive Holt-Winter's Model
checkresiduals(fit1)
## Ljung-Box test
##
## data: Residuals from Holt-Winters' additive method
## Q* = 48.507, df = 8, p-value = 7.903e-08
##
## Model df: 16. Total lags used: 24
Calculating Ljung-Box statistic of SARIMA Model
fit4 <- arima(time_unemp, order=c(1,1,0), seasonal=c(0,1,1))
checkresiduals(fit4)
## Ljung-Box test
##
## data: Residuals from ARIMA(1,1,0)(0,1,1)[12]
## Q* = 29.354, df = 22, p-value = 0.135
##
## Model df: 2. Total lags used: 24