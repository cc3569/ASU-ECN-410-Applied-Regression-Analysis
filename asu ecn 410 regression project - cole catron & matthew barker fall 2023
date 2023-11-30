Y.PA = -309.94052 + 1.64826*X1 + 16.86099*X11 + 2.24244*X12 + 1.41764*X14 + 0.62649*X19 + 7.18646*X22

#tests for heteroskedasticity and autocorrelation among variables
library(lmtest)
bptest(Y.PA)
dwtest(Y.PA)

#adjusted model on significant variables
Y.PA = lm(d$Y~d$X1+d$X11+d$X12+d$X14+d$X19+d$X22)
summary(Y.PA)
library(lmtest)
bptest(Y.PA)
dwtest(Y.PA)

#scatter plots
par(mfrow=c(3,2))
plot(d$X1,d$Y)
plot(d$X11,d$Y)
plot(d$X12,d$Y)
plot(d$X14,d$Y)
plot(d$X19,d$Y)
plot(d$X22,d$Y)

par(mfrow=c(2,2))
plot(Y.PA)

#boxcox analysis
library(MASS)
a_m = boxcox(Y.PA)

optimal_lambda = a_m$x[which.max(a_m$y)]
print(optimal_lambda)

#test for multicollinearity
library(car)
vif_results = vif(Y.PA)
print(vif_results)

correlation_matrix = cor(d[, c("X1", "X11", "X12", "X14", "X19", "X22")])
print(correlation_matrix)

eigenvalues = eigen(correlation_matrix)$values
print(eigenvalues)