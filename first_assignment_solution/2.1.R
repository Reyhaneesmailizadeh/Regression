mydata = data.frame(molar = c(1.0,0.9,0.8,0.7,0.6,0.5,0.4,0.3), chasb = c(0.45, 0.20, 0.34, 0.58, 0.70, 0.57, 0.55, 0.44))
attach(mydata)
mym = lm(chasb ~ molar)
plot(chasb ~ molar) + abline(mym)
mym
model_summary = summary(mym)
intercept = model_summary$coefficients[1,1]
m = model_summary$coefficients[2,1]
yhat = intercept + m * mydata$molar
yhat
(fittedvalues <- mym$fitted.values)