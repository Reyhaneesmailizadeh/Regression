data = data.frame(x = c(0.531,0.550,0.556,0.523,0.602,0.569,0.544,0.557,0.530,0.530,0.569,0.558,0.577,0.572,0.548,0.581,0.557,0.550,0.414,0.383,0.399,0.402,0.442,0.422,0.466,0.500,0.514) , y = c(73466,78610,67657,74017,87291,86836,82540,81699,82096,67705,66088,78486,89869,77369,67095,85156,69571,84160,29186,29266,26215,30162,38867,37831,44576,46097,59698))
mymodel = lm(y~x)
attach(data)
plot(y ~ x) + abline(mymodel)
res = residuals(mymodel)
n = 27
(MSE = sum(res^2) / (n - 2))
estimatedvarianceoferrors = MSE
meanx = mean(data$x)
sxx = (n-1) * var(x)
(estimatedvarianceofb1hat = MSE / sxx)
(estimatedvarianceofb0hat = MSE * (1 /n +(meanx ^ 2)/sxx))
(pvalue = summary(mymodel)$coefficients[2,4])
(beta1hat = mymodel$coefficients[2])
(amare = beta1hat / sqrt(MSE / sxx))
(pvalue2 = 2 * (1 - pt(abs(amare), n - 2 )))
anova(mymodel)
(pvalue3 = anova(mymodel)[1,5])
newdata = data.frame( x = 0.580)
predict(mymodel,newdata,interval = "confidence")
x0 = 0.580
(beta0hat = summary(mymodel)$coefficients[1])
(yhatx0 = beta0hat + beta1hat * x0)
CI = c(yhatx0 + qt(0.025, n-2) * sqrt(MSE * (1 + 1/n + (x0 - meanx)^2/sxx)), yhatx0 - qt(0.025,n-2) * sqrt(MSE * (1 + 1/n + (x0 - meanx)^2/sxx)))
names(CI) = c("lower","upper"); CI

