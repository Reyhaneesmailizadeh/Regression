mydata <- data.frame(x = c(2,1,5,5,20,20,23,10,30,25), y = c(1.9,3.1,3.3,4.8,5.3,6.1,6.4,7.6,9.8,12.4))
attach(mydata)
mymodel <- lm(y ~x)
plot(y~x) + abline(mymodel)
mymodel
(intercept = mymodel$coefficients[1])
(slope = mymodel$coefficients[2])
myfits <- mymodel$fitted.values
myfits