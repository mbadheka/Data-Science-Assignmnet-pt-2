# 4.1.Create a scatter plot for petal length and width variables.

t1<- read.csv("Data Set 5.csv")
# plot(t1$Size, t1$KW, xlab="Size", ylab="KW")

# 4.2 Calculate a linear regression model.

plot(t1$Size, t1$KW, xlab="Size", ylab="KW")
mm1<-lm(t1$KW~t1$Size)
lines(t1$Size,mm1$fitted.values,col="black")


plot(t1$Size, t1$KW, xlab="Size", ylab="KW")
mm1<-lm(t1$KW~poly(t1$Size))
lines(t1$Size,mm1$fitted.values,col="black")

#4.3 Calculate polynomial regression models of order 2 and 3

plot(t1$Size, t1$KW, xlab="Size", ylab="KW")
mm1<-lm(t1$KW~poly(t1$Size,2))
lines(t1$Size,mm1$fitted.values,col="red")

plot(t1$Size, t1$KW, xlab="Size", ylab="KW")
mm2<-lm(t1$KW~poly(t1$Size,3))
#lines(t1$Size,mm2$fitted.values,col="black")




#4.4 

t1<- read.csv("Data Set 5.csv")

par(mfrow = c(1,3), mar=c(4, 4, 2,1))

plot(t1$Size, t1$KW, xlab="x", ylab="y", main ="observed data")
mm<-lm(t1$KW~poly(t1$Size))
lines(t1$Size,mm$fitted.values,col="red")

plot(t1$Size, t1$KW, xlab="x", ylab="y", main ="observed data")
mm1<-lm(t1$KW~poly(t1$Size,2))
lines(t1$Size,mm1$fitted.values,col="red")

plot(t1$Size, t1$KW, xlab="x", ylab="y", main ="observed data")
mm2<-lm(t1$KW~poly(t1$Size,3))
lines(t1$Size,mm2$fitted.values,col="red")

# 4.5 Compare the accuracy of the three models

error_l = mm$fitted.value-t1$KW
MSE_l = mean(error_l^2)
# [1] 14320.65

error_2 = mm1$fitted.value-t1$KW
MSE_2 = mean(error_2^2)
# [1] 2618886

error_3 = mm2$fitted.value-t1$KW
MSE_3 = mean(error_3^2)
# [1] 2618234


