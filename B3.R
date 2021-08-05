# 3.1 Scatter Plot for petal length and width 


t1 <- read.csv("Data Set 4.csv")
plot(t1$Petal.Length, t1$Petal.Width, xlab="Petal length", ylab="Petal width")

#3.2 Linear calculation 
mm<-lm(Petal.Width ~ Petal.Length, data=t1)
lines(t1$Petal.Length,mm$fitted.values)

# 3.3 subdivede each species  individul (another way of doing )
# t1_se = subset(t1,t1$Species =='setosa')
# t2_ve = subset(t1,t1$Species =='versicolor')
# t3_vi = subset(t1,t1$Species =='virginica')

# 3.3 & 3.4 species 1 setosa 

setosa <-  subset(t1, Species == "setosa")
plot(setosa$Petal.Length, setosa$Petal.Width,  xlab="Petal length", ylab="Petal width")

##creating a black abline for setosa 

model1<-lm(Petal.Width ~ Petal.Length, data=setosa)
lines(setosa$Petal.Length,model1$fitted.values, col= "black")
# 3.3  & 3.4 versicolor Scatter plot


versicolor <-  subset(t1, Species == "versicolor")
plot(versicolor$Petal.Length, versicolor$Petal.Width,  xlab="Petal length", ylab="Petal width")
model2<-lm(Petal.Width ~ Petal.Length, data=versicolor)
lines(versicolor$Petal.Length,model2$fitted.values,col= "blue")

# 3.3 & 3.4 virginica scatter plot 
virginica <-  subset(t1, Species == "virginica")
plot(virginica$Petal.Length, virginica$Petal.Width,  xlab="Petal length", ylab="Petal width")
model3<-lm(Petal.Width ~ Petal.Length, data=virginica)
lines(virginica$Petal.Length,model3$fitted.values, col= "green")

#3.5 

plot(t1$Petal.Length, t1$Petal.Width,  col=c("black","blue","green")[unclass(t1$Species)],xlab="Petal length", ylab="Petal width")
model<-lm(Petal.Width ~ Petal.Length, data=t1)
lines(t1$Petal.Length,model$fitted.values, col= "red")
model1<-lm(Petal.Width ~ Petal.Length, data=setosa)
lines(setosa$Petal.Length,model1$fitted.values, col= "black")
model2<-lm(Petal.Width ~ Petal.Length, data=versicolor)
lines(versicolor$Petal.Length,model2$fitted.values, col= "blue")
model3<-lm(Petal.Width ~ Petal.Length, data=virginica)
lines(virginica$Petal.Length,model3$fitted.values, col= "green")

#3.6 

x<-t1$Petal.Length
y<-t1$Petal.Width

mm<-lm(y ~ x)
error<-model$fitted.values-y
MSE<-mean(error^2)
MSE

error_1<-model1$fitted.value-t1$Petal.Width
MSE_1<-mean(error_1^2)
MSE_1

error_2<-model2$fitted.value-t1$Petal.Width
MSE_2<-mean(error_2^2)
MSE_2

error_3<-model3$fitted.value-t1$Petal.Width
MSE_3<-mean(error_3^2)
MSE_3


# conclusion well , mse1 mse2 and mse3 shows higher value than all error 
# so it shows more accurate relationship between petal length and width


##********************* FOR SEPAL**************************

# 3.1 Scatter Plot for Sepal length and width 

t1_Sepal<- read.csv("Data Set 4.csv")
plot(t1_Sepal$Sepal.Length, t1_Sepal$Sepal.Width, xlab="Sepal length", ylab="Sepal width")

#3.2 Linear calculation 
mm<-lm(Sepal.Width ~ Sepal.Length, data=t1_Sepal)
lines(t1_Sepal$Sepal.Length,mm$fitted.values)

# 3.3 subdivede each species  individul (another way of doing )
# t1_se = subset(t1_Sepal,t1_Sepal$Species =='setosa')
# t2_ve = subset(t1_Sepal,t1_Sepal$Species =='versicolor')
# t3_vi = subset(t1_Sepal,t1_Sepal$Species =='virginica')

# 3.3 & 3.4 species 1 setosa 

setosa <-  subset(t1_Sepal, Species == "setosa")
plot(setosa$Sepal.Length, setosa$Sepal.Width,  xlab="Sepal length", ylab="Sepal width")

##creating a black abline for setosa 

model1<-lm(Sepal.Width ~ Sepal.Length, data=setosa)
lines(setosa$Sepal.Length,model1$fitted.values, col= "black")
# 3.3  & 3.4 versicolor Scatter plot


versicolor <-  subset(t1_Sepal, Species == "versicolor")
plot(versicolor$Sepal.Length, versicolor$Sepal.Width,  xlab="Sepal length", ylab="Sepal width")
model2<-lm(Sepal.Width ~ Sepal.Length, data=versicolor)
lines(versicolor$Sepal.Length,model2$fitted.values,col= "blue")

# 3.3 & 3.4 virginica scatter plot 
virginica <-  subset(t1_Sepal, Species == "virginica")
plot(virginica$Sepal.Length, virginica$Sepal.Width,  xlab="Sepal length", ylab="Sepal width")
model3<-lm(Sepal.Width ~ Sepal.Length, data=virginica)
lines(virginica$Sepal.Length,model3$fitted.values, col= "green")

#3.5 

plot(t1_Sepal$Sepal.Length, t1_Sepal$Sepal.Width,  col=c("black","blue","green")[unclass(t1_Sepal$Species)],xlab="Sepal length", ylab="Sepal width")
model<-lm(Sepal.Width ~ Sepal.Length, data=t1_Sepal)
lines(t1_Sepal$Sepal.Length,model$fitted.values, col= "red")
model1<-lm(Sepal.Width ~ Sepal.Length, data=setosa)
lines(setosa$Sepal.Length,model1$fitted.values, col= "black")
model2<-lm(Sepal.Width ~ Sepal.Length, data=versicolor)
lines(versicolor$Sepal.Length,model2$fitted.values, col= "blue")
model3<-lm(Sepal.Width ~ Sepal.Length, data=virginica)
lines(virginica$Sepal.Length,model3$fitted.values, col= "green")

#3.6 

x<-t1_Sepal$Sepal.Length
y<-t1_Sepal$Sepal.Width

mm<-lm(y ~ x)
error<-model$fitted.values-y
MSE_SEPAL<-mean(error^2)
MSE_SEPAL

error_1<-model1$fitted.value-t1_Sepal$Sepal.Width
MSE_SEPAL1<-mean(error_1^2)
MSE_SEPAL1

error_2<-model2$fitted.value-t1_Sepal$Sepal.Width
MSE_SEPAL2<-mean(error_2^2)
MSE_SEPAL2

error_3<-model3$fitted.value-t1_Sepal$Sepal.Width
MSE_SEPAL3<-mean(error_3^2)
MSE_SEPAL3

