t1<- read.csv("Data Set 3.csv")

par(mfrow = c(2,2), mar=c(4, 4, 2,1))
hist( t1$L,col = "Sky Blue", xlab = "L", main = "Histogram of L")
hist( t1$H,col = "Sky Blue", xlab = "H", main = "Histogram of H")
hist( t1$N,col = "Sky Blue", xlab = "N", main = "Histogram of N")
hist( t1$B,col = "Sky Blue", xlab = "B", main = "Histogram of B")

t2<- read.csv("Data Set 3.csv")
par(mfrow = c(2,2), mar=c(4, 4, 2,1))
plot(density(t2$L), xlab = "N= 150 Bandwidth= 1.726" , main= "Density function of L")
plot(density(t2$H), xlab = "N= 150 Bandwidth= 1.632" , main= "Density function of H")
plot(density(t2$N), xlab = "N= 150 Bandwidth= 0.9862" , main= "Density function of N")
plot(density(t2$B), xlab = "N= 150 Bandwidth= 1.479" , main= "Density function of B")

install.packages("moments")

library(moments)

t3 <- read.csv("Data Set 3.csv",na.strings="")
mysubsetL <- density(t3$L)
mysubsetH <- density(t3$H)
mysubsetN <- density(t3$N)
mysubsetB <- density(t3$B)

par(mfrow = c(2,2), mar = c(4,4,2,1))

plot(mysubsetL, main="Density Function of L")
plot(mysubsetH, main="Density Function of H")
plot(mysubsetN, main="Density Function of N")
plot(mysubsetB, main="Density Function of B")

mysubsetL <- t3$L
mysubsetH <- t3$H
mysubsetN <- t3$N
mysubsetB <- t3$B

skewness(mysubsetL)
kurtosis(mysubsetL)

skewness(mysubsetH)
kurtosis(mysubsetH)

skewness(mysubsetN)
kurtosis(mysubsetN)

skewness(mysubsetB)
kurtosis(mysubsetB)


#Creating Boxplot
t4 <- read.csv("Data Set 3.csv",na.strings="")
mysubsetL <- t4$L
mysubsetH <- t4$H
mysubsetN <- t4$N
mysubsetB <- t4$B

par(mfrow = c(1,4), mar = c(4,4,2,1))

boxplot(mysubsetL, ylim = c(40,150), main = "L")
boxplot(mysubsetH, ylim = c(40,150), main = "H")
boxplot(mysubsetN, ylim = c(40,150), main = "N")
boxplot(mysubsetB, ylim = c(40,150), main = "B")


# Calculate the mean, variance and standard deviation of L, H, N and B
mean(mysubsetL)
mean(mysubsetH)
mean(mysubsetN)
mean(mysubsetB)

var(mysubsetL)
var(mysubsetH)
var(mysubsetN)
var(mysubsetB)

sd(mysubsetL)
sd(mysubsetH)
sd(mysubsetN)
sd(mysubsetB)
mean(t4$L)
mean(t4$H)
mean(t4$N)
mean(t4$B)

var(t4$L)
var(t4$H)
var(t4$N)
var(t4$B)

sd(t4$L)
sd(t4$H)
sd(t4$N)
sd(t4$B)

