install.packages("lubridate")
library(lubridate)


t1 <- read.csv("Data set 1a.csv", na.strings = "")

t2<- read.csv("Data set 1b.csv", na.strings = "")

#cleaning the data 

levels(t1$Product) <- c(levels(t1$Product), "Galaxy Edge S8")
t1$Product[t1$Product == "Galaxi Edge S8"]<-"Galaxy Edge S8"

levels(t1$Name) <- c(levels(t1$Name), "Alice")
t1$Name[t1$Name == "ALice"]<-"Alice"

#setting the colum name in the table

names(t1) = c("No","Date","Consumer","Product","Quantity")

#change in date formate 

t1$Date<-dmy(t1$Date) 


t1$Quantity[t1$Quantity == "one"] <- "1"
t1$Quantity[t1$Quantity == "two"] <- "2"
t1 <-na.omit(t1)

#reading  the second tables

t3<-merge(t1,t2,"Product")

#change  to set currency rate

Calculate.Rate <-function(arg1){ 
  if (arg1=='NZD') 
  { 
    out<-1
  } 
  if(arg1=='AUD') 
  {
    out<-1.2
  }  
  return(out)
}

t3["Exchange.Rate"] <-sapply(t3$Currency,Calculate.Rate)
t3$Unit.Price.NZD <-t3$Unit.Price*t3$Exchange.Rate

#transation made after 1-1-2010


ans1<-subset(t3,t3$Date>dmy(01012010))

#calculating total sales 

ans1$Quantity <-as.numeric(ans1$Quantity)
ans1$amountnzd <- ans1$Quantity*ans1$Unit.Price.NZD


#calculating on monthly basis 

ans1$Month <- months(ans1$Date)
ans1$Year <- format(ans1$Date,format="%y")
toplot <- aggregate(amountnzd ~Month +Year, ans1, sum)
print(toplot)

#charts and plots 

barplot(toplot$amountnzd, names.arg = c(toplot$Month), xlab="Month", ylab="NZD", main = "Monthly Sales")
