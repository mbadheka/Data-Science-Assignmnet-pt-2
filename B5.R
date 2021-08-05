
#inputiing data in machine 

machine1 <- c(16.03, 16.04, 16.05, 16.05, 16.02, 16.01, 15.96, 15.98, 16.02, 15.99)
machine2 <- c(16.02, 15.97, 15.96, 16.01, 15.99, 16.03, 16.04, 16.02, 16.01, 16.00)

##Calculate Mean

mean.machine1 <- mean(machine1)
mean.machine2 <- mean(machine2)

##Calculate Variance (not usind SD because varienace is squar of SD)

variance.machine1 <- var(machine1)
variance.machine2 <- var(machine2)

## Z Factor


SP.Machine <- (((10-1)*variance.machine1)+((10-1)*variance.machine2))/(10+10-2)
SP.Machine <- sqrt(SP.Machine)

z0 = (mean.machine1 -mean.machine2)/ (SP.Machine*(sqrt((1/10) +( 1/10))))

t0.025 <- 2.101 ## Value from the z distribution table

##Since t0 is 0.79835 which doesn't lie in the critical region, we accept the Null Hypothesis. x