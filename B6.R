alpha.value <- 0.01
mean.participant <- 110
avg.iq <- 100
sd.iq <- 10
n<-30 #particepants

## Z is calculated as +/- 2.58

z0 <- (mean.participant-avg.iq)/(sd.iq/sqrt(n))
z0


##Result: Reject the Null Hypothesis.

##Conclusion: Medication significantly affect the intelligence Z = 5.477, p < 0.01.

##Confidence Interval

L = 110 - (2.58 * (10 /sqrt(30)))
U = 110 + (2.58 * (10 /sqrt(30)))
L
U

##110 +/- 4.7104