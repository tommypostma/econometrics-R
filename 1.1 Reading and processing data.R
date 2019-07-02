library("rJava")
library("xlsx")
rm(list = ls())
tennis <- read.xlsx("C:/Users/tommy/OneDrive/EOR/3/3.1a/Numerical Methods/Assignments/1/tennis.xlsx",1)
for(i in c(seq(from=13, to=28), 30, 31)) {
  tennis[,i] <- as.numeric(tennis[,i])
}
tennis$Date <- format(tennis$Date,"%y-%m")
table(tennis$Date)
avPayoutA <- mean(tennis$B365A)
avPayoutB <- mean(tennis$B365B)
t.test(tennis$B365A,tennis$B365B, use = "pairewise.complete.obs")