tennis[, "PA"] <- (1/tennis$B365A)/(1/tennis$B365A+1/tennis$B365B)
tennis[, "PB"] <- 1 - tennis[, "PA"]
for(i in 1:dim(tennis)[1]) {
  if(tennis$ASets[i] > tennis$Bsets[i]) {
    tennis[i, "WinA"] <- 1
  } else {
    tennis[i, "WinA"] <- 0
  }
}
buckets <- split(tennis[,c("PA","WinA")],cut(tennis$PA, breaks = 10))
averages <- matrix(nrow = 10, ncol = 2)
colnames(averages) <- c("Average implied probability", "Average actual win percentage")
for(i in 1:10) {
  averages[i,1] <- mean(buckets[[i]][,1])
  averages[i,2] <- mean(buckets[[i]][,2])
}
plot(x=averages[,1], y=averages[,2], xlab = colnames(averages)[1], ylab = colnames(averages)[2])
lines(seq(from=0,to=1), seq(from=0,to=1))