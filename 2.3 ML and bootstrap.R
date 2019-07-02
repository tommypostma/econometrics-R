data3 <- c(3,5,7,18,43,85,91,98,100,130,230,487)
n <- length(data3)
beta.hat <- mean(data3)
lambda.hat <- 1/beta.hat
print(lambda.hat)
# [1] 0.00925212
B <- 9999
lambda.star <- rep(NA,B)
set.seed(19290)
for(b in 1:B) {
  sample.b <- sample(data3,n,replace = T)
  lambda.star[b] <- 1/mean(sample.b)
}
bias.lambda.hat <- mean(lambda.star) - lambda.hat
print(bias.lambda.hat)
# [1] 0.001313015
se.lambda.hat <- sd(lambda.star)
print(se.lambda.hat)
# [1] 0.004359665

beta.loo <- rep(NA,n)
for(i in 1:n) {
  beta.loo[i] <- mean(data3[-i])
}
psi <- mean(beta.loo) - beta.loo
a <- (sum(psi^3)/6)/(sum(psi^2)^1.5)
b <- qnorm(mean(1/lambda.star < beta.hat))
alpha <- 0.05
percentile1 <- pnorm(b + (b + qnorm(alpha/2))/(1 - a*(b + qnorm(alpha/2))))
print(percentile1)
# [1] 0.06596048
percentile2 <- pnorm(b + (b + qnorm(1 - alpha/2))/(1 - a*(b + qnorm(1 - alpha/2))))
print(percentile2)
# [1] 0.9955992
bca.interval <- quantile(1/lambda.star,c(percentile1,percentile2))
se.beta.hat <- sd(1/lambda.star)
print(se.beta.hat)
# [1] 37.70684
normal.interval <- beta.hat + se.beta.hat*qnorm(c(0.025,0.975))
percentile.interval<- quantile(1/lambda.star,c(0.025,0.975))
intervals.beta.hat <- as.data.frame(rbind(normal.interval,percentile.interval,bca.interval))
colnames(intervals.beta.hat) <- c("lower","upper")
intervals.beta.hat$length <- intervals.beta.hat$upper - intervals.beta.hat$lower
intervals.beta.hat$center <- intervals.beta.hat$lower + intervals.beta.hat$length/2
bias.beta.hat <- mean(1/lambda.star) - beta.hat
print(bias.beta.hat)
# [1] 0.1852602
print(intervals.beta.hat)
#                        lower    upper   length   center
# normal.interval     34.17928 181.9874 147.8081 108.0833
# percentile.interval 46.07917 191.4333 145.3542 118.7562
# bca.interval        56.33333 224.1677 167.8344 140.2505
hist(1/lambda.star, breaks = 100, main = "Bootstrap distribution of beta.hat", xlim = c(0,max(1/lambda.star)), xlab = "")