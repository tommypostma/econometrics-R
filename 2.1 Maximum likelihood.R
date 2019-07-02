load("C:/Users/tommy/OneDrive/EOR/3/3.1a/Numerical Methods/Assignments/2/problem1.Rdata")
x.table$bounds <- as.character(x.table$bounds)
x.table$lower <- rep(NA, 10)
x.table$upper <- rep(NA, 10)
library("filesstrings")
x.table[,3:4] <- t(as.data.frame(extract_numbers(x.table$bounds, decimals = T, negs = T)))
loglikfreq <- function(p, data) {
  mean <- p[1]
  sd <- p[2]
  frequencies <- data[,1]
  lower <- data[,2]
  upper <- data[,3]
  ll <- frequencies*log(pnorm(upper,mean,sd) - pnorm(lower,mean,sd))
  return(sum(ll))
}
p0 <- c(mean=0,sd=2)
ll.model.table <- optim(p0,loglikfreq,hessian = T,data=x.table[,2:4],control = list(fnscale = -1))
print(ll.model.table)
# $par
# mean        sd 
# 0.4725903 1.2494289 
# 
# $value
# [1] -2306.632
# 
# $counts
# function gradient 
# 57       NA 
# 
# $convergence
# [1] 0
# 
# $message
# NULL
# 
# $hessian
# mean         sd
# mean -619.0362    14.5043
# sd     14.5043 -1020.9998
loglik <- function(p,obs) {
  mean <- p[1]
  sd <- p[2]
  ll <- dnorm(x, mean, sd, log = T)
  return(sum(ll))
}
ll.model.vector <- optim(p0,loglik,hessian = T, obs = x,control = list(fnscale = -1))
print(ll.model.vector)
# $par
# mean        sd 
# 0.5117507 1.2392571 
# 
# $value
# [1] -1633.461
# 
# $counts
# function gradient 
# 51       NA 
# 
# $convergence
# [1] 0
# 
# $message
# NULL
# 
# $hessian
# mean            sd
# mean -651.1441580    -0.1974205
# sd     -0.1974205 -1302.3350464
table <- sqrt(diag(solve(-ll.model.table$hessian,diag(c(1,1)))))
vector <- sqrt(diag(solve(-ll.model.vector$hessian,diag(c(1,1)))))
samplingvariances <- data.frame(rbind(table,vector))
colnames(samplingvariances) <- c("mean", "sd")
print(samplingvariances)
# mean         sd
# table  0.04019891 0.03130109
# vector 0.03918875 0.02771014