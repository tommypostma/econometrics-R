table1 <- cbind(lower=c(0,2.5,7.5,12.5,17.5,22.5,32.5,47.5,67.5,87.5,125,225,300),
                   upper=c(2.5,7.5,12.5,17.5,22.5,32.5,47.5,67.5,87.5,125,225,300,Inf),
                   freq=c(41,48,24,18,15,14,16,12,6,11,5,4,3))
loglik.freq <- function(p,data) {
  lower <- data[,1]
  upper <- data[,2]
  frequency <- data[,3]
  ll<-frequency*log(ifelse(upper<Inf,pgamma(upper,p[1],p[2]),1)-pgamma(lower,p[1],p[2]))
  return(sum(ll))
}
p0 <- c(alpha=0.5,beta=0.02)
ll.model.table1 <- optim(p0,loglik.freq,control = list(fnscale = -1), data = table1, hessian = T)
print(ll.model.table1)
# $par
# alpha      beta
# 0.46853403 0.01395663 
# 
# $value
# [1] -507.8422
# 
# $counts
# function gradient 
# 53       NA 
# 
# $convergence
# [1] 0
# 
# $message
# NULL
# 
# $hessian
#           alpha       beta
# shape -1003.118   15402.75
# rate  15402.755 -506163.02

q.hat <- qgamma(0.995,ll.model.table1$par[1],ll.model.table1$par[2])
print(q.hat)
#[1] 274.9793
D.qgamma <- function(p,alpha,beta,epsilon) {
  d.qgamma.d.x <- 1/dgamma(qgamma(p,alpha,beta),alpha,beta)
  d.qgamma.d.alpha <- (qgamma(p,alpha + epsilon,beta) - qgamma(p,alpha - epsilon,beta))/(2*epsilon)
  d.qgamma.d.beta <- (qgamma(p,alpha,beta + epsilon) - qgamma(p,alpha,beta - epsilon))/(2*epsilon)
  return(c(d.qgamma.d.x,d.qgamma.d.alpha,d.qgamma.d.beta))
}
D <- D.qgamma(0.995,ll.model.table1$par[1],ll.model.table1$par[2],1e-6)
print(D)
#[1]  12849.1626    235.7635 -19702.4185
var.q.hat <- t(D[2:3]) %*% solve(-ll.model.table1$hessian) %*% D[2:3]
delta.interval <- q.hat + sqrt(var.q.hat)*qnorm((c(0.025,0.975)))
print(delta.interval)
#[1] 212.5502 337.4085

loglik <- function(p,x) {
  ll <- dgamma(x,p[1],p[2],log = T)
  return(sum(ll))
}
B <- 9999
q995 <- rep(NA,B)
n <- sum(table1[,"freq"])
set.seed(10)
for(b in 1:B) {
  x.b <- rgamma(n,ll.model.table1$par[1],ll.model.table1$par[2])
  ll.model.x.b <- optim(p0,loglik,control = list(fnscale = -1), x = x.b)
  if(ll.model.x.b$convergence == 0) {
    q995[b] <- qgamma(.995,ll.model.x.b$par[1],ll.model.x.b$par[2])
  }
}
bootstrap.interval <- quantile(q995, c(0.025,0.975))
print(bootstrap.interval)
#    2.5%    97.5% 
#218.3010 336.4853  
