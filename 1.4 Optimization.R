region1 <- data.frame(matrix(c(1,0,2.5,41,1.389,
                             2,2.5,7.5, 48, 4.661,
                             3,7.5, 12.5 ,24 ,9.991,
                             4,12.5, 17.5 ,18 ,15.482,
                             5,17.5 ,22.5 ,15 ,20.232,
                             6,22.5 ,32.5 ,14 ,26.616,
                             7,32.5, 47.5 ,16 ,40.278,
                             8,47.5 ,67.5 ,12 ,56.414,
                             9,67.5, 87.5 ,6 ,74.985,
                             10,87.5, 125, 11 ,106.851,
                             11,125 ,225 ,5 ,184.735,
                             12,225 ,300 ,4 ,264.025,
                             13,300,0,3,300),ncol = 5,nrow = 13,byrow = TRUE))
colnames(region1) <- c('Group','Lower','Upper','Frequency','Average')

mean1 <- weighted.mean(region1$Average,region1$Frequency)
l1 <- function(k) {
  log(k) - digamma(k) - log(mean1) + sum(log(region1$Average)*region1$Frequency)/sum(region1$Frequency)
}
l. <- function(k) {
  1/k - trigamma(k)
}
k1hat <- nw(l1,l.,100,1) #0.5858351
theta1hat <- mean1/k1hat #57.43664

region2 <- data.frame((matrix(c(
  1, 0 ,2.5, 101,
  2, 2.5, 7.5, 132,
  3, 7.5, 12.5, 61,
  4, 12.5, 17.5, 50,
  5, 17.5, 22.5, 29,
  6, 22.5, 32.5, 50,
  7, 32.5, 47.5, 28,
  8, 47.5, 67.5, 40,
  9, 67.5, 87.5, 22,
  10, 87.5, 125, 27,
  11, 125, 175, 19,
  12, 175, 225, 6,
  13, 225, 325, 10,
  14, 325, 500, 4,
  15, 500,0, 5
),ncol=4,nrow = 15, byrow=TRUE)))
colnames(region2) <- c('Group','Lower','Upper','Frequency')

par(mfrow=c(1,2))
barplot(region1$Frequency, space = 0, axes = F, main = "First Region")
axis(side = 2, pos = -0.2) 
axis(side = 1, labels = FALSE)
axis(side = 1, at = seq_along(region1$Frequency) - 1,  tick = FALSE, labels = region1$Lower)
barplot(region2$Frequency, space = 0, axes = F, main = "Second Region")
axis(side = 2, pos = -0.2) 
axis(side = 1, labels = FALSE)
axis(side = 1, at = seq_along(region2$Frequency) - 1,  tick = FALSE, labels = region2$Lower)

logll <- function(p,dt){
  shape <- p[1]
  rate <- p[2]
  n <- nrow(dt)
  sum(dt$Frequency[1:n-1]*log(pgamma(dt$Upper[1:n-1],shape=shape,rate=rate)-pgamma(dt$Lower[1:n-1],shape=shape,rate=rate)),log((1-pgamma(dt$Lower[n],shape=shape,rate=rate)))*dt$Frequency[n])
}

logll1 <- optim(c(shape=0.5,rate=0.02),logll,control = list(fnscale=-1),dt=region1,hessian=TRUE,
            method='BFGS')
logll2 <- optim(c(shape=0.5,rate=0.02),logll,control = list(fnscale=-1),dt=region2,hessian=TRUE,
            method='BFGS')

region12 <- region2
region12$Frequency <- region12$Frequency + c(region1$Frequency[-nrow(data3)],7,0,0)

logll12 <- optim(c(shape=0.5,rate=0.02),logll,control = list(fnscale=-1),dt=region12,hessian=TRUE,
                method='BFGS')
W <- -2*(logll12$value - logll1$value - logll2$value) #32.18625
p <- pchisq(W,2,lower.tail = F) #1.025283e-07