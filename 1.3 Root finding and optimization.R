#first observation, Newton-Raphson
f <- function(a) {
  10^(-a) - 0.502
}
f. <- function(a) {
  -log(10) * 10^(-a)
}
nw <- function(f, f., steps, start) {
  x <- start - f(start)/f.(start)
  for(i in 1:(steps - 1)) {
    x <- x - f(x)/f.(x)
    if(!is.na(f(x))) {
      if(f(x) == 0) {
        return(x)
      }
    }
  }
  return(c(f(x), x, as.integer(i)))
}
alphanw <- nw(f,f.,100,1) #0.2992963
#First observation, Method of Moments
alpha <- -log(0.502)/log(10) #0.2992963

#Both observations, optimization of difference
g <- function(a){
  abs(10^(-a)+40^(-a)-0.803)
}

opt <- optimize(g,interval = c(0,1))
alpha2 <- opt$minimum # 0.3399743
#Both observations, log-likelihood
alpha2ll <- 2/(log(10)+log(40)) #0.3338082

