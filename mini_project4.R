
# PART I
# Construct the our own bootstrap function to get parameter 
# estimate (90th population percentile), estimated bias and
# standard error, the three 95% bootstrap confidence 
# intervals for the parameter.

bootstrap<-function(x)
{
  # get the 90th percentile of every set of resample data
  resample.t<-replicate(999, quantile(sample(x, length(x),
            replace = TRUE, prob = NULL), .9, names = FALSE))
  # get the 90th percentile of the original data
  t0<-quantile(x, .9, names = FALSE)
  # calculate the bias
  bias<-mean(resample.t)-t0
  # calculate the standard error of the 999 90th percentile of
  # every set of resample data
  sderror<-sd(resample.t)
  # use 95% normal approximation method to calculate the bootstrap CI
  normal<-c(t0-bias-qnorm(0.975)*sderror, 
            t0-bias-qnorm(0.025)*sderror)
  # use 95% percentile bootstrap method to calculate the bootstrap CI
  percentileBootstrap<-sort(resample.t)[c(25,975)]
  # use 95% basic bootstrap method to calculate the bootstrap CI
  basic<-c(2*t0-percentileBootstrap[2],
           2*t0-percentileBootstrap[1])
  # print out all the outcome
  cat("(1) parameter estimate:",t0,"\n")
  cat("(2) estimated bias:",bias,"\n")
  cat("(3) standard error:",sderror,"\n")
  cat("(4) 95% normal approximation method:",normal,"\n")
  cat("(5) 95% percentile bootstrap method:",percentileBootstrap,"\n")
  cat("(6) 95% basic bootstrap method:",basic,"\n")
}


# PART II
# Then we use the boot package of R to do the calculation to 
# verify our outcomes

library(boot)
cpu<-scan(file="cputime.txt")
bootstrap(cpu)
# set the function to get 90th percentile
P <-function(x,indices)
{
  result<-quantile(x[indices], .9,numes=FALSE)
  return(result)
}
# use boot function to get the parameter estimate (90th 
# population percentile), estimated bias and standard error
P.boot <-boot(cpu, P, R=999, sim="ordinary", stype="i")
print(P.boot)
# use boot.ci function to get the three 95% bootstrap 
# confidenceintervals
print(boot.ci(P.boot))
