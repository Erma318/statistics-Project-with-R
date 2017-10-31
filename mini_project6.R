#read the data from prostate_cancer.csv
pc<- read.table("prostate_cancer.csv", header = TRUE, sep = ",")

#vesinv is not quantitative predictors
windows();plot(pc$cancervol,pc$psa,main='Scatterplots of PSA level against Cancervol'
               ,xlab='Cancervol (cc)',ylab='PSA (mg/ml)')
windows();plot(pc$weight,pc$psa,main='Scatterplots of PSA level against Weight'
               ,xlab='Weight (gm)',ylab='PSA (mg/ml)')
windows();plot(pc$age,pc$psa,main='Scatterplots of PSA level against Age'
               ,xlab='Age (years)',ylab='PSA (mg/ml)')
windows();plot(pc$benpros,pc$psa,main='Scatterplots of PSA level against Benpros'
               ,xlab='Benpros (cm^2)',ylab='PSA (mg/ml)')
windows();plot(pc$capspen,pc$psa,main='Scatterplots of PSA level against Capsen'
               ,xlab='Capspen (cm)',ylab='PSA (mg/ml)')
windows();plot(pc$gleason,pc$psa,main='Scatterplots of PSA level against Gleason'
               ,xlab='Gleason',ylab='PSA (mg/ml)')


#according to the scatterplots to choose cancervol as the most 
#effectively to predict PSA level

#get the simple linear model and do the test of significance   
lm(pc$psa~pc$cancervol)
psa.reg<-lm(pc$psa~pc$cancervol)
anova(psa.reg)

#use plots to see if the model is met with the three assumption
#residual plot
windows();plot(fitted(psa.reg),resid(psa.reg));abline(h=0)

#qq plot
windows();qqnorm(resid(psa.reg));qqline(resid(psa.reg))

#time series plot of residuals
windows();plot(resid(psa.reg),type="l");abline(h=0)

#isn't satisfied with the first two assumption 
#do log transformation of cancervol
logpsa<-log(pc$psa)
logcancervol<-log(pc$cancervol)
windows();plot(logcancervol,logpsa,main='Scatterplots of PSA level against log of Cancervol'
               ,xlab='log of Cancervol (cc)',ylab='PSA (mg/ml)');abline(reg=)

#get the simple linear model after the log transformation and do the test of significance 
lm(logpsa~logcancervol)
logpsa.reg<-lm(logpsa~logcancervol)
anova(logpsa.reg)

#use plots to see if the model after the log transformation is met with the three assumption
#residual plot
windows();plot(fitted(logpsa.reg),resid(logpsa.reg));abline(h=0)

#qq plot
windows();qqnorm(resid(logpsa.reg));qqline(resid(logpsa.reg))

#time series plot of residuals
windows();plot(resid(logpsa.reg),type="l");abline(h=0)

#Use the final model to predict the PSA level for a patient whose
#predictor variable value is at the sample median of the variable.
logcancervol.new<-data.frame(logcancervol=log(median(pc$cancervol)))
logPSAlevel<-predict(logpsa.reg,newdata=logcancervol.new)
logPSAlevel
PSAlevel=exp(logPSAlevel)
PSAlevel
