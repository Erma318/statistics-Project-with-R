#Exercise 1
cat("Exercise 1\n")
#read the bp.txt file
bp<-read.table("bp.txt", fill = FALSE, sep = "",header = T)

#a.make boxplots of these two data
windows();boxplot(bp$armsys,ylab='armsys BP (mmHg)',main='Boxplot of armsys BP',range = 0)
windows();boxplot(bp$fingsys,ylab='fingsys BP (mmHg)',main='Boxplot of fingsys BP',range = 0)

#b.make histograms of these two data 
windows();hist(bp$armsys,xlab='armsys BP (mmHg)',main='Histogram of armsys BP',freq = F)
windows();hist(bp$fingsys,xlab='fingsys BP (mmHg)',main='Histogram of fingys BP',freq = F)
#  make qqplot of these two data
windows();qqnorm(bp$armsys,main='QQplot of armsys BP');qqline(bp$armsys)
windows();qqnorm(bp$fingsys,main='QQplot of fingsys BP');qqline(bp$fingsys)

#c.Construct an appropriate 95% confidence interval for the difference
#  in the means of these two data
deltabp<-bp$armsys-bp$fingsys
sd<-sd(deltabp)
mean<-mean(deltabp)
CI<-(mean+c(-1,1)*qnorm(1-0.025)*sd/sqrt(length(deltabp)))
cat("95% confidence interval for the difference is: ",CI,"\n")

#d.Perform an appropriate 5% level test to see if there is any difference
#  in the means of these two data
#H0:armsys-fingsys=0
#H1:armsys-fingsys!=0
cat("5% level test is:\n")
print(t.test(deltabp, alternative = "two.sided", mu = 0, conf.level = (1 - 0.05)))


#Exercise 2
cat("\nExercise 2\n")
#a.set up the null and alternative hypotheses
#H0:mean=10
#H1:mean>10

#b.choose t-test

#c.compute the observed value of the test statistic
mean<-9.02
s<-2.22
tobs<-(mean-10)/(s/sqrt(20))
cat("the observed value of the test statistic is: ",tobs,"\n")

#d.compute the p-value of the test using the usual way
pvalue<-1-pt(tobs,19)
cat("the p-value of the test using the usual way is: ",pvalue,"\n")

#e.estimate the p-value of the test using Monte Carlo simulation
MC<-rt(999,19)
pvalueofMC<-length(which(MC>tobs))/999
cat("the p-value of the test using Monte Carlo simulation is: ",pvalueofMC,"\n")

#Exercise 3
cat("\n\nExercise 3\n")
#a.construct an appropriate 95% confidence interval for the difference
#  in mean credit limits 
CI<-((2635-2887)+c(-1,1)*(qnorm(1-0.025)*sqrt(365^2/400+412^2/500)))
cat("95% confidence interval for the difference is: ",CI,"\n")

#b.Perform an appropriate 5% level test to see if the mean credit limit
#  of all credit cards issued in May 2011 is greater than the same in January 2011.
#delta=Jan-May
#H0: delta=0
#H1: delta<0
tobs<-(2635-2887)/sqrt(365^2/400+412^2/500)
dof<-(365^2/400+412^2/500)^2/(365^4/(400^2*(400-1))+412^4/(500^2*(500-1)))
pvalue<-pt(tobs,dof)
cat("the p-value of the test is: ",pvalue,"\n")

