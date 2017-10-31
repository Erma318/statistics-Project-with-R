#1.simulate the three block execution times
x<-rexp(3,0.2) 
#get the execution time of the whole program
max(x) 
#2.repeat the previous two steps 10,000 times
sim.10k<-replicate(10000,max(rexp(3,0.2))) 
#3.make a histogram of the previous 10,000 draws
hist(sim.10k,ylim=c(0,0.1),prob=TRUE)
#superimpose the density function abtained in the exercise 4.6
curve(0.6*(1-exp(-0.2*x))^2*exp(-0.2*x),0,60,add=TRUE)
#4.estimate the mean
mean(sim.10k)
#5.get the probability that the entire program takes more than 20 mins to compile
sum(sim.10k>20)/10000
qqnorm(sim.10k)

