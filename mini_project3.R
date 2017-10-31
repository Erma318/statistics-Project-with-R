#Get the tree data from the csv file
tree <- read.table("trees.csv", header = TRUE, sep = ",")

#Make a histogram and a boxplot of each variables
windows();hist(tree$Girth,xlab='Girth (inches)',main='Histogram of Girth',freq = F)
windows();boxplot(tree$Girth,ylab='Girth (inches)',main='Boxplot of Girth',range = 0)
windows();hist(tree$Height,xlab='Height (feet)',main='Histogram of Height',freq = F)
windows();boxplot(tree$Height,ylab='Height (feet)',main='Boxplot of Height',range = 0)
windows();hist(tree$Volume,xlab='Volume (cubic feet)',main='Histogram of Volume',freq = F)
windows();boxplot(tree$Volume,ylab='Volume (cubic feet)',main='Boxplot of Volume',range = 0)

#Calculate the 5-number summary of each variables
cat('5-number summary of Girth\n')
print(quantile(tree$Girth))
cat('5-number summary of Height\n')
print(quantile(tree$Height))
cat('5-number summary of Volume\n')
print(quantile(tree$Volume))

#Make a normal Q-Q plot of each variables
windows();qqnorm(tree$Girth,main='QQplot of Girth')
qqline(tree$Girth)
windows();qqnorm(tree$Height,main='QQplot of Height')
qqline(tree$Height)
windows();qqnorm(tree$Volume,main='QQplot of Volume')
qqline(tree$Volume)

#Make a scatterplot for each pair of variables
windows();pairs(tree,main='Scatterplot of each pair of variables')

#Calculate the correlation for each pair of variables
cat('Correlation between Height and Girth\n')
print(cor(tree$Height,tree$Girth))
cat('Correlation between Volume and Girth\n')
print(cor(tree$Volume,tree$Girth))
cat('Correlation between Height and Volume\n')
print(cor(tree$Height,tree$Volume))

#Perform a natural log transformation of the variables
#Then repeat all the calculations using the data after log transformation
tree<-log(tree)

#Make a histogram and a boxplot of each variables after log transformation
windows();hist(tree$Girth,xlab='log Girth (inches)',main='Histogram of log Girth',freq = F)
windows();boxplot(tree$Girth,ylab='log Girth (inches)',main='Boxplot of log Girth',range = 0)
windows();hist(tree$Height,xlab='log Height (feet)',main='Histogram of log Height',freq = F)
windows();boxplot(tree$Height,ylab='log Height (feet)',main='Boxplot of log Height',range = 0)
windows();hist(tree$Volume,xlab='log Volume (cubic feet)',main='Histogram of log Volume',freq = F)
windows();boxplot(tree$Volume,ylab='log Volume (cubic feet)',main='Boxplot of log Volume',range = 0)

#Calculate the 5-number summary of each variables after log transformation
cat('5-number summary of log Girth\n')
print(quantile(tree$Girth))
cat('5-number summary of log Height\n')
print(quantile(tree$Height))
cat('5-number summary of log Volume\n')
print(quantile(tree$Volume))

#Make a normal Q-Q plot of each variables after log transformation
windows();qqnorm(tree$Girth,main='QQplot of log Girth')
qqline(tree$Girth)
windows();qqnorm(tree$Height,main='QQplot of log Height')
qqline(tree$Height)
windows();qqnorm(tree$Volume,main='QQplot of log Volume')
qqline(tree$Volume)

#Make a scatterplot for each pair of variables after log transformation
windows();pairs(tree,main='Scatterplot of each pair of variables after log transformation')

#Calculate the correlation for each pair of variables after log transformation
cat('Correlation between Height and Girth after log transformation\n')
print(cor(tree$Height,tree$Girth))
cat('Correlation between Volume and Girth after log transformation\n')
print(cor(tree$Volume,tree$Girth))
cat('Correlation between Height and Volume after log transformation\n')
print(cor(tree$Height,tree$Volume))