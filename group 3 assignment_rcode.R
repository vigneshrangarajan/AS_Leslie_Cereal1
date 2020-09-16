mydata=read.csv(file.choose(), header=TRUE)
attach(mydata)
mydata2<-mydata[,2:26]

mean<-colMeans(x=mydata2, na.rm = TRUE)
print(mean,digits=2)
plot(mean)
stddev<-apply(mydata2,2,sd)
print(stddev, digits = 2)
plot(stddev)
ske<-apply(mydata2,2,skew)
print(ske, digits=2)
plot(ske)


install.packages("corrplot")
library(corrplot)
corrplot(cor(mydata2), method = "color", type="lower") # plot corelation matrix

library(nFactors) #compute eigen vector and eigen values
ev = eigen(cor(mydata2))
EigenValue=ev$values
EigenValue
ev
Factor<-c(1:25)
Scree=data.frame(Factor,EigenValue)
plot(Scree,main="Scree Plot", col="Blue",ylim=c(0,10)) # plot scree plot
lines(Scree,col="Red")
abline(h=1, col="Green")

library(psych) # PCA
Unrotate=principal(mydata2, nfactors=5, rotate="none")
print(Unrotate,digits=3)
UnrotatedProfile=plot(Unrotate,row.names(Unrotate$loadings))
Rotate=principal(mydata2,nfactors=5,rotate="varimax")
print(Rotate,digits=3)
RotatedProfile=plot(Rotate,row.names(Rotate$loadings),cex=1.0)
Rotate$scores #scores
View(Rotate$scores)

