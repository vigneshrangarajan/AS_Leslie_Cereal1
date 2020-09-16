LeslieSalt=read.csv(file.choose(), header=TRUE)
attach(LeslieSalt)
View(LeslieSalt)

LeslieSalt$County <- as.factor(LeslieSalt$County)
levels(LeslieSalt$County) <- c("San Mateo", "Santa Clara")
LeslieSalt$Flood <- as.factor(LeslieSalt$Flood)
levels(LeslieSalt$Flood) <- c("No", "Yes")

str(LeslieSalt)
summary(LeslieSalt)
PriceMean<-mean(LeslieSalt$Price)
PriceSD<-sd(LeslieSalt$Price)
PriceMean
PriceSD
Pricebox<-boxplot(LeslieSalt$Price)

LeslieSalt.clean<- LeslieSalt[-26,]
LeslieSalt.clean
Pricebox.clean<-boxplot(LeslieSalt.clean$Price)
PriceMean.clean<-mean(LeslieSalt.clean$Price)
PriceSD.clean<-sd(LeslieSalt.clean$Price)
PriceMean.clean
PriceSD.clean


library(dplyr)
LeslieSalt.clean.matrix <- as.matrix(dplyr::select_if(LeslieSalt.clean, is.numeric))
corrplot(cor(LeslieSalt.clean.matrix), method = "color", type="lower") 



#Running Regression model with Price as dependent variable and with rest of all 
model1<-lm(Price ~County + Size + Elevation+ Sewer+ Date+ Flood+Distance, data = LeslieSalt.clean)
summary(model1)
shapiro.test(model1$residuals)

model2<-lm(Price ~Elevation+ Date+ Flood, data = LeslieSalt.clean)
summary(model2)
shapiro.test(model2$residuals)

model3<-lm(Price ~Elevation+ Date+ Flood+County + Sewer, data = LeslieSalt.clean)
summary(model3)
shapiro.test(model3$residuals)

model4<-lm(log(Price) ~Elevation+ Date+ Flood+County + Sewer, data = LeslieSalt.clean)
summary(model4)
shapiro.test(model4$residuals)


confint(model3)
newdata<-data.frame(County="Santa Clara", Size=248.1, Elevation=0, Sewer=0, Date=3, Flood="No", Distance=0)
prediction3<-predict(model3,newdata, interval="confidence")
prediction3

#Comparision<-data.frame(Actual=log(LeslieSalt.clean$Price),Predicted=predict(model5))
#plot(Comparision)
#factors<-c(1:30)
#plot(factors,log(LeslieSalt.clean$Price))
#plot(factors,predict(model5))
#prediction.final<-exp(prediction5)
#prediction.final
