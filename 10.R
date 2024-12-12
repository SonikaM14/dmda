data("iris")
install.packages("caret")
install.packages("C50")
library(caret)
library(C50)
set.seed(7)
inTraininglocal<-createDataPartition(iris$Species,p=.70,list=F)
training<-iris[inTraininglocal,]
testing<-iris[-inTraininglocal,]
model<-C5.0(Species~.,data=training)
summary(model)
pred<-predict.C5.0(model,testing[,-5])
a<-table(testing$Species,pred)
sum(diag(a))/sum(a)
plot(model)



snowfall <- c(790,1170.8,860.1,1330.6,630.4,911.5,
              683.5,996.6,783.2,982,881.8,1021)
snowfall_timeseries <- ts(snowfall,start=c(2013,1),frequency = 12)
print(snowfall_timeseries)
png(file = "snowfall.png")
plot(snowfall_timeseries)
dev.off()
model <- lm(snowfall_timeseries ~ time(snowfall_timeseries))
png(file = "snowfall_with_trend1.png")
plot(snowfall_timeseries,main="Snowfall Over Time with Trend Line:",
     xlab="Time",ylab="Snowfall",col="blue")
abline(model,col="red")
dev.off()




xvalues<-c(1.6,2.1,2,2.23,3.71,3.25,3.4,3.86,1.19,2.21)
yvalues<-c(5.19,7.43,6.94,8.11,18.75,14.88,16.06,19.12,3.21,7.58)
png(file="nls3.png")
plot(xvalues,yvalues)
model<-nls(yvalues~b1*xvalues^2+b2,start=list(b1=1,b2=3))
new.data<-data.frame(xvalues=seq(min(xvalues),max(xvalues),len=10))
lines(new.data$xvalues,predict(model,newdata=new.data))
dev.off()
print(sum(resid(model)^2))
print(confint(model))
