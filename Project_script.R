#Importing Libraries
rm(list=ls())
library(rio)
library(moments)
install.packages('corrplot')
library(corrplot)

#Improting Data set
fish=import("C:\\Users\\saisu\\Downloads\\Fish_Data.xlsx")

#Modifying Columns
colnames(fish)=tolower(make.names(colnames(fish)))

#Attaching the dataset
attach(fish)

#Setting seed
set.seed(987654321)

#Summary of the Dataset
str(fish)

#MUltiple regression with all the variables 
fish_reg = lm(weight~length+height+width,fish)
summary(fish_reg)

#Linear regression of weight and length 
sreg1=lm(weight~length, fish)
summary(sreg1)

#Linear regression of weight and height 
sreg2=lm(weight~height, fish)
summary(sreg2)

#Linear regression of weight and width 
sreg3=lm(weight~width, fish)
summary(sreg3)

#Multiple regression of weigth with length and height
Multiple_reg1 = lm(weight~length+height,fish)
summary(Multiple_reg1)

#Multiple regression of weigth with length and height
Multiple_reg1 = lm(weight~length+height,fish)
summary(Multiple_reg1)

#Multiple regression of weigth with length and width
Multiple_reg2 = lm(weight~length+width,fish)
summary(Multiple_reg2)

#Multiple regression of weigth with height and width
Multiple_reg3 = lm(weight~height+width,fish)
summary(Multiple_reg3)

#Line assumptions

par(mfrow=c(2,2))

plot(fish$weight, fish_reg$fitted.values, col='black', pch=19, 
     main = "Actual vs Fitted")
abline(0,1,lwd=3,col='red')

qqnorm(fish_reg$residuals,pch=19,col='blue',main="Normality Plot of the Residuals")
qqline(fish_reg$residuals,col='red',lwd=3)

hist(fish_reg$residuals, col='red', probability = TRUE)
curve(dnorm(x,mean(fish_reg$residuals), sd(fish_reg$residuals)),
      from=min(fish_reg$residuals), to =max(fish_reg$residuals),col='black', add= TRUE, lwd=3 )

plot(fish_reg$fitted.values,fish_reg$residuals,pch=19,
     main="Residuals")
abline(0,0,col="red",lwd=3)
pairs(fish, pch=19)
cor(fish)
round(cor(fish),3)
continuous_variables_2=cor(fish)
par(mfrow=c(1,2))
corrplot(continuous_variables_2, method='number')
corrplot(continuous_variables_2, method="ellipse")

#Multiple regression with interaction terms
rm_interaction=lm(weight~width+height+(width*height), fish)
summary

#prediction
estimated_weight=data.frame( length=='1',height>=9 )
rmpred=lm(weight~length+height+(length*height), fish)
predict(rmpred, estimated_weight, interval = "predict")
summary(rmpred)
