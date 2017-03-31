rm(list = ls())
blood=read.table("D:/U of A/Courses/590/Final-Report/data.txt",header=T,sep="	")
blood=data.frame(blood)
library(glmnet)
library(sm)

blood.low=blood[1:250,]
x1=blood.low[,c(2:501)]
y1=blood.low[,1]
x1=data.matrix(x1)
y1=as.numeric(y1)
attach(blood.low)
###############################################
par(mfrow=c(2,2))
boxplot(sbp~gender,xlab="Gender",ylab="Systolic Blood Pressure")
boxplot(sbp~married,xlab="Married",ylab="Systolic Blood Pressure")
boxplot(sbp~smoke,xlab="smoke",ylab="Systolic Blood Pressure")
boxplot(sbp~exercise,xlab="Exercise",ylab="Systolic Blood Pressure")
boxplot(sbp~overwt,xlab="overwt",ylab="Systolic Blood Pressure")
boxplot(sbp~race,xlab="race",ylab="Systolic Blood Pressure")
boxplot(sbp~alcohol,xlab="alcohol",ylab="Systolic Blood Pressure")
boxplot(sbp~trt,xlab="treatment",ylab="Systolic Blood Pressure")
boxplot(sbp~salt,xlab="Salt (NaCl) Intake Level",ylab="Systolic Blood Pressure")
boxplot(sbp~chldbear,xlab="Childbearing Potential",ylab="Systolic Blood Pressure")
boxplot(sbp~income,xlab="Income Level",ylab="Systolic Blood Pressure")
boxplot(sbp~educatn,xlab="Education Level",ylab="Systolic Blood Pressure")
plot(bmi,sbp)
plot(age,sbp)
plot(weight,sbp)
plot(height,sbp)
t.test(sbp~smoke)
summary(aov(sbp~exercise))
summary(aov(sbp~overwt))
summary(aov(sbp~race))
summary(aov(sbp~alcohol))
t.test(sbp~trt)

###################################################
library(MASS)
x1=blood.low[,c(2:18)]
y1=blood.low[,1]
x1=data.matrix(x1)
y1=as.numeric(y1)
fit.lin=lm(y1~x1)
summary(fit.lin)
#fit.lin <- xtable(fit.lin)
#print(fit.lin,print.results = FALSE)
plot(y1)
hist(y1)
summary(y1)



########Chech the model######
library(car)
plot(fit.lin)
summary(influence.measures(fit.lin))
crPlots(fit.lin,terms = ~.)
studres(fit.lin)
fitted=fit.lin$fitted.values
plot(fitted,studres(fit.lin),xlab="Fitted values",ylab="Studentized Residual")
str(fit.lin)
plot(x1[,5],fit.lin$ residuals, main = "Residuals va Age", xlab = "Age",ylab = "Residuals")
plot(x1[,6],fit.lin$ residuals,main = "Residuals va Weight", xlab = "Weight",ylab = "Residuals")
plot(x1[,7],fit.lin$ residuals,main = "Residuals va Hight", xlab = "Hight",ylab = "Residuals")
###########omite the influence#######
x.o=blood.low[-c(4,8,53,107,125,165,187,203,241,243,247),c(2:18)]
y.o=blood.low[-c(4,8,53,107,125,165,187,203,241,243,247),1]
x.o=data.matrix(x.o)
y.o=as.numeric(y.o)
fit.lin.o=lm(y.o~x.o)
summary(fit.lin.o)
plot(fit.lin.o)
crPlots(fit.lin.o,terms = ~.)
studres(fit.lin.o)
fitted=fit.lin.o$fitted.values
plot(fitted,studres(fit.lin.o),xlab="Fitted values",ylab="Studentized Residual")


###################transformation####################
rm(list = ls())
blood=read.table("D:/U of A/Courses/590/Final-Report/data.txt",header=T,sep="	")
blood=data.frame(blood)
library(glmnet)
library(sm)
blood.low=blood[1:250,]
x1=blood.low[-c(4,8,53,107,125,165,187,203,241,243,247),c(2:18)]
y1=blood.low[-c(4,8,53,107,125,165,187,203,241,243,247),1]

x1=data.matrix(x1)
y1=as.numeric(y1)
summary(y1)

y1=141-y1
y1=log(y1)

fit.lin=lm(y1~x1)
summary(fit.lin)
##############Chech the model######
library(car)
plot(fit.lin)
crPlots(fit.lin,terms = ~.)
studres(fit.lin)
fitted=fit.lin$fitted.values
plot(fitted,studres(fit.lin),xlab="Fitted values",ylab="Studentized Residual")
plot(x1[,5],fit.lin$ residuals, main = "Residuals va Age", xlab = "Age",ylab = "Residuals")
plot(x1[,6],fit.lin$ residuals,main = "Residuals va Weight", xlab = "Weight",ylab = "Residuals")
plot(x1[,7],fit.lin$ residuals,main = "Residuals va Hight", xlab = "Hight",ylab = "Residuals")
#############################Model selection#############
data.v=data.frame(y1,x1)
attach(data.v)
step(lm(y1~gender+married+smoke+age+weight+height+overwt+race+alcohol+
          trt+bmi+stress+salt+chldbear+income+educatn),direction = "both",data=data.v)

############################################################################
###############all into regularization############
rm(list = ls())
blood=read.table("D:/U of A/Courses/590/Final-Report/data.txt",header=T,sep="	")
blood=data.frame(blood)
library(glmnet)
#library(sm)
#library(hydroGOF)
#blood.low=blood[1:250,]
#x1=blood.low[-c(4,8,53,107,125,165,187,203,241,243,247),c(2:501)]
#y1=blood.low[-c(4,8,53,107,125,165,187,203,241,243,247),1]
#x1=data.matrix(x1)
#y1=as.numeric(y1)

blood.low=blood[1:250,]
x1=blood.low[,c(2:501)]
y1=blood.low[,1]
x1=data.matrix(x1)
y1=as.numeric(y1)
attach(blood.low)
#######################Elastic net alpha=0.6##################################
par(mfrow=c(1,2))
p.fac = rep(1, 501)
p.fac[c(1:17)] = 0
fit.gl.pen=glmnet(x1,y1, penalty.factor = p.fac,alpha = 0.6)
summary(fit.gl.pen)
#str(fit.gl.pen)
plot(fit.gl.pen, xvar = "lambda", label = TRUE)
cvfit.pen=cv.glmnet(x1, y1,penalty.factor = p.fac,alpha = 0.6)
plot(cvfit.pen)
cvfit.pen$lambda.min
newy=predict(cvfit.pen, x1, s = "lambda.min",penalty.factor = p.fac)
plot(newy)
points(y1,col="red",pch=8)
coef(cvfit.pen,s="lambda.min")
mse = sum((newy - y1) ^ 2) / length(y1)
mse

##########################Elastic without penalty#############################################
par(mfrow=c(1,2))

fit.gl.pen=glmnet(x1,y1,alpha = 0.6)
summary(fit.gl.pen)
#str(fit.gl.pen)
plot(fit.gl.pen, xvar = "lambda", label = TRUE)
cvfit.pen=cv.glmnet(x1, y1,alpha = 0.6)
plot(cvfit.pen)
cvfit.pen$lambda.min
newy=predict(cvfit.pen, x1, s = "lambda.min")
plot(newy)
points(y1,col="red",pch=8)
coef(cvfit.pen,s="lambda.min")
mse = sum((newy - y1) ^ 2) / length(y1)
mse