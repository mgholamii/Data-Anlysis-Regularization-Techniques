#############data set#########
rm(list = ls())
blood=read.table("D:/U of A/Courses/590/Final-Report/data.txt",header=T,sep="	")
blood=data.frame(blood)
library(glmnet)
attach(blood)
pairs(blood)
blood[1,]

###############descriptive analysis##############
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


###############for only clinical data#########
library(MASS)
x=blood[,c(2:18)]

y=blood[,1]
y=as.numeric(y)
fit.lin=lm(y~x)
fit.lin=lm(y~x$gender+x$married+x$smoke+as.factor(x$exercise)+x$age+x$weight+x$height+
             as.factor(x$overwt)+as.factor(x$race)+as.factor(x$alcohol)+as.factor(x$trt)
           +x$bmi+as.factor(x$stress)+as.factor(x$salt)+
             as.factor(x$chldbear)+as.factor(x$income)+as.factor(x$educatn))
fit.lin.omit=lm(y~x$gender+x$married+x$smoke+as.factor(x$exercise)+x$age+
                  as.factor(x$overwt)+as.factor(x$race)+as.factor(x$alcohol)+as.factor(x$trt)
                +x$bmi+as.factor(x$stress)+as.factor(x$salt)+
                  as.factor(x$chldbear)+as.factor(x$income)+as.factor(x$educatn))
summary(fit.lin.omit)
########exclud#########
xe=x[,-c(12,8)]
fit.lin=lm(y~xe)
summary(fit.lin)
library(xtable)
a=xtable(inf)

print(a,type= "html")




#fit.lin <- xtable(fit.lin)
#print(fit.lin,print.results = FALSE)
plot(y)
hist(y, xlab = "Systolic blood presure")
summary(y)

a=cor(x)
kappa(fit.lin)
########Chech the model######
library(car)
library(perturb)
plot(fit.lin)
inf=summary(influence.measures(fit.lin))
crPlots(fit.lin,terms = ~.)
studres(fit.lin)
fitted=fit.lin$fitted.values
plot(fitted,studres(fit.lin),xlab="Fitted values",ylab="Studentized Residual")
plot(x[,5],y)
plot(x[,6],y)
plot(x[,7],y)
str(fit.lin)
plot(x[,5],fit.lin$ residuals, main = "Residuals va Age", xlab = "Age",ylab = "Residuals")
plot(x[,6],fit.lin$ residuals,main = "Residuals va Weight", xlab = "Weight",ylab = "Residuals")
plot(x[,7],fit.lin$ residuals,main = "Residuals va Hight", xlab = "Hight",ylab = "Residuals")
###########omite the influence#######
x.o=blood[-c(8,32,204,231,243,339,355,356,366,375,403,474,485),c(2:18)]
y.o=blood[-c(8,32,204,231,243,339,355,356,366,375,403,474,485),1]
x.o=data.matrix(x.o)
y.o=as.numeric(y.o)
fit.lin.o=lm(y.o~x.o)
summary(fit.lin.o)
omit=xtable(fit.lin.o)
print(omit,type= "html")
plot(fit.lin.o)
summary(influence.measures(fit.lin.o))
crPlots(fit.lin.o,terms = ~.)
plot(x.o[,5],y.o)
plot(x.o[,6],y.o)
plot(x.o[,7],y.o)
str(fit.lin.o)
plot(x.o[,5],fit.lin.o$ residuals, main = "Residuals va Age", xlab = "Age",ylab = "Residuals")
plot(x.o[,6],fit.lin.o$ residuals,main = "Residuals va Weight", xlab = "Weight",ylab = "Residuals")
plot(x.o[,7],fit.lin.o$ residuals,main = "Residuals va Hight", xlab = "Hight",ylab = "Residuals")
##############variable selection###
data.v=data.frame(y.o,x.o)
x.o[1,]
attach(data.v)

a=step(lm(y.o~gender+married+smoke+exercise+age+weight+height+overwt+race+alcohol+
        trt+bmi+stress+salt+chldbear+income+educatn),direction = "both",data=data.v)
library(xtable)
a=xtable(a)

print(a,type= "html")
step(lm(y.o~gender+married+smoke+age+weight+height+overwt+race+alcohol+
          trt+bmi+stress+salt+chldbear+income+educatn),direction = "backward" ,data=data.v)

step(lm(y.o~gender+married+smoke+age+weight+height+overwt+race+alcohol+
          trt+bmi+stress+salt+chldbear+income+educatn),direction = "forward" ,data=data.v)
#######model based on stepwise##########
fit.step=lm(y.o~(x.o$married+x.o$smoke+as.factor(x.o$exercise)+x.o$age+x.o$height
        +as.factor(x.o$alcohol)+as.factor(x.o$trt)
           +x.o$bmi+as.factor(x.o$stress)+
             as.factor(x.o$income))^2)
fit.step=lm(y.o~married+smoke+exercise+age+height+alcohol+
     trt+bmi+stress+income)
extractAIC(fit.step)
newy=predict(fit.step,data.v$x.o,data=data.v)
mse = sum((newy - y.o) ^ 2) / length(y.o)
mse
summary(fit.step)
23*23

summary(fit.step)
plot(fit.step)
summary(influence.measures(fit.step))
crPlots(fit.lin.o,terms = ~.)
detach(data.v)
############subset selection#########
library(leaps)

ModelSel=leaps(x.o,y.o,method='Cp')
plot(ModelSel$size,abs(ModelSel$Cp-ModelSel$size),pch = 21, bg='red', xlab = "Selected Size", ylab="Cp - P")
aa = min(abs(ModelSel$Cp-ModelSel$size)[ModelSel$size==3])
bb = which(ModelSel$Cp == aa+3)
bb = which(ModelSel$Cp == 3-aa)
ModelSel$which[bb,]

ModelSel=leaps(x.o,y.o,method='adjr2')
aa = max(ModelSel$adjr2)
bb = which(ModelSel$adjr2 == aa)
ModelSel$which[bb,]

########################## LASSO#################
library(glmnet)

fit.lasoo = glmnet(x.o,y.o)
par(mfrow=c(1,2))
plot(fit.lasoo,label=TRUE)
plot(fit.lasoo,xvar="lambda",label=TRUE)

## slection of \lambda
fit.cv = cv.glmnet(x.o,y.o,lambda=fit.lasoo$lambda.min)
plot(fit.cv)
predict(fit.cv,newx=x[1:3,])
fit.cv$lambda.min
coef(fit.cv,s="lambda.min")
predict(fit.cv,newx=x[1:3,],s=c(0.001,0.002))
plot.cv.glmnet(fit.cv)
########################################################
#############################All covariates########################
#############data set#########
rm(list = ls())
blood=read.table("D:/U of A/Courses/590/Final-Report/data.txt",header=T,sep="	")
blood=data.frame(blood)
library(glmnet)
x=blood[,c(2:501)]
y=blood[,1]
x=data.matrix(x)
y=as.numeric(y)
attach(blood)
fit.res.lasoo=lm(y~gender+married+smoke+exercise+age+weight+height+overwt+race+alcohol+
          trt+bmi+stress+salt+chldbear+income+educatn+g7+g50+g137+g169+g179+g200+g298+g364
        +g391+g438+g447+g453+g465+g466)
summary(fit.res.lasoo)
###############all into regularization############
fit.gl=glmnet(x,y)
summary(fit.gl)
str(fit.gl)
plot(fit.gl, xvar = "lambda", label = TRUE)
cvfit=cv.glmnet(x, y)
plot(cvfit)
cvfit$lambda.min
newy=predict(cvfit, x, s = "lambda.min")
plot(newy)
points(y,col="red",pch=8)
coef(cvfit,s="lambda.min")
mse = sum((newy - y) ^ 2) / length(y)
mse
###############only genes into regularization############
par(mfrow=c(1,2))
p.fac = rep(1, 501)
p.fac[c(1:17)] = 0
fit.gl.pen=glmnet(x,y, penalty.factor = p.fac)
summary(fit.gl.pen)
#str(fit.gl.pen)
plot(fit.gl.pen, xvar = "lambda", label = TRUE)
cvfit.pen=cv.glmnet(x, y,penalty.factor = p.fac)
plot(cvfit.pen)
cvfit.pen$lambda.min
newy=predict(cvfit.pen, x, s = "lambda.min",penalty.factor = p.fac)
plot(newy)
points(y,col="red",pch=8)
coef(cvfit.pen,s="lambda.min")
mse = sum((newy - y) ^ 2) / length(y)
mse
#######################Elastic net alpha=0.6##################################
par(mfrow=c(1,2))
p.fac = rep(1, 501)
p.fac[c(1:17)] = 0
fit.gl.pen=glmnet(x,y, penalty.factor = p.fac,alpha = 0.6)
summary(fit.gl.pen)
#str(fit.gl.pen)
plot(fit.gl.pen, xvar = "lambda", label = TRUE)
cvfit.pen=cv.glmnet(x, y,penalty.factor = p.fac,alpha = 0.6)
plot(cvfit.pen)
cvfit.pen$lambda.min
newy=predict(cvfit.pen, x, s = "lambda.min",penalty.factor = p.fac)
plot(newy)
points(y,col="red",pch=8)
coef(cvfit.pen,s="lambda.min")
mse = sum((newy - y) ^ 2) / length(y)
mse

##########################Elastic without penalty#############################################
par(mfrow=c(1,2))

fit.gl.pen=glmnet(x,y,alpha = 0.6)
summary(fit.gl.pen)
#str(fit.gl.pen)
plot(fit.gl.pen, xvar = "lambda", label = TRUE)
cvfit.pen=cv.glmnet(x, y,alpha = 0.6)
plot(cvfit.pen)
cvfit.pen$lambda.min
newy=predict(cvfit.pen, x, s = "lambda.min")
plot(newy)
points(y,col="red",pch=8)
coef(cvfit.pen,s="lambda.min")
##############################################
##################logistic####################
rm(list = ls())
blood=read.table("D:/U of A/Courses/590/Final-Report/data.txt",header=T,sep="	")
blood=data.frame(blood)
library(glmnet)
x=blood[,c(2:501)]
y=c(rep(0,250),rep(1,250))
x=data.matrix(x)
attach(blood)

###############all into regularization############min mse#########
par(mfrow=c(1,2))
fit.gl=glmnet(x,y, family = "binomial")
summary(fit.gl)
str(fit.gl)
plot(fit.gl, xvar = "lambda", label = TRUE)
cvfit=cv.glmnet(x, y,family = "binomial")
plot(cvfit)
cvfit$lambda.min
newy=predict(cvfit, x, s = "lambda.min",family = "binomial")
plot(newy)
points(y,col="red",pch=8)
coef(cvfit,s="lambda.min")
mse = sum((newy - y) ^ 2) / length(y)
mse
exp(0.66)
###############only genes into regularization############
par(mfrow=c(1,2))
p.fac = rep(1, 501)
p.fac[c(1:17)] = 0
fit.gl.pen=glmnet(x,y, penalty.factor = p.fac,family = "binomial")
summary(fit.gl.pen)
#str(fit.gl.pen)
plot(fit.gl.pen, xvar = "lambda", label = TRUE)
cvfit.pen=cv.glmnet(x, y,penalty.factor = p.fac,family = "binomial")
plot(cvfit.pen)
cvfit.pen$lambda.min
newy=predict(cvfit.pen, x, s = "lambda.min",penalty.factor = p.fac,family = "binomial")
plot(newy)
points(y,col="red",pch=8)
coef(cvfit.pen,s="lambda.min")
mse = sum((newy - y) ^ 2) / length(y)
mse
#######################Elastic net alpha=0.6##################################
par(mfrow=c(1,2))
p.fac = rep(1, 501)
p.fac[c(1:17)] = 0
fit.gl.pen=glmnet(x,y, penalty.factor = p.fac,alpha = 0.6,family = "binomial")
summary(fit.gl.pen)
#str(fit.gl.pen)
plot(fit.gl.pen, xvar = "lambda", label = TRUE)
cvfit.pen=cv.glmnet(x, y,penalty.factor = p.fac,alpha = 0.6,family = "binomial")
plot(cvfit.pen)
cvfit.pen$lambda.min
newy=predict(cvfit.pen, x, s = "lambda.min",penalty.factor = p.fac,family = "binomial")
plot(newy)
points(y,col="red",pch=8)
coef(cvfit.pen,s="lambda.min")
mse = sum((newy - y) ^ 2) / length(y)
mse

##########################Elastic without penalty#############################################
par(mfrow=c(1,2))

fit.gl.pen=glmnet(x,y,alpha = 0.6,family = "binomial")
summary(fit.gl.pen)
#str(fit.gl.pen)
plot(fit.gl.pen, xvar = "lambda", label = TRUE)
cvfit.pen=cv.glmnet(x, y,alpha = 0.6,family = "binomial")
plot(cvfit.pen)
cvfit.pen$lambda.min
newy=predict(cvfit.pen, x, s = "lambda.min",family = "binomial")
plot(newy)
points(y,col="red",pch=8)
coef(cvfit.pen,s="lambda.min",family = "binomial")
mse = sum((newy - y) ^ 2) / length(y)
mse

##############################################
fit.res.lasoo=lm(y~gender+married+smoke+exercise+age+weight+height+overwt+race+alcohol+
                   trt+bmi+stress+salt+chldbear+income+educatn+g7+g50+g137+g169+g179+g200+g298+g364
                 +g391+g438+g447+g453+g465+g466)
summary(fit.res.lasoo)
