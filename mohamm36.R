install.packages("glmnet")
install.packages("forecast")
install.packages("glmnet")
install.packages("glmnetUtils")
library("glmnet")
library("caTools")
df<-read.csv("./Data.csv")
df<-df[complete.cases(df),]
m1.df<-df
m2.df<-df
set.seed(556)
sample = sample.split(m1.df, SplitRatio = 0.90)
m1train = subset(m1.df, sample == TRUE)
m1test = subset(m1.df, sample == FALSE)

#linearmodel------
linearm1<-lm(res.sales.adj~year+month+res.price+com.price+com.sales.adj+EMXP+MXSD+TPCP+TSNW+EMXT+EMNT+MMXT+MMNT+MNTM+DT90+DX32+DT00+DT32+DP01+DP05+DP10+MDPT+VISIB+WDSP+MWSPD+GUST+HTDD+CLDD+LABOR+EMP+UNEMP+UNEMPRATE+PCINCOME+GSP,data=m1train)
summary(linearm1)
par(mfrow=c(4,4))
plot(linearm1)
summary(linearm1t)
a<-predict(linearm1,newdata=m1test)
linearrmse<-sqrt(mean(((a)-(m1test$res.sales.adj))^2))
AIC(linearm1)

#logTransforamtion--------
linearm1t<-lm(log(res.sales.adj)~year+month+res.price+com.price+com.sales.adj+EMXP+MXSD+TPCP+TSNW+EMXT+EMNT+MMXT+MMNT+MNTM+DT90+DX32+DT00+DT32+DP01+DP05+DP10+MDPT+VISIB+WDSP+MWSPD+GUST+HTDD+CLDD+LABOR+EMP+UNEMP+UNEMPRATE+PCINCOME+GSP,data=m1train)
b<-predict(linearm1t,newdata = m1test)
LOGTRRMSE<-sqrt(mean(((b)-(log(m1test$res.sales.adj)))^2))
AIC(linearm1t)

#Boxcox-----
library("MASS")
library("forecast")
bc<-boxcox(res.sales.adj~year+month+res.price+com.price+com.sales.adj+EMXP+MXSD+TPCP+TSNW+EMXT+EMNT+MMXT+MMNT+MNTM+DT90+DX32+DT00+DT32+DP01+DP05+DP10+MDPT+VISIB+WDSP+MWSPD+GUST+HTDD+CLDD+LABOR+EMP+UNEMP+UNEMPRATE+PCINCOME+GSP,data=m1train, lambda=seq(-2,2, len=5))
l<- bc$x[which.max(bc$y)]
bc<-lm(res.sales.adj*((1-l)/l)~year+month+res.price+com.price+com.sales.adj+EMXP+MXSD+TPCP+TSNW+EMXT+EMNT+MMXT+MMNT+MNTM+DT90+DX32+DT00+DT32+DP01+DP05+DP10+MDPT+VISIB+WDSP+MWSPD+GUST+HTDD+CLDD+LABOR+EMP+UNEMP+UNEMPRATE+PCINCOME+GSP,data=m1train)
summary(bc)
bcp<-predict(bc,newdata = m1test)
BoxcoxRMSE<-sqrt(mean(((bcp)-((m1test$res.sales.adj)*((1-l)/l)))^2))


#Ridge-------
x2.train <- data.matrix(cbind(m1train[,-4]))
y2.train <-as.matrix((m1train$res.sales.adj))
m<-data.matrix(m1test[,-4])
cv.ridge2.mod<-cv.glmnet(x=x2.train,y=y2.train,alpha=0)
ridge2.mod<-glmnet(x=x2.train,y=y2.train, alpha=0,lambda=cv.ridge2.mod$lambda.min)
ridge2pred<-predict(ridge2.mod, newx=m)
RidgeRMSE<-sqrt(mean(((ridge2pred)-(m1test$res.sales.adj))^2))


#lasso----
cv.l2.mod<-cv.glmnet(x=x2.train,y=y2.train,alpha=1)
l2.mod<-glmnet(x=x2.train,y=y2.train, alpha=1,lambda=cv.l2.mod$lambda.min)
pred<-predict(l2.mod, newx=m)
LassoRMSE<-sqrt(mean(((pred)-(m1test$res.sales.adj))^2))


#GAM---
install.packages("gam")
library('gam')
library('mgcv')
gam.1<-gam(res.sales.adj~year+month+res.price+com.price+com.sales.adj+EMXP+MXSD+TPCP+TSNW+EMXT+EMNT+MMXT+MMNT+MNTM+DT90+DX32+DT00+DT32+DP01+DP05+DP10+MDPT+VISIB+WDSP+MWSPD+GUST+HTDD+CLDD+LABOR+EMP+UNEMP+UNEMPRATE+PCINCOME+GSP,family=gaussian(),data=m1train)
g<-predict(gam.1,newdata=m1test)
GAMRMSE<-sqrt(mean(((g)-(m1test$res.sales.adj))^2))


#STEPGAM----
alpha<-gam.scope(m1.df,response=4, smoother = "s", arg =c("df=1","df=2","df=3","df=4"), form = T)
gam.step<-step.Gam(gam.1, scope=alpha, direction = "both", trace=2)


#Random----
install.packages("randomForest")
install.packages("Metrics")
library('randomForest')
library('Metrics')
randomforest<-randomForest(res.sales.adj~year+month+res.price+com.price+com.sales.adj+EMXP+MXSD+TPCP+TSNW+EMXT+EMNT+MMXT+MMNT+MNTM+DT90+DX32+DT00+DT32+DP01+DP05+DP10+MDPT+VISIB+WDSP+MWSPD+GUST+HTDD+CLDD+LABOR+EMP+UNEMP+UNEMPRATE+PCINCOME+GSP,data=m1train)
rf<-predict(randomforest, newdata = m1test)
RFRMSE<-sqrt(mean(((rf)-(m1test$res.sales.adj))^2))
print(randomforest)
plot(randomforest)
importance(randomforest)
par(mfrow=c(1,1))
varImpPlot(randomforest)

#CART----
library('rpart.plot')
rpartmodel<-rpart(res.sales.adj~year+month+res.price+com.price+com.sales.adj+EMXP+MXSD+TPCP+TSNW+EMXT+EMNT+MMXT+MMNT+MNTM+DT90+DX32+DT00+DT32+DP01+DP05+DP10+MDPT+VISIB+WDSP+MWSPD+GUST+HTDD+CLDD+LABOR+EMP+UNEMP+UNEMPRATE+PCINCOME+GSP,data=m1train)
rp<-predict(rpartmodel, newdata = m1test)
CARTRMSE<-sqrt(mean((rp-m1test$res.sales.adj)^2))
AIC(rpartmodel)
rpart.plot(rpartmodel)


finalmodel<-randomForest(res.sales.adj~year+month+res.price+com.price+com.sales.adj+EMXP+MXSD+TPCP+TSNW+EMXT+EMNT+MMXT+MMNT+MNTM+DT90+DX32+DT00+DT32+DP01+DP05+DP10+MDPT+VISIB+WDSP+MWSPD+GUST+HTDD+CLDD+LABOR+EMP+UNEMP+UNEMPRATE+PCINCOME+GSP,data=m1train)
save(finalmodel,file="mohamm36.RData")
