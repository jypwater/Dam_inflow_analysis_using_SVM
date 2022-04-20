install.packages("dplyr")
install.packages("Metrics")
install.packages("dtw")
install.packages("reshape2")
install.packages("e1071")
install.packages("neuralnet")
library(dplyr)
library(nnet)
library(psych)#기술통계량
library(lubridate)#월,일month
library(data.table)
library(Metrics)
library(dtw)#동적time warping
library(reshape2)
library(stats)
library(readxl)#엑셀패키지
library(e1071)
library(neuralnet)

##경로설정
setwd("D:/test/1024_dam/result1107_ann")
getwd()
remove(list = ls())

##분석대상
temp = list.files(pattern="*.csv")
li=length(temp)
for(i in 1:li){
    a=temp[i]
    b=nchar(a)
    df0=read.csv(paste(a, sep=""),header = TRUE)
    na1=substr(a,1,(b-4))
na2=which(df0[,1]=="20161231")

##데이터전처리
df0[,5]=ifelse(((9-as.numeric(substr(as.character(df0[,1]),6,6)))%%9)>4,1,0)
sum(is.na(df0))
name=c("q_m","r_m","q1","q2","q3","r1","r2","r3","rs","d","season")
q=df0[,4]
rf=df0[,3]
rf_d=ifelse(rf<5,0,1)
rf=ifelse(rf<5,0,rf)
n=length(q)-5
df=as.data.frame(matrix(ncol = 11,nrow = n-5))
q_m1=as.data.frame(matrix(ncol = 10,nrow = n-5))
rf_m1=as.data.frame(matrix(ncol = 10,nrow = n-5))
for(i in 1:10){
    q_m1[,i]=q[c((5+i):(n+i-1))]
    rf_m1[,i]=rf[c((5+i):(n+i-1))]
}
q_m=apply(q_m1,1,mean)
r_m=apply(rf_m1,1,mean)
df[,1]=log(q_m+1)
df[,2]=r_m
for(i in 2:4){
    df[,i+1]=log(q[c((6-i+1):(n-i+1))]+1)
    df[,i+4]=rf[c((6-i+1):(n-i+1))]
}
df[,9]=df[,6]+df[,7]+df[,8]
df[,10]=rf_d[c(5:(n-1))]+rf_d[c(4:(n-2))]+rf_d[c(3:(n-3))]
df[,11]=df0[c(6:n),5]

df=df[c(1:na2),]
#df_z=df
m=mean(df[,1])
v=sd(df[,1])
df_z=as.data.frame(scale(df))
colnames(df_z)=name
df_z1=df_z

##SVM분석
#df_z1=df_z[,c(1:11)]
samp=c(1:(na2-366))
#samp=sample(1:11322,11322*0.9)
train.df1=df_z1[samp,]
test.df1=df_z1[-samp,]
#svm.dam=svm(q_m~.,data=train.df1,cost=1)

##ann
model.nnet=nnet(q_m~., data=train.df1, size=5, decay=0.0001, maxit=1000, linout = TRUE)##
#n1 = names(train.df1)
#form = as.formula(paste('q_m~',paste(n1[!n1 %in% 'q_m'],collapse = '+'))) 
#model.nnet=neuralnet(form, data=train.df1,hidden = c(4),linear.output = T) 

summary(model.nnet)
df.pred1 <- predict(model.nnet, test.df1)

##결과
#svm.dam=tune(svm, Q~.,data=train.df,ranges=list(gamma=c(0.1,1,10),cost=c(0.1,1,10)))
#summary(svm.dam)
#cor(svm.dam$fitted,train.df1$q_m)
#rmse(svm.dam$fitted,train.df1$q_m)
#mae(svm.dam$fitted,train.df1$q_m)

#df.pred1=predict(svm.dam, test.df1, type="class")
#test=cbind(df.pred1,test.df1)
x=exp((test.df1$q_m*v)+m)
y=exp((df.pred1*v)+m)
y1=((max(max(x),max(y))%/%10)+1)*10
plot(x, main=paste(na1,"댐(2016년예측)",sep=""),type="l",ylim=c(0,y1))
lines(y,col="red")
result=as.data.frame(matrix(ncol = 4))
colnames(result)=c("cor","rmse","mae","r2")
result[,1]=cor(x,y)
R2 <- 1 - (sum((x-y)^2)/sum((x-mean(x))^2))
result[,2]=rmse(x,y)
result[,3]=mae(x,y)
result[,4]=R2
write.csv(result,file=paste("result_",na1,".csv",sep = ""),row.names=FALSE)
}

temp2 = list.files(pattern="result*")
li1=length(temp2)
a1=temp2[1]
b1=read.csv(paste(a1, sep=""),header = TRUE)
resul1=cbind(substr(a1,8,9),b1)
for(i in 2:li1){
    a1=temp2[i]
    b1=read.csv(paste(a1, sep=""),header = TRUE)
    c=cbind(substr(a1,8,9),b1)
    resul1=rbind(resul1,c)
}
re=resul1[,c(2,3,5)]
re[16,]=apply(re,2,mean)
write.csv(resul1,file = "결과_ann.csv",row.names = FALSE)
