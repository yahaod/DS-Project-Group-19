library(tidyverse)
dat<-read.csv('/Users/xiaohengliang/Desktop/DS-Project-Group-19-main/final_sales_data.csv')
dat1<-dat[,1:2]

dat0<-dat1%>%left_join(dat[,4:5],by=c('date'='Delivery.Date'))%>%
  left_join(dat[,7:8],by=c('date'='Delivery.Date.1'))%>%
  left_join(dat[,10:11],by=c('date'='Delivery.Date.2'))%>%
  left_join(dat[,13:14],by=c('date'='Delivery.Date.3'))

dat0<-na.omit(dat0)

mod1<-lm(MHL_sales~Ixom + lag(MHL_sales, 1),data=dat0)
summary(mod1)
tsdiag(arima(mod1$residuals,c(0,0,0)),gof.lag=6)# diagnose
layout(matrix(1:4,nrow=2));plot(mod1)
round(confint(mod1),4)#95% CI

mod2<-lm(SOPDHYP_sales~Ixom +lag(SOPDHYP_sales, 6),data=dat0)
summary(mod2)
tsdiag(arima(mod2$residuals,c(0,0,0)),gof.lag=6)# diagnose
layout(matrix(1:4,nrow=2));plot(mod2)
round(confint(mod2),4)#95% CI

mod3<-lm(PAC_sales~Ixom +lag(PAC_sales, 1),data=dat0)
summary(mod3)
tsdiag(arima(mod3$residuals,c(0,0,0)),gof.lag=6)# diagnose
layout(matrix(1:4,nrow=2));plot(mod3)
round(confint(mod3),4)#95% CI

mod4<-lm(ALU_sales~Ixom +lag(ALU_sales, 1)+lag(ALU_sales, 4),data=dat0)
summary(mod4)
tsdiag(arima(mod4$residuals,c(0,0,0)),gof.lag=6)# diagnose
layout(matrix(1:4,nrow=2));plot(mod4)
round(confint(mod4),4)#95% CI