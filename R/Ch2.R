#####################
### decomposition
#####################
GDP=read.csv("data/GDP.csv")
y=ts(GDP[,2], end=c(2024,12),freq=4)

dd_dec=decompose(y)
dev.new() #Figure 2.1
par(mfrow=c(2,2))
plot(y,main="", ylab="",xlab="(A) US real GDP per capita");grid()
plot(dd_dec$trend,main="", ylab="",xlab="(B) trend component");grid()
plot(dd_dec$seasonal,main="", ylab="",xlab="(C) seasonal component");grid()
plot(dd_dec$random,main="", ylab="",xlab="(D) random variations");grid()
par(mfrow=c(1,1))


###############################
# 2.1.2. Simulate an ARIMA process
###############################

ar1.sim0=arima.sim(n = 200, list(ar = c(0.5)))

ar1.sim1=ts(ar1.sim0, end=c(2024,12),freq=12)

ar1.sim2=timeSeries::as.timeSeries(ar1.sim1)


dev.new();par(mfrow=c(2,1))
plot(ar1.sim1,col="steelblue",ylab="AR(1)", xlab="(A) Draw a ts() object ")
grid()

plot(ar1.sim2,col="steelblue",ylab="AR(1)",xlab="(B) Draw a timeSeries() object")
grid()
rug(as.vector(ar1.sim2), ticksize = 0.01, side = 2, quiet = TRUE)
(mfrow=c(1,1))

arima.sim(n = 300, list(ar = c(0.5), ma = c(-0.25)))
arima.sim(n = 300, list(ar = c(0.5), ma = c(-0.25)),sd = sqrt(0.16))

arima.sim(list(order = c(0,1,0)), n = 600)

dev.new() #Figure 2.3
par(mfrow=c(2,1))
plot(arima.sim(list(order = c(0,1,0)), n = 600),
                xlab="(A) ARIMA(0,1,0)",ylab="",
                main="A simple non-stationary time series",
                col="steelblue");grid()
MAIN="ar=-0.135, ma=0.531"
plot(arima.sim(list(order=c(1,1,1),ar=c(0.135),ma=c(-0.531)), n = 600),
               xlab="(B) ARIMA(1,1,1)",ylab="",
               main=MAIN,
               col="steelblue");grid()
par(mfrow=c(1,1))

dev.new() #Figure 2.4
par(mfrow=c(2,1))
plot(arima.sim(list(sesonal = c(0,1,0)), n = 200),
     xlab="(A) seasonal ARIMA(0,1,0)",ylab="",
     main="Non-stationary time series with seasonal difference",
     col="steelblue");grid()
MAIN="sar=-0.135, sma=0.531"
plot(arima.sim(list(seasonal=c(1,1,1),sar=c(-0.135),sma=c(0.531)), n = 200),
     xlab="(B) Seasonal ARIMA(1,1,1)",ylab="",
     main=MAIN,
     col="steelblue");grid()
par(mfrow=c(1,1))


plot(arima.sim(list(order=c(1,1,1),ar = c(0.125),ma=c(-0.225)), n = 600))

plot(arima.sim(list(seasonal=c(1,1,1),sar = c(0.531),sma=c(-0.135)), n = 600))


# mildly long-tailed
arima.sim(n = 3000, list(ar=c(0.88, -0.48), ma=c(-0.22, 0.28)),rand.gen = function(n, ...) sqrt(0.16) * rt(n, df = 5))

TEXT="arima.sim(n=600, list(ar=c(0.88, -0.48), ma=c(-0.22, 0.28)),\n rand.gen = function(n) sqrt(0.16)*rt(n, df=5))"

dev.new()#Figure 2.5
plot(arima.sim(n = 3000, list(ar=c(0.88, -0.48), ma = c(-0.22, 0.28)),
                         rand.gen = function(n) sqrt(0.16) * rt(n, df = 5)), ylab="",main=TEXT,col="steelblue");grid()


y.norm=arima.sim(n = 3000, list(ar=c(0.88, -0.48), ma=c(-0.22, 0.28)))
y.rt=arima.sim(n = 3000, list(ar=c(0.88, -0.48), ma=c(-0.22, 0.28)),rand.gen = function(n, ...) sqrt(0.16) * rt(n, df = 5))

dev.new()#Figure 2.6
par(mfrow=c(2,1))
plot(density(y.norm),col="red",main="", xlab ="(A) Density of normal distribution");grid()
plot(density(y.rt),col="red",main="", xlab ="(B) Density of Student-t distribution");grid()
par(mfrow=c(1,1))

dev.new()#Figure 2.7
par(mfrow=c(2,1))
hist(y.norm,col="steelblue",main="", xlab ="(A) Histogram of normal distribution");grid()
hist(y.rt,col="steelblue",main="", xlab ="(B) Histogram of Student-t distribution");grid()
par(mfrow=c(1,1))


DGP1 <- Arima(ts(rt(200,7),freq=4), order=c(2,0,0), seasonal=c(0,0,2),
                        fixed=c(ar1=0.125,ar2=-0.15,sma1=0.0107,sma2=-0.107,intercept=0.2))
DGP2 <- Arima(ts(rt(200,5),freq=12),order=c(0,0,2), seasonal=c(2,0,0),
                        fixed=c(ma1=0.15,ma2=-0.125,sar1=0.07, sar2=-0.0147,intercept=-0.1))
n=1500
auto.arima(simulate(DGP1, nsim=n))
auto.arima(simulate(DGP2, nsim=n),ic="bic")

###############
# 2.1.3 Forecasting inflation
###############
library(forecast)
#library(timeSeries)
tmp=read.csv("./data/CPI.csv") #core CPI
tmp[400,]
y0=ts(tmp[,2],end=c(2024,12),freq=12)
y=na.omit(diff(log(y0),12))*100 #Core Inflation
out=auto.arima(y,ic="aic")
E_y=fitted(out)

MAIN="US core inflation \n ARIMA(2,1,2)(2,0,2)[12]" #Figure 2.8
dev.new();plot(y,col="steelblue",
               xlab="",ylab="%",
               main=MAIN);grid()
lines(E_y,col="red",lty=2)
rug(as.vector(y), ticksize = 0.01, side = 2, quiet = TRUE)


Y=window(y,start=c(2015,1)) #Figure 2.9
dev.new();plot(Y,col="steelblue",xlab="",ylab="%",
               main="US core inflation rate \n Out-of-sample:2022M1-2024M12");grid()
rug(as.vector(Y), ticksize = 0.01, side = 2, quiet = TRUE)
abline(v=c(2022),col="red",lty=3,lwd=3)
abline(v=c(2025),col="red",lty=3,lwd=3)

###############
# 2.1.4 Multistep forecasts
###############
in_sample=window(y,end=c(2021,12))
out_sample=window(y,start=c(2022,1))

OUT1=auto.arima(in_sample,ic="aic")
#arimaOrder1=arimaorder(OUT1) 
#arimaOrder1

round(accuracy(fitted(OUT1),x=in_sample)[,-c(1,4)],4)

Fcst1 = forecast(OUT1, h=length(out_sample))$mean

round(accuracy(Fcst1,x=out_sample)[,-c(1,4)],4)



TEXT=round(forecast::accuracy(Fcst1,x=out_sample)[,-c(1,4)],3)
TEXT1=paste(names(TEXT),collapse = "    ")
TEXT2=paste(as.numeric(TEXT),collapse = "    ")


dev.new() #Figure 2.10
Y1=window(y,start=c(2015,1))
plot(Y1, col="steelblue",xlab="",main="Multistep forecasts of US inflation",ylab="%");grid()
rug(as.vector(Y1), ticksize = 0.01, side = 2, quiet = TRUE)
abline(v=2022,col="red",lty=3)
lines(Fcst1,col="red",lwd=2)
text(2024.5,5,"forecasts",col="red")
text(2023.8,3.5,"actual",col="steelblue")
text(2018,6.1,TEXT1,col="blue")
text(2018,5.8,TEXT2,col="blue")

auto.arima(in_sample,ic="aic")
auto.arima(in_sample,ic="aicc")
auto.arima(in_sample,ic="bic")

###############
# 2.1.4 ARIMAx
###############

Rhs1=seasonaldummy(in_sample)
OUT2=auto.arima(in_sample,xreg =Rhs1)
#arimaOrder2=arimaorder(OUT2) 
#arimaOrder2

round(accuracy(fitted(OUT2),x=in_sample)[,-c(1,4)],4)

Rhs2=seasonaldummy(out_sample)
Fcst2 = forecast(OUT2, h=length(out_sample),xreg =Rhs2)$mean

TEXT=round(forecast::accuracy(Fcst2,x=out_sample)[,-c(1,4)],3)
TEXT1=paste(names(TEXT),collapse = "    ")
TEXT2=paste(as.numeric(TEXT),collapse = "    ")

dev.new() #Figure 2.11
Y1=window(y,start=c(2015,1))
plot(Y1,col="steelblue",xlab="",main="Multistep forecasts of US inflation",ylab="%");grid()
rug(as.vector(Y1), ticksize = 0.01, side = 2, quiet = TRUE)
abline(v=2022,col="red",lty=3)
lines(Fcst2,col="red",lwd=2)
text(2024.5,5.5,"forecasts",col="red")
text(2023.8,3.5,"actual",col="steelblue")
text(2018,6.1,TEXT1,col="blue")
text(2018,5.8,TEXT2,col="blue")



###############################
#2.2.2 SETAR/LSTAR. May not work for small out-of-sample
###############################


library(tsDyn)
tmp=read.csv("./data/CPI.csv") #core CPI
y0=ts(tmp[,2],start=c(1957,1),freq=12)
y=na.omit(diff(log(y0),12))*100 #Core Inflation
in_sample=window(y,end=c(2021,12))
out_sample=window(y,start=c(2022,1))

# SETAR, inflation level 
out1.setar=setar(in_sample,m=4,mL=3, mH=1,include="const")
fcst1.setar=predict(out1.setar,n.ahead = length(out_sample))
TEXT1.setar=round(accuracy(fcst1.setar,x=out_sample)[,-c(1,4)],3)
TEXT1a.setar=paste(names(TEXT1.setar),collapse = "    ")
TEXT1b.setar=paste(as.numeric(TEXT1.setar),collapse = "    ")

cbind(fcst1.setar,out_sample)

dev.new() #Figures 2.13 and 2.14
plot(out1.setar,ask=FALSE)

dev.new() #Figure 2.15
Y1=window(y,start=c(2015,1))
plot(Y1,col="steelblue",xlab="",ylab="%",ylim=c(range(c(Y1, fcst1.setar))),
     main="Multistep forecasts of US core inflation \n SETAR");grid()
rug(as.vector(Y1), ticksize = 0.01, side = 2, quiet = TRUE)
abline(v=2022,col="red",lty=3)
lines(fcst1.setar,col="red",lwd=2)
text(2024.5,5.5,"forecasts",col="red")
text(2023.8,3.5,"actual",col="steelblue")
text(2018,6.1,TEXT1a.setar,col="blue")
text(2018,5.8,TEXT1b.setar,col="blue")

# LSTAR, inflation level 
out1.lstar=lstar(in_sample,m=4,mL=3, mH=2,include="const")
fcst1.lstar=predict(out1.lstar,n.ahead = length(out_sample))
TEXT1.lstar=round(forecast::accuracy(fcst1.lstar,x=out_sample)[,-c(1,4)],3)
TEXT1a.lstar=paste(names(TEXT1.lstar),collapse = "    ")
TEXT1b.lstar=paste(as.numeric(TEXT1.lstar),collapse = "    ")

dev.new() #Figure 2.16
Y1=window(y,start=c(2015,1))
plot(Y1,col="steelblue",xlab="",ylab="%",ylim=c(range(c(Y1, fcst1.lstar))),
     main="Multistep forecasts of US core inflation \n LSTAR");grid()
rug(as.vector(Y1), ticksize = 0.01, side = 2, quiet = TRUE)
abline(v=2022,col="red",lty=3)
lines(fcst1.lstar,col="red",lwd=2)
text(2024.5,5.5,"forecasts",col="red")
text(2023.8,3.5,"actual",col="steelblue")
text(2018,6.1,TEXT1a.lstar,col="blue")
text(2018,5.8,TEXT1b.lstar,col="blue")

###################
# 2.3.2 BATS model
###################
library(forecast)
tmp=read.csv("./data/CPI.csv") #core CPI
y0=ts(tmp[,2],start=c(1957,1),freq=12)
y=na.omit(diff(log(y0),12))*100 #Core Inflation
in_sample=window(y,end=c(2021,12))
out_sample=window(y,start=c(2022,1))

# BATS, inflation level
out1.bats = bats(in_sample, seasonal.periods=TRUE)
fcst1.bats =forecast(out1.bats,h=length(out_sample))$mean
TEXT1.bats=round(accuracy(fcst1.bats,x=out_sample)[,-c(1,4)],3)
TEXT1a.bats=paste(names(TEXT1.bats),collapse = "    ")
TEXT1b.bats=paste(as.numeric(TEXT1.bats),collapse = "    ")


dev.new() #Figure 2.17
Y1=window(y,start=c(2015,1))
plot(Y1,col="steelblue",xlab="",ylab="%",ylim=c(range(c(Y1, fcst1.bats))),
     main="Multistep forecasts of US core inflation \n BATS");grid()
rug(as.vector(Y1), ticksize = 0.01, side = 2, quiet = TRUE)
abline(v=2022,col="red",lty=3)
lines(fcst1.bats,col="red",lwd=2)
text(2024.5,5.5,"forecasts",col="red")
text(2023.8,3.5,"actual",col="steelblue")
text(2018,6.1,TEXT1a.bats,col="blue")
text(2018,5.8,TEXT1b.bats,col="blue")


###############################
# Bagged model
###############################
# Bagged model 1. ETS
out1.ets = baggedModel(in_sample,fn = ets)
ID1.ets=which.min(sapply(out1.ets$models, AIC))
M1.ets=out1.ets$models[[ID1.ets]]
fcst1.ets =forecast(M1.ets,h=length(out_sample))$mean
TEXT1.ets=round(accuracy(fcst1.ets,x=out_sample)[,-c(1,4)],3)
TEXT1a.ets=paste(names(TEXT1.ets),collapse = "    ")
TEXT1b.ets=paste(as.numeric(TEXT1.ets),collapse = "    ")

# Bagged model 2. auto.arima
out1.arima = baggedModel(in_sample,fn = auto.arima)
ID1.arima=which.min(sapply(out1.arima$models,AIC))
M1.arima=out1.arima$models[[ID1.arima]]
fcst1.arima =forecast(M1.arima,h=length(out_sample))$mean
TEXT1.arima=round(accuracy(fcst1.arima,x=out_sample)[,-c(1,4)],3)
TEXT1a.arima=paste(names(TEXT1.arima),collapse = "    ")
TEXT1b.arima=paste(as.numeric(TEXT1.arima),collapse = "    ")

dev.new() #Figure 2.18A
par(mfrow=c(2,1))
Y1=window(y,start=c(2015,1))
plot(Y1,col="steelblue",xlab="(A) bagged.ets",ylab="%",ylim=c(range(c(Y1, fcst1.ets))),
     main="Multistep forecasts of US core inflation");grid()
rug(as.vector(Y1), ticksize = 0.01, side = 2, quiet = TRUE)
abline(v=2022,col="red",lty=3)
lines(fcst1.ets,col="red",lwd=2)
text(2024.5,5.5,"forecasts",col="red")
text(2023.8,3.5,"actual",col="steelblue")
text(2018,6.1,TEXT1a.ets,col="blue")
text(2018,5.5,TEXT1b.ets,col="blue")

#Figure 2.18B
plot(Y1,col="steelblue",xlab="(B) bagged.arima",ylab="%",ylim=c(range(c(Y1, fcst1.arima))),
     main="Multistep forecasts of US core inflation");grid()
rug(as.vector(Y1), ticksize = 0.01, side = 2, quiet = TRUE)
abline(v=2022,col="red",lty=3)
lines(fcst1.arima,col="red",lwd=2)
text(2024.5,5.5,"forecasts",col="red")
text(2023.8,3.5,"actual",col="steelblue")
text(2018,6.1,TEXT1a.arima,col="blue")
text(2018,5.5,TEXT1b.arima,col="blue")
par(mfrow=c(1,1))

# Averaging multistep forecasts
prediction.ets=lapply(out1.ets$models,function(x) forecast(x,h=length(out_sample))$mean)
prediction.ets=as.data.frame(prediction.ets)
colnames(prediction.ets)=paste0("P",1:100)
yf.ets=ts(apply(prediction.ets,1,mean),start=start(out_sample),freq=frequency(out_sample))
accuracy(yf.ets,x=out_sample)


prediction.arima=lapply(out1.arima$models,function(x) forecast(x,h=length(out_sample))$mean)
prediction.arima=as.data.frame(prediction.arima)
colnames(prediction.arima)=paste0("P",1:100)
yf.arima=ts(apply(prediction.arima,1,mean),start=start(out_sample),freq=frequency(out_sample))
accuracy(yf.arima,x=out_sample)


#############
#GAMS model
############

out1.gam=tsDyn::aar(in_sample,m=1,d=3)
fcst1.gam=predict(out1.gam, n.ahead = length(out_sample))
TEXT=round(accuracy(fcst1.gam, x=out_sample)[,-c(1,4)],3)
TEXT1=paste(names(TEXT),collapse = "    ")
TEXT2=paste(as.numeric(TEXT),collapse = "    ")

dev.new() #Figure 2.19
Y1=window(y,start=c(2015,1))
plot(Y1,col="steelblue",xlab="GAMS",ylab="%",
     main="Multistep forecasts of US core inflation");grid()
rug(as.vector(Y1), ticksize = 0.01, side = 2, quiet = TRUE)
abline(v=2022,col="red",lty=3)
lines(fcst1.gam,col="red",lwd=2)
text(2024.5,5.5,"forecasts",col="red")
text(2023.8,3.5,"actual",col="steelblue")
text(2018,6.1,TEXT1,col="blue")
text(2018,5.8,TEXT2,col="blue")

#############
# ARIFMA
############
out1.arfima=arfima(in_sample, 
                   drange = c(0, 0.95),
                   estim = c("mle", "ls")[1], 
                   model = NULL,
                   lambda = "auto", 
                   biasadj = T,xreg=seasonaldummy(in_sample))

fcast.arfima <- forecast(out1.arfima, h=length(out_sample),bootstrap=TRUE)$mean
round(accuracy(fcast.arfima,x=out_sample)[,-c(1,4)],4)

TEXT=round(accuracy(fcast.arfima, x=out_sample)[,-c(1,4)],3)
TEXT1=paste(names(TEXT),collapse = "    ")
TEXT2=paste(as.numeric(TEXT),collapse = "    ")

dev.new() #Figure 2.20
Y1=window(y,start=c(2015,1))
plot(Y1,col="steelblue",xlab="ARFIMA",ylab="%",
     main="Multistep forecasts of US core inflation");grid()
rug(as.vector(Y1), ticksize = 0.01, side = 2, quiet = TRUE)
abline(v=2022,col="red",lty=3)
lines(fcast.arfima,col="red",lwd=2)
text(2024.5,5.5,"forecasts",col="red")
text(2023.8,3.5,"actual",col="steelblue")
text(2018,6.1,TEXT1,col="blue")
text(2018,5.8,TEXT2,col="blue")




#Diebold-Mariano Test
e.arima=Fcst1-out_sample
e.setar=fcst1.setar-out_sample
e.lstar=fcst1.lstar-out_sample
e.bats=fcst1.bats-out_sample
e.gam=fcst1.gam-out_sample

dm.test(e.setar,e.arima,alternative="greater")

dm.test(e.lstar,e.arima,alternative="greater")

dm.test(e.bats,e.arima,alternative="greater")

dm.test(fcst1.ets-out_sample,Fcst1-out_sample,alternative="greater")

dm.test(fcst1.arima-out_sample,Fcst1-out_sample,alternative="greater")

dm.test(e.gam,e.arima,alternative="greater")



Sys.setlocale(category = "LC_ALL", locale = "English_United States.1252")

# One-step forecast
library(forecast)
# Data manipulation
tmp=read.csv("./data/CPIx.csv") #core CPI
head(tmp,15)
y0=ts(tmp,start=c(1957,1),freq=12)
y=diff(log(y0[,2]),12)*100 #Core Inflation
dat0= na.omit(data.frame(y,y0[-seq(12),-c(1:2)]))
dat=embed(as.matrix(dat0),2)
INF=cbind(dat[,1],dat[,5:6])
colnames(INF)=c("inf","spreadL1","unrateL1")
INF =ts(INF, end=c(2024,12),freq=12)

# Moving one-step forecast

J=data.frame(Year=c(2021,rep(2022:2024,each=12)),
             Month=c(12, rep(1:12,3)))
fcst1.arx=NULL 
for (i in (2:(nrow(J)-1))) {
  mw_in=window(INF,end=c(J[i-1,1],J[i-1,2]))
  mw_out=window(INF,start=c(J[i,1],J[i,2]))

  out0=Arima(mw_in[,1],order = c(2,1,0),seasonal=c(2,0,0),xreg =mw_in[,-1])
  if (i==36) {
    fcst1.arx=c(fcst1.arx,
                  forecast(out0,h=2,
                           xreg =mw_out[,-1])$mean)
  } else {
    fcst1.arx=c(fcst1.arx,
                  forecast(out0,h=1,
                           xreg =mw_out[,-1])$mean[1])
  }
  print(i)
}


fcst1.lm=ts(fcst1.arx,end = c(2024,12),freq=12)
TEXT1.lm=round(accuracy(fcst1.lm, x=out_sample[,1])[,-c(1,4)],3);TEXT1.lm
TEXT1a.lm=paste(names(TEXT1.lm),collapse = "    ")
TEXT1b.lm=paste(as.numeric(TEXT1.lm),collapse = "    ")



# ARIMAx
in_sample=window(INF,end=c(2021,12))
out_sample=window(INF,start=c(2022,1))
out0.armax=auto.arima(in_sample[,1],xreg=in_sample[,-1])

fcst1.armax=NULL
for (i in (2:(nrow(J)-1))) {#i=36
mw_in=window(INF,end=c(J[i-1,1],J[i-1,2]))
mw_out=window(INF,start=c(J[i,1],J[i,2]))
#mod=auto.arima(mw_in[,1],xreg=mw_in[,-1])
mod=Arima(mw_in[,1],model=out0.armax,xreg=mw_in[,-1])
#h=ifelse(i==36,2,1)
if (i==36) {
fcst1.armax=c(fcst1.armax,
              forecast(mod,h=2,
                       xreg =mw_out[,-1])$mean)
} else {
fcst1.armax=c(fcst1.armax,
              forecast(mod,h=1,
                       xreg =mw_out[,-1])$mean[1])
}
print(i)
}

fcst1.armax=ts(fcst1.armax,end=c(2024,12),freq=12)
accuracy(fcst1.armax, x=out_sample[,1])

TEXT1.armax=round(accuracy(x=out_sample[,1],fcst1.armax)[,-c(1,4)],3)
TEXT1a.armax=paste(names(TEXT1.armax),collapse = "    ")
TEXT1b.armax=paste(as.numeric(TEXT1.armax),collapse = "    ")


Y1=window(y,start=c(2010,1))

dev.new() #Figure 2.23
par(mfrow=c(2,1))
plot(Y1,col="steelblue",xlab="(B) auto.arima(2,1,2)(2,0,0)+X",ylab="%",
     main="Moving one-step forecasts of US core inflation");grid()
rug(as.vector(Y1), ticksize = 0.01, side = 2, quiet = TRUE)
abline(v=2022,col="red")
lines(window(fcst1.armax,start=start(Y1)),col="red",lwd=2,lty=3)
text(2024.5,5.5,"forecasts",col="red")
text(2023.8,3.5,"actual",col="steelblue")
text(2015,6.1,TEXT1a.armax,col="blue")
text(2015,5.2,TEXT1b.armax,col="blue")

plot(Y1,col="steelblue",xlab="(A) ARMA(2,1,0)(2,0,0)+X",ylab="%",
     main="Moving one-step forecasts of US core inflation");grid()
rug(as.vector(Y1), ticksize = 0.01, side = 2, quiet = TRUE)
abline(v=2022,col="red")
lines(window(fcst1.lm,start=start(Y1)),col="red",lwd=2,lty=3)
text(2024.5,2.5,"forecasts",col="red")
text(2023.8,5.5,"actual",col="steelblue")
text(2015,6.1,TEXT1a.lm,col="blue")
text(2015,5.2,TEXT1b.lm,col="blue")
par(mfrow=c(1,1))


dm.test(out_sample[,1]-FCST1.lm,out_sample[,1]-fcst1.armax,alternative="greater")




##### Models averaging
FCST=ts(apply(cbind(fcst1.bats,fcst1.setar,fcst1.lstar,fcst1.gam),1,mean),end=c(2024,12),freq=12)

accuracy(x=out_sample,FCST)











# DJI Returns forecast
library(tsDyn)
load("data/StockMacro.RData")
y=na.omit(diff(log(StockMacro[,1])))*100 #DJI returns
in_sample=window(y,end=c(2023,12))
out_sample=window(y,start=c(2024,1))

OUT2=auto.arima(in_sample,ic="aic")
Fcst2 = forecast(OUT2, h=length(out_sample))$mean
round(accuracy(Fcst2,x=out_sample)[,-c(1,4)],3)

out2.setar=setar(in_sample,m=4,mL=3, mH=1,include="const")
fcst2.setar=predict(out2.setar,n.ahead = length(out_sample))
TEXT2.setar=round(accuracy(fcst2.setar,x=out_sample)[,-c(1,4)],3)

out2.lstar=lstar(in_sample,m=4,mL=3, mH=2,include="const")
fcst2.lstar=predict(out2.lstar,n.ahead = length(out_sample))
TEXT2.lstar=round(accuracy(fcst2.lstar,x=out_sample)[,-c(1,4)],3)

out2.bats = bats(in_sample, seasonal.periods=TRUE)
fcst2.bats =forecast(out2.bats,h=length(out_sample))$mean
TEXT2.bats=round(accuracy(fcst2.bats,x=out_sample)[,-c(1,4)],3)

out2.ets = baggedModel(in_sample,fn = ets)
ID2.ets=which.min(sapply(out2.ets$models, AIC))
M2.ets=out2.ets$models[[ID2.ets]]
fcst2.ets =forecast(M2.ets,h=length(out_sample))$mean
TEXT2.ets=round(accuracy(fcst2.ets,x=out_sample)[,-c(1,4)],3)

out2.arima = baggedModel(in_sample,fn = auto.arima)
ID2.arima=which.min(sapply(out2.arima$models,AIC))
M2.arima=out2.arima$models[[ID2.arima]]
fcst2.arima =forecast(M2.arima,h=length(out_sample))$mean
TEXT2.arima=round(accuracy(fcst2.arima,x=out_sample)[,-c(1,4)],3)


out2.gam=aar(in_sample,m=1,d=3)
fcst2.gam=predict(out2.gam, n.ahead = length(out_sample))
TEXT2.gam=round(accuracy(fcst2.gam, x=out_sample)[,-c(1,4)],3)


dm.test(fcst2.setar,Fcst2,alternative="greater")

dm.test(fcst2.lstar,Fcst2,alternative="greater")

dm.test(fcst2.bats,Fcst2,alternative="greater")

dm.test(fcst2.ets,Fcst2,alternative="greater")

dm.test(fcst2.arima,Fcst2,alternative="greater")

dm.test(fcst2.gam,Fcst2,alternative="greater")






#library(quantmod)
quantmod::getSymbols("^DJI", from="1992-01-01", adjust=TRUE)
save(DJI, file="data/DJI.RData")
load("data/DJI.RData")
y=na.omit(log(DJI[,"DJI.Volume"]))
y_in=y["1992:::2024"]
y_out=y["2025"]
