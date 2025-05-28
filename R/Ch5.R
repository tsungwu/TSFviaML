library(forecast)
library(iForecast)
library(timeSeries)

# 5.1 unemployment rate
load("data/unrate.RData")
y=na.omit(unrate[,1])
By=c("1m","6m")[2]
timeframe=iForecast::rollingWindows(y,estimation="720m",by=By) 
FROM=as.character(timeframe$from)
TO=as.character(timeframe$to)

#5.1.1 Univariate multistep forecasts
# ARIMA baseline by auto.arima
FCST0=NULL
start0=Sys.time();for (i in 1:(length(TO)-1)) { #  i=1
  
  EST=window(y,start=FROM[1],end=TO[i]) 
  VLD=window(y,start=TO[i],end=TO[i+1])[-1,] #[-1,] removes overlapping
  out=auto.arima(EST,ic="aic")
  fcst=forecast(out, h=hstep)$mean[seq(nrow(VLD))]
  fcst=as.matrix(fcst)
  
  rownames(fcst)=as.character(time(VLD))
  fcst=as.timeSeries(fcst)
  colnames(fcst)="FCST"
  FCST0 <- rbind(FCST0,fcst)
  
  print(paste(i,(length(TO)-1),sep="/"))
};end0=Sys.time()

end0-start0

Actual=window(y,start=start(FCST0),end=end(FCST0))

FCST=cbind(Actual=Actual,FCST=FCST0)
round(accuracy(x=as.ts(FCST[,"Actual"]),as.ts(FCST[,"FCST"]))[,-c(1,4)],3)

save(y,FCST,file=paste0("./output/data/univariate/multistep/arima.RData"))


#Machine Learning
METHODS=c("enet","gbm","rf")
hstep=6

for (m in METHODS) {

FCST0=NULL
start0=Sys.time();for (i in 1:(length(TO)-1)) { #  i=1
output = tts.caret(y=y,
                   x=NULL,
                   arOrder=c(1:12),
                   xregOrder=c(0),
                   method=m, 
                   train.end=TO[i],
                   type=c("none","trend","season","both")[4],
                   tuneLength =14,
                   resampling=c("boot","cv","repeatedcv")[1],
                   preProcess = "center")

VLD=window(output$dataused,start=TO[i],end=TO[i+1])[-1,]
fcst=iForecast(Model=output,Type="dynamic",n.ahead = nrow(VLD)) 
fcst=as.timeSeries(fcst,as.character(time(VLD)))
FCST0=rbind(FCST0,fcst)
print(paste0(m, ":",i,"/",(length(TO)-1)))
};end0=Sys.time()


end0-start0

Actual=window(y,start=start(FCST0),end=end(FCST0))

FCST=cbind(Actual=Actual,FCST=FCST0)
print(round(accuracy(x=as.ts(FCST[,"Actual"]),as.ts(FCST[,"FCST"]))[,-c(1,4)],3))
PATH=paste0("./output/data/unrate/univariate/multistep/",m,".RData")
save(y,FCST,file=PATH)
}

#5.1.1 VAR multistep forecasts
load("data/unrate.RData")
dat=na.omit(unrate[,1:4])
head(dat)
inf=diff(log(dat[,"CPI"]),12)*100
ICSA=log(dat[,"ICSA"])
ipiG=diff(log(dat[,"IPI"]),12)*100
y=na.omit(cbind(unrate=dat[-seq(12),1],INF=inf,ICSA=ICSA,ipiG=ipiG))
By=c("1m","6m")[6]
timeframe=iForecast::rollingWindows(y,estimation="480m",by=By) 
FROM=as.character(timeframe$from)
TO=as.character(timeframe$to)

# LS-VAR
library(timeSeries)
#library(vars)
FCST1=NULL;for (i in 1:(length(TO)-1)) { #  i=1
EST=window(y,start=FROM[1],end=TO[i]) 
VLD=window(y,start=TO[i],end=TO[i+1])[-1,] #[-1,] removes overlapping
out1.var=vars::VAR(EST,lag.max=12,type="none")
fcst_var=as.matrix(predict(out1.var,n.ahead=nrow(VLD))[[1]]$unrate[,1])
rownames(fcst_var)=rownames(VLD)
colnames(fcst_var)="unrate"
FCST1=rbind(FCST1,fcst_var)

}

FCST1=as.timeSeries(FCST1)
Actual=window(y[,"unrate"],start=start(FCST1),end=end(FCST1))
FCST=cbind(Actual=Actual,unrate=FCST1)
ACC1=round(accuracy(x=as.ts(FCST[,"Actual"]),as.ts(FCST[,"unrate"]))[,-c(1,4)],3)
if (By=="1m") {
save(y,FCST,file=paste0("./output/data/unrate/VAR/onestep/LS.RData"))
} else {save(y,FCST,file=paste0("./output/data/unrate/VAR/multistep/LS.RData"))}


# MLE-VECM
FCST2=NULL;for (i in 1:(length(TO)-1)) { #  i=1
  EST=window(y,start=FROM[1],end=TO[i]) 
  VLD=window(y,start=TO[i],end=TO[i+1])[-1,] #[-1,] removes overlapping
  lags = vars::VARselect(EST, lag.max=20)$selection 
  vecm.eg=tsDyn::VECM(y, lag=lags[1],include ="both", estim = "ML")
  fcst_var=predict(vecm.eg,n.ahead = nrow(VLD))[,"unrate",drop=FALSE]
  rownames(fcst_var)=rownames(VLD)
  FCST2=rbind(FCST2,fcst_var)
}
FCST2=as.timeSeries(FCST2)

Actual=window(y[,"unrate"],start=start(FCST2),end=end(FCST2))
FCST=cbind(Actual=Actual,unrate=FCST2)
ACC2=round(accuracy(x=as.ts(FCST[,"Actual"]),as.ts(FCST[,"unrate"]))[,-c(1,4)],3)
save(y,FCST,file=paste0("./output/data/unrate/VAR/multistep/VECM.RData"))


#plots
TEXT1=round(ACC1,4)
TEXT1a=paste(names(TEXT1),collapse = "    ")
TEXT1b=paste(as.numeric(TEXT1),collapse = "    ")

TEXT2=round(ACC2,4)
TEXT2a=paste(names(TEXT2),collapse = "    ")
TEXT2b=paste(as.numeric(TEXT2),collapse = "    ")

dev.new() #Figure 5.10
Y1=window(y[,"unrate"],start="2000-01-01",end=end(y))
par(mfrow=c(2,1))
plot(Y1,xlab="(A) LS-VAR",ylab="%",col="steelblue",ylim=range(c(Y1,FCST1)),
     main="Multistep forecasts of US unemployment");grid()
rug(as.vector(Y1), ticksize = 0.01, side = 2, quiet = TRUE)
#abline(v=as.POSIXct("2008-01-01"),col="red",lty=3)
lines(FCST1,col="red",lwd=2,lty=3)
text(as.POSIXct("2023-09-01"),7.0,"forecasts",col="red")
text(as.POSIXct("2010-01-01"),12.5,TEXT1a,col="blue")
text(as.POSIXct("2010-01-01"),11,TEXT1b,col="blue")

plot(Y1,col="steelblue",xlab="(B) VECM: Cointegrated VAR",ylab="%",
     main="Multistep forecasts of US unemployment");grid()
rug(as.vector(Y1), ticksize = 0.01, side = 2, quiet = TRUE)
#abline(v=as.POSIXct("2008-01-01"),col="red",lty=3)
lines(FCST2,col="red",lwd=2,lty=3)
text(as.POSIXct("2024-05-01"),6.5,"forecasts",col="red")
text(as.POSIXct("2010-01-01"),12.5,TEXT2a,col="blue")
text(as.POSIXct("2010-01-01"),11,TEXT2b,col="blue")
par(mfrow=c(1,1))





# Machine Learning VAR
library(iForecast)
load("data/unrate.RData")
dat=na.omit(unrate[,1:4])
head(dat)
inf=diff(log(dat[,"CPI"]),12)*100
ICSA=log(dat[,"ICSA"])
ipiG=diff(log(dat[,"IPI"]),12)*100
y=na.omit(cbind(unrate=dat[-seq(12),1],INF=inf,ICSA=ICSA,ipiG=ipiG))

By=c("1m","6m")[1]
timeframe=iForecast::rollingWindows(y,estimation="540m",by=By) 
FROM=as.character(timeframe$from)
TO=as.character(timeframe$to)
m=c("svm","enet","gbm")[3]
start0=Sys.time();FCST0=NULL;for (i in 1:(length(TO)-1)) { #  i=1
VLD=window(y,start=TO[i],end=TO[i+1])[-1,]
OUT1=tts.var(data=y,
             p=6,
             method=m,
             train.end=TO[i],
             type=c("none","trend","season","both")[1])

fcst_ml=as.matrix(iForecast.var(OUT1, n.ahead=nrow(VLD))[,1])
rownames(fcst_ml)=rownames(VLD)
fcst_ml=as.timeSeries(fcst_ml)
FCST0=rbind(FCST0,fcst_ml)
colnames(FCST0)=c("unrate")
print(paste0(m, ":",i,"/",(length(TO)-1)))
};end0=Sys.time();end0-start0

colnames(FCST0)="unrate"
Actual=window(y[,"unrate"],start=start(FCST0),end=end(FCST0))
FCST=cbind(Actual=Actual,FCST0)
round(accuracy(x=as.ts(FCST[,"Actual"]),as.ts(FCST[,"unrate"]))[,-c(1,4)],3)


if (By=="1m") {
  save(y,FCST,file=paste0("./output/data/unrate/VAR/onestep/",m,".RData"))
} else {save(y,FCST,file=paste0("./output/data/unrate/VAR/multistep/",m,".RData"))}




#===========================================
#5.2 Stock returns and macro-fundamentals
library(forecast)
library(iForecast)
library(timeSeries)
print(load("data/StockMacro.RData"))
dev.new();plot(StockMacro[,-c(1:3)],xlab="",main="") #Figure 5.14

dim(StockMacro)
head(StockMacro)
tail(StockMacro)

q=1
yID=c("SP500","DJI")[q]
dat=na.omit(StockMacro)[,c(q,4:13)]
head(dat)

dep=embed(dat[,yID],2)
indep=embed(dat[,-1],2)[,seq(ncol(dat[,-1]))]
RF=StockMacro[,"RF"][-1]/100
dep=(log(dep[,1]/dep[,2])-RF)*100
y=cbind(dep,indep)
rownames(y)=rownames(dat)[-1]
colnames(y)=colnames(dat)
y=as.timeSeries(y)
plot(y[,-1])

By=c("1m","3m")[1]
timeframe=iForecast::rollingWindows(y,estimation="540m",by=By) 
FROM=as.character(timeframe$from)
TO=as.character(timeframe$to)

# ARIMA
FCST0=NULL
for (i in 1:(length(TO)-1)) {#i=1
  EST=window(y,start=FROM[1],end=TO[i]) 
  VLD=window(y,start=TO[i],end=TO[i+1])[-1,] #[-1,] removes overlapping
  out0.armax=auto.arima(EST[,1])
  fcst=as.matrix(forecast(out0.armax,h=nrow(VLD))$mean)
  rownames(fcst)=rownames(VLD)
  colnames(fcst)=yID
  FCST0=rbind(FCST0,fcst)
  print(paste0(i,"/",length(TO), ": ARIMA, ", yID,", ", By))
}

FCST2=as.timeSeries(FCST0)
colnames(FCST2)=yID
Actual=window(y[,yID],start=start(FCST2),end=end(FCST2))
FCST2=cbind(Actual=Actual,FCST2[,yID])
colnames(FCST2)=c("Actual",yID)
ACC2=round(iForecast::Accuracy(x=FCST2[,"Actual"],FCST2[,yID]),3);ACC2
FCST=cbind(Actual=Actual,DJI=FCST2[,yID])
if (By=="1m") {
  save(y,FCST,file=paste0("./output/data/StockMacro/univariate/onestep/arima.RData"))
} else {save(y,FCST,file=paste0("./output/data/StockMacro/univariate/multistep/arima.RData"))}


# ARIMAx
FCST0=NULL
for (i in 1:(length(TO)-1)) {#i=1
  EST=window(y,start=FROM[1],end=TO[i]) 
  VLD=window(y,start=TO[i],end=TO[i+1])[-1,] #[-1,] removes overlapping
  out0.armax=auto.arima(EST[,1],xreg=EST[,-1])
  fcst=as.matrix(forecast(out0.armax,h=nrow(VLD),xreg =VLD[,-1])$mean)
  rownames(fcst)=rownames(VLD)
  colnames(fcst)=yID
  FCST0=rbind(FCST0,fcst)
  print(paste0(i,"/",length(TO), ": ARIMAx, ", yID,", ", By))
}
FCST1=as.timeSeries(FCST0)
colnames(FCST1)=yID
Actual=window(y[,yID],start=start(FCST1),end=end(FCST1))
FCST1=cbind(Actual=Actual,FCST1[,yID])
colnames(FCST1)=c("Actual",yID)
ACC1=round(iForecast::Accuracy(x=FCST1[,"Actual"],FCST1[,yID]),3);ACC1
FCST=cbind(Actual=Actual,DJI=FCST1[,yID])

if (By=="1m") {
  save(y,FCST,file=paste0("./output/data/StockMacro/univariate/onestep/arimax.RData"))
} else {save(y,FCST,file=paste0("./output/data/StockMacro/univariate/multistep/arimax.RData"))}




# VAR
#= LS
library(timeSeries)
#library(vars)
print(load("data/StockMacro.RData"))
dim(StockMacro)
head(StockMacro)
tail(StockMacro)

q=2
yID=c("SP500","DJI")[q]
dat=na.omit(StockMacro)[,c(q,4:13)]
head(dat)

dep=embed(dat[,yID],2)
indep=embed(dat[,-1],2)[,seq(ncol(dat[,-1]))]
RF=StockMacro[,"RF"][-1]/100
dep=(log(dep[,1]/dep[,2])-RF)*100
y=cbind(dep,indep)
rownames(y)=rownames(dat)[-1]
colnames(y)=colnames(dat)
y=as.timeSeries(y)

By=c("1m","3m")[2]
timeframe=iForecast::rollingWindows(y,estimation="540m",by=By) 
FROM=as.character(timeframe$from)
TO=as.character(timeframe$to)

FCST0=NULL;for (i in 1:(length(TO)-1)) { #  i=1
  EST=window(y,start=FROM[1],end=TO[i]) 
  VLD=window(y,start=TO[i],end=TO[i+1])[-1,] #[-1,] removes overlapping
  out1.var=vars::VAR(EST,lag.max=20,type="none")
  fcst_var=as.matrix(predict(out1.var,n.ahead=nrow(VLD))[[1]]$DJI[,1])
  rownames(fcst_var)=rownames(VLD)
  colnames(fcst_var)=yID
  FCST0=rbind(FCST0,fcst_var)
  print(paste0(i,"/",length(TO), ": VAR, ", yID,", ", By))
  
}

FCST1=as.timeSeries(FCST0)
Actual=window(y[,yID],start=start(FCST1),end=end(FCST1))
FCST1=cbind(Actual=Actual,FCST1[,yID])
colnames(FCST1)=c("Actual",yID)
ACC1=round(iForecast::Accuracy(x=FCST1[,"Actual"],FCST1[,yID]),3);ACC1
FCST=cbind(Actual=Actual,DJI=FCST1[,yID])
if (By=="1m") {
  save(y,FCST,file=paste0("./output/data/StockMacro/VAR/onestep/LS.RData"))
} else {save(y,FCST,file=paste0("./output/data/StockMacro/VAR/multistep/LS.RData"))}










#= Machine-Learning VAR
source("/src/ttsVAR.R")
library(timeSeries)
library(iForecast)
print(load("data/StockMacro.RData"))
dim(StockMacro)
head(StockMacro)
tail(StockMacro)

q=2
yID=c("SP500","DJI")[q]
dat=na.omit(StockMacro)[,c(q,4:13)]
head(dat)

dep=embed(dat[,yID],2)
indep=embed(dat[,-1],2)[,seq(ncol(dat[,-1]))]
RF=StockMacro[,"RF"][-1]/100
dep=(log(dep[,1]/dep[,2])-RF)*100
y=cbind(dep,indep)
rownames(y)=rownames(dat)[-1]
colnames(y)=colnames(dat)
y=as.timeSeries(y)


By=c("1m","3m")[1]
timeframe=iForecast::rollingWindows(y,estimation="540m",by=By) 
FROM=as.character(timeframe$from)
TO=as.character(timeframe$to)

m=c("svm","enet","gbm")[3]
start0=Sys.time();FCST0=NULL;for (i in 1:(length(TO)-1)) { #  i=1
  VLD=window(y,start=TO[i],end=TO[i+1])[-1,]
  OUT1=tts.var(data=y,
               p=6,
               method=m,
               train.end=TO[i],
               type=c("none","trend","season","both")[1])
  
  fcst_ml=as.matrix(iForecast.var(OUT1, n.ahead=nrow(VLD))[,1])
  rownames(fcst_ml)=rownames(VLD)
  fcst_ml=as.timeSeries(fcst_ml)
  FCST0=rbind(FCST0,fcst_ml)
  colnames(FCST0)=qID
  print(paste0(m, ":",i,"/",(length(TO)-1),", ",By))
};end0=Sys.time();end0-start0

FCST1=as.timeSeries(FCST0)
Actual=window(y[,yID],start=start(FCST1),end=end(FCST1))
FCST1=cbind(Actual=Actual,FCST1[,yID])
colnames(FCST1)=c("Actual",yID)
ACC1=round(accuracy(x=as.ts(FCST1[,"Actual"]),as.ts(FCST1[,yID]))[,-c(1,4:5)],3);ACC1
FCST=cbind(Actual=Actual,DJI=FCST1[,yID])

if (By=="1m") {
  save(y,FCST,file=paste0("./output/data/StockMacro/univariate/onestep/",m,".RData"))
} else {save(y,FCST,file=paste0("./output/data/StockMacro/univariate/multistep/",m,".RData"))}






#5.3 Volatility
library(timeSeries)
library(xts)
quantmod::getSymbols("^GSPC",from="1980-01-01")
dim(GSPC)
head(GSPC)
tail(GSPC)
GSPC=as.timeSeries(GSPC)

Ret=TTR::ROC(GSPC[,6], n=1,na.pad = TRUE)*100 #Adjusted.Close
mom=TTR::momentum(GSPC[,6], n = 14, na.pad = TRUE)
Ret=as.timeSeries(Ret)
mom=as.timeSeries(mom)

logV=log(GSPC[,5]) #Log Volume
rownames(Ret)=rownames(mom)=rownames(logV)=rownames(GSPC)

RV=(log(GSPC[,2]/GSPC[,3]))/(4*log(2))*100 #range-based volatility

ATR=as.timeSeries(TTR::ATR(GSPC[,c(2:4)], n=14)[,c("tr","atr")])
head(ATR)

SP500_daily=na.omit(cbind(Ret=Ret,MOM=mom,logV=logV,RV=RV,TR=ATR[,"tr"],ATR=ATR[,"atr"]))
head(SP500_daily)

TIME=paste0(start(SP500_daily)," : ",end(SP500_daily))

dev.new()#Figure 5.20
COLNAMES=c("log returns S&P500, %","Momentum, 14 days","log(Volume)",
           "Parkinson's Range Volatility","True Range","Average True Range, 14 days")
par(mfrow=c(3,2))
for (i in seq(6)) {
plot(SP500_daily[,i],main=COLNAMES[i],xlab="",
     ylab="",col=i);grid()
}
par(mfrow=c(1,1))

quantmod::getSymbols("^GSPC",from="1980-01-01")
dim(GSPC)
head(GSPC)
tail(GSPC)
GSPC=to.weekly(GSPC,OHLC=FALSE)
GSPC=as.timeSeries(GSPC)

Ret=TTR::ROC(GSPC[,6], n=1,na.pad = TRUE)*100 #Adjusted.Close
mom=TTR::momentum(GSPC[,6], n = 2, na.pad = TRUE)

logV=log(GSPC[,5]) #Log Volume
rownames(Ret)=rownames(mom)=rownames(logV)=rownames(GSPC)
Ret=as.timeSeries(Ret)
mom=as.timeSeries(mom)
logV=as.timeSeries(logV)
RV=as.timeSeries((log(GSPC[,2]/GSPC[,3]))/(4*log(2)))*100
volatility=cbind(Ret=Ret,MOM=mom,logV=logV,RV=RV)
head(volatility)
ATR=as.timeSeries(TTR::ATR(GSPC[,c(2:4)], n=2)[,c("tr","atr")])
head(ATR)

SP500_weekly=na.omit(as.timeSeries(cbind(volatility,TR=ATR[,"tr"],ATR=ATR[,"atr"])))
tail(SP500_weekly,15)
save(SP500_weekly,file="./data/SP500_weekly.RData")
#====================================================
load("./data/SP500_daily.RData")
load("./data/SP500_weekly.RData")

Y=SP500_daily
Y=SP500_weekly
dim(Y)
tail(Y,15)
#1. Choosing the dates of training and testing data
train.end="2024-12-30"
t0=which(as.character(time(Y))==train.end)
test.start=as.character(time(Y))[t0+1];test.start


variable.label="Weekly log returns of SP500,%"
dev.new();plot(Y[,1],col="steelblue",main=variable.label,ylab="");grid()
abline(h=0)
abline(v=as.POSIXct(test.start),col="red")
text(as.POSIXct(test.start), max(Y)*0.9, test.start,col="blue")

#2. Split training data and test data
Ys=na.omit(cbind(Y[,1],lag(Y[,-1])))
trainDatas=window(Ys,start=start(Ys),end=train.end)
testDatas=window(Ys,start=test.start,end=end(Ys))
ys=trainDataS;dim(ys)
ysa=testDataS
ahead=nrow(ysa)
#=======================#
# Static Forecasting    #
#=======================#
library(iForecast)
library(forecast)
#ARIMA
y.autos=auto.arima(ys[,1],xreg=ys[,-1])
arimaOrderS=arimaorder(y.autos) 
summary(y.autos)
if(is.na(arimaOrderS[c("P","D","Q")][1])) {seasonal=c(0, 0, 0)} else {seasonal=arimaOrderS[c("P","D","Q")]}
order1=paste0(arimaOrderS["p"],",",arimaOrderS["d"],",",arimaOrderS["q"])
if(anyNA(arimaOrderS[c("P","D","Q")])) {order2=c("0,0,0")
} else {order2=paste0(arimaOrderS["P"],",",arimaOrderS["D"],",",arimaOrderS["Q"])}
model.nameS=paste0("ARIMA(",order1,")(",order2,")")
fit1s=forecast(y.autos,h=ahead,xreg=ysa[,-1])$mean
dat1s=cbind(Actual=ysa[,1],fit=fit1s)

dev.new();plot(Ys[,1],ylab="%",main=paste0(colnames(Y)[1], ": ", model.nameS),xlab="");grid()
lines(dat1s[,2],col="red")
Accuracy(x=dat1s[,1],dat1s[,2])

#Machine Learning
output = tts.caret(y=Y[,1],
                   x=Y[,-1],
                   arOrder=c(1:12),
                   xregOrder=c(1),
                   method="svm", 
                   train.end=train.end,
                   type="none",
                   tuneLength =14,
                   resampling="boot",
                   preProcess = "center")

VLD=window(output$dataused,start=start(ya),end=end(ya))
fcst2s=iForecast(Model=output,Type="static",newdata=VLD)
dat2s=na.omit(cbind(x=ya[,1],fcst2s))
Accuracy(x=dat2s[,1],dat2s[,2])


#======================#
# Dynamic forecasting  #
#======================#
head(Y)

trainDataD=window(Y,start=start(Y),end=train.end)
testDataD=window(Y,start=test.start,end=end(Y))
yd=trainDataD;dim(yd)
yda=testDataD
ahead=nrow(yda)

# ARIMA
y.autoD=auto.arima(yd[,1])
arimaOrder=arimaorder(y.autoD) 
summary(y.autoD)
if(is.na(arimaOrder[c("P","D","Q")][1])) {seasonal=c(0, 0, 0)} else {seasonal=arimaOrder[c("P","D","Q")]}
order1=paste0(arimaOrder["p"],",",arimaOrder["d"],",",arimaOrder["q"])
if(anyNA(arimaOrder[c("P","D","Q")])) {order2=c("0,0,0")
} else {order2=paste0(arimaOrder["P"],",",arimaOrder["D"],",",arimaOrder["Q"])}
model.nameD=paste0("ARIMA(",order1,")(",order2,")")
fit1d=forecast(y.autoD,h=ahead)$mean
dat1d=cbind(Actual=yda[,1],fit=fit1d)
dev.new();plot(Y[,1],ylab="%",main=paste0(colnames(Y)[1], ": ", model.nameD),xlab="");grid()
lines(dat1d[,2],col="red")
Accuracy(x=dat1d[,1],dat1d[,2])


#Machine Learning
output = tts.caret(y=Y[,1],
                   x=NULL,
                   arOrder=c(1:8),
                   xregOrder=c(0),
                   method="svm", 
                   train.end=train.end,
                   type="none",
                   tuneLength =14,
                   resampling="boot",
                   preProcess = "center")

VLD=window(output$dataused,start=start(yda),end=end(yda))
fcst2d=iForecast(Model=output,Type="dynamic",n.ahead=nrow(VLD))
dat2d=na.omit(cbind(x=yda[,1],fcst2d))
Accuracy(x=dat2d[,1],dat2d[,2])













#5.4 Exchange rate 
library(forecast)
library(iForecast)
library(timeSeries)
fx_urls1="https://stats.bis.org/api/v2/data/dataflow/BIS/WS_XRU/1.0/D.XM.EUR.A?format=csv"
EuroUSD=read.csv(fx_urls1)[,c("TIME_PERIOD","OBS_VALUE")]
EuroUSD=xts::as.xts(EuroUSD[,-1],
                    order.by=as.Date(EuroUSD[,1]))
colnames(EuroUSD)="EuroUSD"
EuroUSD=na.omit(as.timeSeries(EuroUSD))
save(EuroUSD,file="./data/EuroUSD.RData")

fx_urls2="https://stats.bis.org/api/v2/data/dataflow/BIS/WS_XRU/1.0/D.GB.GBP.A?format=csv"
GBPUSD=read.csv(fx_urls2)[,c("TIME_PERIOD","OBS_VALUE")]
GBPUSD=xts::as.xts(GBPUSD[,-1],
                    order.by=as.Date(GBPUSD[,1]))
colnames(GBPUSD)="GBPUSD"
GBPUSD=na.omit(as.timeSeries(GBPUSD))
save(GBPUSD,file="./data/GBPUSD.RData")

y=log(window(GBPUSD,start="2020-01-01",end=end(GBPUSD)))



y0=window(y,start=start(y),end="2024-12-31");dim(y0)
y1=window(y,start="2025-01-01",end=end(y))

timeframe=iForecast::rollingWindows(y,estimation="1283d",by="1d") 
FROM=as.character(timeframe$from)
TO=as.character(timeframe$to)

FCST0=NULL
start0=Sys.time();for (i in 1:(length(TO)-1)) { #  i=1
  
  EST=window(y,start=FROM[i],end=TO[i]) 
  VLD=window(y,start=TO[i],end=TO[i+1])[-1,] #[-1,] removes overlapping
  out=auto.arima(EST,ic="aic")
  fcst=forecast(out, h=1)$mean[seq(nrow(VLD))]
  fcst=as.matrix(fcst)
  
  rownames(fcst)=as.character(time(VLD))
  FCST0 <- rbind(FCST0,fcst)
  colnames(FCST0)="FCST"
  print(paste(i,(length(TO)-1),sep="/"))
};end0=Sys.time();end0-start0

FCST0=as.timeSeries(FCST0)
Actual=window(y,start=start(FCST0),end=end(FCST0))

FCST=cbind(Actual=Actual,FCST=FCST0)
round(accuracy(x=as.ts(FCST[,"Actual"]),as.ts(FCST[,"FCST"]))[,-c(1,4)],3)

#save(y,FCST,file=paste0("./output/data/FX/univariate/onestep/arima.RData"))

Error_rw=window(diff(y),start=start(FCST0),end=end(FCST0))
Error_arima=FCST[,"Actual"]-FCST[,"FCST"]
dm.test(Error_rw,Error_arima,alternative="greater",power=1)




m=c("enet","gbm","rf")[1]
FCST0=NULL
start0=Sys.time();for (i in 1:(length(TO)-1)) { #  i=1
  output = tts.caret(y=y,
                     x=NULL,
                     arOrder=c(1:12),
                     xregOrder=c(0),
                     method=m, 
                     train.end=TO[i],
                     type=c("none","trend","season","both")[2],
                     tuneLength =14,
                     resampling=c("boot","cv","repeatedcv")[1],
                     preProcess = "center")
  
  VLD=window(output$dataused,start=TO[i],end=TO[i+1])[-1,]
  fcst=iForecast(Model=output,Type="static",newdata=VLD)
  rownames(fcst)=as.character(time(VLD))
  FCST0=rbind(FCST0,fcst)
  colnames(FCST0)="FCST"
  print(paste0(m, ":",i,"/",(length(TO)-1)))
};end0=Sys.time();end0-start0

FCST0=as.timeSeries(FCST0)

Actual=window(y,start=start(FCST0),end=end(FCST0))

FCST=cbind(Actual=Actual,FCST=FCST0)
Error_enet=FCST[,"Actual"]-FCST[,"FCST"]
dm.test(Error_rw,Error_enet,alternative="greater",power=1)
