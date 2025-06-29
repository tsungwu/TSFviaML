library(timeSeries)
library(iForecast)
library(forecast)
tmp=read.csv("./data/CPI.csv") #core CPI
y0=as.timeSeries(tmp[,2],as.Date(tmp[,1]))
y=na.omit(diff(log(y0),12))*100 #Core Inflation


# 3.1 Random forest

output1.rf = tts.caret(y=y,
                         x=NULL,
                         arOrder=c(1:6,12),
                         xregOrder=c(0),
                         method="rf", 
                         train.end="2021-12-01",
                         type=c("none","trend","season","both")[1],
                         tuneLength =10,
                         resampling=c("boot","cv","repeatedcv")[1],
                         preProcess = "center")
tail(output1.rf$dataused)
output1.rf$training.Pred
output1.rf$output$bestTune

testData=window(output1.rf$dataused,start="2022-01-01",end=end(y))
fcst1.rf=iForecast(Model=output1.rf,Type="dynamic",n.ahead = nrow(testData)) # dynamic forecasts generated
fcst1.rf=as.timeSeries(fcst1.rf,as.character(time(testData)))
forecast::accuracy(x=as.ts(testData[,1]),as.ts(fcst1.rf))

output2.rf = tts.caret(y=y,
                       x=NULL,
                       arOrder=c(1:6,12),
                       xregOrder=c(0),
                       method="rf", 
                       train.end="2021-12-01",
                       type=c("none","trend","season","both")[4],
                       tuneLength =10,
                       resampling=c("boot","cv","repeatedcv")[1],
                       preProcess = "center")

options(scipen = 9)
varImp(output2.rf$output)

testData=window(output2.rf$dataused,start="2022-01-01",end=end(y))
fcst2.rf=iForecast(Model=output2.rf,Type="dynamic",n.ahead = nrow(testData)) # dynamic forecasts generated
fcst2.rf=as.timeSeries(fcst2.rf,as.character(time(testData)))
forecast::accuracy(x=as.ts(testData[,1]),as.ts(fcst2.rf))


#Plots
TEXT1.rf=round(forecast::accuracy(x=as.ts(testData[,1]),as.ts(fcst1.rf))[,-c(1,4)],3)
TEXT1a.rf=paste(names(TEXT1.rf),collapse = "    ")
TEXT1b.rf=paste(as.numeric(TEXT1.rf),collapse = "    ")

TEXT2.rf=round(forecast::accuracy(x=as.ts(testData[,1]),as.ts(fcst2.rf))[,-c(1,4)],3)
TEXT2a.rf=paste(names(TEXT2.rf),collapse = "    ")
TEXT2b.rf=paste(as.numeric(TEXT2.rf),collapse = "    ")

dev.new() #Figure 3.10
par(mfrow=c(2,1))
Y1=window(y,start="2015-01-01",end=end(y))
plot(Y1,col="steelblue",xlab="(A) No trend dummy, No seasonal dummies",ylab="%",ylim=c(range(c(Y1, fcst1.rf))),
     main="Multistep forecasts of US core inflation \n Random Forest");grid()
rug(as.vector(Y1), ticksize = 0.01, side = 2, quiet = TRUE)
abline(v=as.POSIXct("2022-01-01"),col="red",lty=3)
lines(fcst1.rf,col="red",lwd=2)
text(as.POSIXct("2024-05-01"),5,"forecasts",col="red")
text(as.POSIXct("2023-08-01"),3.5,"actual",col="steelblue")
text(as.POSIXct("2018-01-01"),6.1,TEXT1a.rf,col="blue")
text(as.POSIXct("2018-01-01"),5.5,TEXT1b.rf,col="blue")

plot(Y1,col="steelblue",xlab="(B) Both trend and seasonal dummies",ylab="%",ylim=c(range(c(Y1, fcst2.rf))),
     main="Multistep forecasts of US core inflation \n Random Forest");grid()
rug(as.vector(Y1), ticksize = 0.01, side = 2, quiet = TRUE)
abline(v=as.POSIXct("2022-01-01"),col="red",lty=3)
lines(fcst2.rf,col="red",lwd=2)
text(as.POSIXct("2024-05-01"),5,"forecasts",col="red")
text(as.POSIXct("2023-08-01"),3.5,"actual",col="steelblue")
text(as.POSIXct("2018-01-01"),6.1,TEXT2a.rf,col="blue")
text(as.POSIXct("2018-01-01"),5.5,TEXT2b.rf,col="blue")
par(mfrow=c(2,2))


#inflation difference
Ly=embed(y,2)
Ly=timeSeries(Ly,as.character(time(y))[-1])
dy=Ly[,1]-Ly[,2]

output2.rf = tts.caret(y=dy,
                       x=NULL,
                       arOrder=c(1:6),
                       xregOrder=c(0),
                       method="rf", 
                       train.end="2021-12-01",
                       type=c("none","trend","season","both")[4],
                       tuneLength =10,
                       resampling=c("boot","cv","repeatedcv")[1],
                       preProcess = "center")



tail(output2.rf$dataused)
output2.rf$training.Pred
output2.rf$output$bestTune

testData=window(y,start="2022-01-01",end=end(y))
fcst2.rf=iForecast(Model=output1.rf,Type="dynamic",n.ahead = nrow(testData))

fcst2.rf[1]=fcst2.rf[1]+window(Ly[,2],start="2022-01-01",end="2022-01-01")
fcst2.rf=cumsum(fcst2.rf)

fcst2.rf=as.timeSeries(fcst2.rf,as.character(time(testData)))
forecast::accuracy(x=as.ts(testData[,1]),as.ts(fcst2.rf))



#3.2 nnet
output.nnet = tts.caret(y=y,
                         x=NULL,
                         arOrder=c(1:6),
                         xregOrder=c(0),
                         method="nnet", 
                         train.end="2021-12-01",
                         type=c("none","trend","season","both")[4],
                         tuneLength =10,
                         resampling=c("boot","cv","repeatedcv")[1],
                         preProcess = "center")

#3.3 svm
output1.svm = tts.caret(y=y,
                         x=NULL,
                         arOrder=c(1:6),
                         xregOrder=c(0),
                         method="svm", 
                         train.end="2021-12-01",
                         type=c("none","trend","season","both")[1],
                         tuneLength =14,
                         resampling=c("boot","cv","repeatedcv")[1],
                         preProcess = "center")

tail(output1.svm$dataused)

output1.svm$training.Pred
output1.svm$output$bestTune

testData=window(output1.svm$dataused,start="2022-01-01",end=end(y))
fcst1.svm=iForecast(Model=output1.svm,Type="dynamic",n.ahead = nrow(testData)) # dynamic forecasts generated
fcst1.svm=as.timeSeries(fcst1.svm,as.character(time(testData)))
forecast::accuracy(x=as.ts(testData[,1]),as.ts(fcst1.svm))


output2.svm = tts.caret(y=y,
                        x=NULL,
                        arOrder=c(1:6),
                        xregOrder=c(0),
                        method="svm", 
                        train.end="2021-12-01",
                        type=c("none","trend","season","both")[4],
                        tuneLength =14,
                        resampling=c("boot","cv","repeatedcv")[1],
                        preProcess = "center")

tail(output2.svm$dataused)
output2.svm$training.Pred
output2.svm$output$bestTune

testData=window(output2.svm$dataused,start="2022-01-01",end=end(y))
fcst2.svm=iForecast(Model=output2.svm,Type="dynamic",n.ahead = nrow(testData)) # dynamic forecasts generated
fcst2.svm=as.timeSeries(fcst2.svm,as.character(time(testData)))
forecast::accuracy(x=as.ts(testData[,1]),as.ts(fcst2.svm))


#plots
TEXT1.svm=round(forecast::accuracy(x=as.ts(testData[,1]),as.ts(fcst1.svm))[,-c(1,4)],3)
TEXT1a.svm=paste(names(TEXT1.svm),collapse = "    ")
TEXT1b.svm=paste(as.numeric(TEXT1.svm),collapse = "    ")

TEXT2.svm=round(forecast::accuracy(x=as.ts(testData[,1]),as.ts(fcst2.svm))[,-c(1,4)],3)
TEXT2a.svm=paste(names(TEXT2.svm),collapse = "    ")
TEXT2b.svm=paste(as.numeric(TEXT2.svm),collapse = "    ")

dev.new() #Figure 3.17
Y1=window(y,start="2015-01-01",end=end(y))
par(mfrow=c(2,1))
plot(Y1,col="steelblue",xlab="(A) No trend dummy, No seasonal dummies",ylab="%",ylim=c(range(c(Y1, fcst1.svm))),
     main="Multistep forecasts of US core inflation \n SVM");grid()
rug(as.vector(Y1), ticksize = 0.01, side = 2, quiet = TRUE)
abline(v=as.POSIXct("2022-01-01"),col="red",lty=3)
lines(fcst1.svm,col="red",lwd=2)
text(as.POSIXct("2024-05-01"),8,"forecasts",col="red")
text(as.POSIXct("2023-08-01"),3.5,"actual",col="steelblue")
text(as.POSIXct("2018-01-01"),6.5,TEXT1a.svm,col="blue")
text(as.POSIXct("2018-01-01"),5.5,TEXT1b.svm,col="blue")



plot(Y1,col="steelblue",xlab="(B) Both trend and seasonal dummies",ylab="%",ylim=c(range(c(Y1, fcst2.svm))),
     main="Multistep forecasts of US core inflation \n SVM");grid()
rug(as.vector(Y1), ticksize = 0.01, side = 2, quiet = TRUE)
abline(v=as.POSIXct("2022-01-01"),col="red",lty=3)
lines(fcst2.svm,col="red",lwd=2)
text(as.POSIXct("2024-01-01"),5,"actual",col="steelblue")
text(as.POSIXct("2023-05-01"),3,"forecasts",col="red")
text(as.POSIXct("2018-01-01"),6.1,TEXT2a.svm,col="blue")
text(as.POSIXct("2018-01-01"),5.5,TEXT2b.svm,col="blue")
par(mfrow=c(1,1))





#3.4 GBM
output1.gbm = tts.caret(y=y,
                        x=NULL,
                        arOrder=c(1:12),
                        xregOrder=c(0),
                        method="gbm", 
                        train.end="2021-12-01",
                        type=c("none","trend","season","both")[4],
                        tuneLength =14,
                        resampling=c("boot","cv","repeatedcv")[1],
                        preProcess = "center")
library(gbm)
options(scipen = 9)
varImp(output1.gbm$output)

output1.gbm$training.Pred
output1.gbm$output$bestTune

testData=window(output1.gbm$dataused,start="2022-01-01",end=end(y))
fcst1.gbm=iForecast(Model=output1.gbm,Type="dynamic",n.ahead = nrow(testData)) # dynamic forecasts generated
fcst1.gbm=as.timeSeries(fcst1.gbm,as.character(time(testData)))
forecast::accuracy(x=as.ts(testData[,1]),as.ts(fcst1.gbm))


output2.gbm = tts.caret(y=y,
                        x=NULL,
                        arOrder=c(1:6),
                        xregOrder=c(0),
                        method="svm", 
                        train.end="2021-12-01",
                        type=c("none","trend","season","both")[4],
                        tuneLength =14,
                        resampling=c("boot","cv","repeatedcv")[1],
                        preProcess = "center")

tail(output2.gbm$dataused)
output2.gbm$training.Pred
output2.gbm$output$bestTune

testData=window(output2.gbm$dataused,start="2022-01-01",end=end(y))
fcst2.gbm=iForecast(Model=output2.gbm,Type="dynamic",n.ahead = nrow(testData)) # dynamic forecasts generated
fcst2.gbm=as.timeSeries(fcst2.gbm,as.character(time(testData)))
forecast::accuracy(x=as.ts(testData[,1]),as.ts(fcst2.gbm))


#plots
TEXT1.gbm=round(forecast::accuracy(x=as.ts(testData[,1]),as.ts(fcst1.gbm))[,-c(1,4)],3)
TEXT1a.gbm=paste(names(TEXT1.gbm),collapse = "    ")
TEXT1b.gbm=paste(as.numeric(TEXT1.gbm),collapse = "    ")

TEXT2.gbm=round(forecast::accuracy(x=as.ts(testData[,1]),as.ts(fcst2.gbm))[,-c(1,4)],3)
TEXT2a.gbm=paste(names(TEXT2.gbm),collapse = "    ")
TEXT2b.gbm=paste(as.numeric(TEXT2.gbm),collapse = "    ")

dev.new() #Figure 3.22
Y1=window(y,start="2015-01-01",end=end(y))
par(mfrow=c(2,1))
plot(Y1,col="steelblue",xlab="(A) No trend dummy, No seasonal dummies",ylab="%",ylim=c(range(c(Y1, fcst1.gbm))),
     main="Multistep forecasts of US core inflation \n GBM");grid()
rug(as.vector(Y1), ticksize = 0.01, side = 2, quiet = TRUE)
abline(v=as.POSIXct("2022-01-01"),col="red",lty=3)
lines(fcst1.gbm,col="red",lwd=2)
text(as.POSIXct("2024-05-01"),6.0,"forecasts",col="red")
text(as.POSIXct("2023-08-01"),3.3,"actual",col="steelblue")
text(as.POSIXct("2018-01-01"),6.3,TEXT1a.gbm,col="blue")
text(as.POSIXct("2018-01-01"),5.3,TEXT1b.gbm,col="blue")



plot(Y1,col="steelblue",xlab="(B) Both trend and seasonal dummies",ylab="%",ylim=c(range(c(Y1, fcst2.gbm))),
     main="Multistep forecasts of US core inflation \n GBM");grid()
rug(as.vector(Y1), ticksize = 0.01, side = 2, quiet = TRUE)
abline(v=as.POSIXct("2022-01-01"),col="red",lty=3)
lines(fcst2.gbm,col="red",lwd=2)
text(as.POSIXct("2024-05-01"),2,"forecasts",col="red")
text(as.POSIXct("2024-05-01"),4,"actual",col="steelblue")
text(as.POSIXct("2018-01-01"),6.1,TEXT2a.gbm,col="blue")
text(as.POSIXct("2018-01-01"),5.5,TEXT2b.gbm,col="blue")
par(mfrow=c(1,1))




#3.6 h2o.AutoML

if(FALSE) {
# For rolling window, to avoid redundant initialization, you may execute two commands below, 
# and declare "initial=FALSE" within tts.autoML()
h2o::h2o.init()        # Initialize h2o
invisible(h2o::h2o.no_progress()) # Turn off progress bars
}
out1.autoML=tts.autoML(y=y,
                       x=NULL,
                       arOrder=c(1:3),
                       xregOrder=c(0),
                       type=c("none","trend","season","both")[1],
                       train.end="2021-12-01",
					   initial=TRUE)
print(out1.autoML$modelsUsed,n = nrow(out1.autoML$modelsUsed))
tail(out1.autoML$dataused)

testData1=window(out1.autoML$dataused,start="2022-01-01",end=end(y))
fcst1.autoM=iForecast(Model=out1.autoML,Type="dynamic",n.ahead = nrow(testData1)) # dynamic forecasts generated
fcst1.autoM=as.timeSeries(fcst1.autoM,as.character(time(testData1)))
forecast::accuracy(x=as.ts(testData1[,1]),as.ts(fcst1.autoM))


out2.autoML=tts.autoML(y=y,
                       x=NULL,
                       arOrder=c(1:6),
                       xregOrder=c(0),
                       type=c("none","trend","season","both")[4],
                       train.end="2021-12-01")
print(out2.autoML$modelsUsed,n = nrow(out2.autoML$modelsUsed))
tail(out2.autoML$dataused)

testData2=window(out2.autoML$dataused,start="2022-01-01",end=end(y))
fcst2.autoM=iForecast(Model=out2.autoML,Type="dynamic",n.ahead = nrow(testData2)) # dynamic forecasts generated
fcst2.autoM=as.timeSeries(fcst2.autoM,as.character(time(testData2)))
forecast::accuracy(x=as.ts(testData2[,1]),as.ts(fcst2.autoM))


#Plots
TEXT1=round(forecast::accuracy(x=as.ts(testData1[,1]),as.ts(fcst1.autoM))[,-c(1,4)],3)
TEXT1a=paste(names(TEXT1),collapse = "    ")
TEXT1b=paste(as.numeric(TEXT1),collapse = "    ")

TEXT2=round(forecast::accuracy(x=as.ts(testData2[,1]),as.ts(fcst2.autoM))[,-c(1,4)],3)
TEXT2a=paste(names(TEXT2),collapse = "    ")
TEXT2b=paste(as.numeric(TEXT2),collapse = "    ")

dev.new() #Figure 
Y1=window(y,start="2015-01-01",end=end(y))
par(mfrow=c(2,1))
plot(Y1,col="steelblue",xlab="(A) No trend dummy, No seasonal dummies",ylab="%",
     ylim=c(range(c(Y1, fcst1.autoM))),
     main="Multistep forecasts of US core inflation \n autoML");grid()
rug(as.vector(Y1), ticksize = 0.01, side = 2, quiet = TRUE)
abline(v=as.POSIXct("2022-01-01"),col="red",lty=3)
lines(fcst1.autoM,col="red",lwd=2)
text(as.POSIXct("2024-05-01"),6,"forecasts",col="red")
text(as.POSIXct("2023-08-01"),3.5,"actual",col="steelblue")
text(as.POSIXct("2018-01-01"),6.1,TEXT1a,col="blue")
text(as.POSIXct("2018-01-01"),5.5,TEXT1b,col="blue")

plot(Y1,col="steelblue",xlab="(B) Both trend and seasonal dummies",ylab="%",
     ylim=c(range(c(Y1, fcst2.autoM))),
     main="Multistep forecasts of US core inflation \n autoML");grid()
rug(as.vector(Y1), ticksize = 0.01, side = 2, quiet = TRUE)
abline(v=as.POSIXct("2022-01-01"),col="red",lty=3)
lines(fcst2.autoM,col="red",lwd=2)
text(as.POSIXct("2024-05-01"),5.5,"forecasts",col="red")
text(as.POSIXct("2023-08-01"),3.5,"actual",col="steelblue")
text(as.POSIXct("2018-01-01"),6.1,TEXT2a,col="blue")
text(as.POSIXct("2018-01-01"),5.5,TEXT2b,col="blue")
par(mfrow=c(2,2))





#3.6 one-step ahead

# One-step forecast

# Data manipulation
tmp=read.csv("./data/CPIx.csv") #core CPI
head(tmp)
y0=timeSeries(tmp[,-1],as.Date(tmp[,1]))
y=na.omit(diff(log(y0[,1]),12))*100 #Core Inflation
INF=cbind(y,y0[-seq(12),-c(1)])
colnames(INF)=c("inf","spread","unrate")
dep=INF[,1]
indep=INF[,-1]

J=rownames(window(dep,start="2021-12-01",end=end(INF)))

fcst1=NULL
for(i in 2:length(J)) { #i=2
output1 = tts.caret(y=dep,
                       x=indep,
                       arOrder=c(1:6),
                       xregOrder=c(1:2),
                       method="svm", 
                       train.end=J[i-1],
                       type=c("none","trend","season","both")[4],
                       tuneLength =10,
                       resampling=c("boot","cv","repeatedcv")[1],
                       preProcess = "center")
testData=output1$dataused[J[i],]
fcst0=iForecast(Model=output1,
                Type="static",
                newdata = testData) # dynamic forecasts generated
fcst1=c(fcst1,fcst0)
print(i)
}

fcst1=timeSeries(fcst1, J[-1])

Actual=window(dep,start=J[2],end=end(dep))
forecast::accuracy(x=as.ts(Actual),as.ts(fcst1))

TEXT=round(forecast::accuracy(x=as.ts(Actual),as.ts(fcst1))[,-c(1,4)],3)
TEXTa=paste(names(TEXT),collapse = "    ")
TEXTb=paste(as.numeric(TEXT),collapse = "    ")

dev.new() #Figure 3.29
Y1=window(y,start="2015-01-01",end=end(y))
plot(Y1,col="steelblue",xlab="Both trend and seasonal dummies",ylab="%",
     ylim=c(range(c(Y1, fcst1))),
     main="Onestep forecasts of US core inflation \n SVM");grid()
rug(as.vector(Y1), ticksize = 0.01, side = 2, quiet = TRUE)
abline(v=as.POSIXct("2022-01-01"),col="red",lty=3)
lines(fcst1,col="red",lwd=2)
text(as.POSIXct("2024-06-01"),4.3,"forecasts",col="red")
text(as.POSIXct("2023-08-01"),3.5,"actual",col="steelblue")
text(as.POSIXct("2018-01-01"),6.1,TEXTa,col="blue")
text(as.POSIXct("2018-01-01"),5.8,TEXTb,col="blue")


# 3.7

output = tts.caret(y=dep,
                   x=indep,
                   arOrder=c(1,4,7),
                   xregOrder=c(3,6,9),
                   method="svm", 
                   train.end="2021-12-01",
                   type="none",
                   tuneLength =14,
                   resampling="boot",
                   preProcess = "center")

round(tail(output$dataused),3)
M22=cbind(model_id=M22[,1],round(M22[,-1],3))
dev.new();gridExtra::grid.table((M22))
