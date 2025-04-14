# Data manipulation
library(timeSeries)
library(forecast)
tmp=read.csv("./data/CPIx.csv") #core CPI
head(tmp)
y0=timeSeries(tmp[,-1],as.Date(tmp[,1]))
y=na.omit(diff(log(y0[,1]),12))*100 #Core Inflation
INF=cbind(y,y0[-seq(12),-c(1)])
colnames(INF)=c("inf","spread","unrate")

#4.1 VAR
#= LS
library(vars)
insample=window(INF,start=start(INF),end="2021-12-01")
outsample=window(INF,start="2022-01-01",end=end(INF))
out1.var=VAR(insample,lag.max=12,type="both")

fcst_var=predict(out1.var,n.ahead=nrow(outsample))[[1]]$inf[,1]
fcst_var=ts(fcst_var,start=c(2022,1),freq=12)
accuracy(x=as.ts(outsample[,1]),fcst_var)



#= Machine Learning
library(iForecast)

OUT1=tts.var(data=INF,
            p=18,
            method="svm",
            train.end="2021-12-01",
            type=c("none","trend","season","both")[2])

OUT2=iForecast.var(OUT1, n.ahead=nrow(outsample))
rownames(OUT2)=rownames(outsample)
OUT2=as.timeSeries(OUT2)
accuracy(x=as.ts(outsample[,1]),as.ts(OUT2[,1]))



#plots
fcst_var=as.matrix(fcst_var)
rownames(fcst_var)=as.character(time(outsample))
TEXT1.svm=round(forecast::accuracy(x=as.ts(outsample[,1]),as.ts(as.timeSeries(fcst_var)))[,-c(1,4)],3)
TEXT1a.svm=paste(names(TEXT1.svm),collapse = "    ")
TEXT1b.svm=paste(as.numeric(TEXT1.svm),collapse = "    ")

TEXT2.svm=round(forecast::accuracy(x=as.ts(outsample[,1]),as.ts(OUT2[,1]))[,-c(1,4)],3)
TEXT2a.svm=paste(names(TEXT2.svm),collapse = "    ")
TEXT2b.svm=paste(as.numeric(TEXT2.svm),collapse = "    ")

dev.new() #Figure 4.1
Y1=window(y,start="2015-01-01",end=end(y))
par(mfrow=c(2,1))
plot(Y1,col="steelblue",xlab="(A) Trend dummy only",ylab="%",ylim=c(range(c(Y1, as.timeSeries(fcst_var)))),
     main="Multistep forecasts of US core inflation \n LS-VAR(10)");grid()
rug(as.vector(Y1), ticksize = 0.01, side = 2, quiet = TRUE)
abline(v=as.POSIXct("2022-01-01"),col="red",lty=3)
lines(as.timeSeries(fcst_var),col="red",lwd=2)
text(as.POSIXct("2024-05-01"),6.2,"forecasts",col="red")
text(as.POSIXct("2023-08-01"),3.5,"actual",col="steelblue")
text(as.POSIXct("2018-01-01"),6.2,TEXT1a.svm,col="blue")
text(as.POSIXct("2018-01-01"),5.5,TEXT1b.svm,col="blue")



plot(Y1,col="steelblue",xlab="(B) Trend dummy only",ylab="%",ylim=c(range(c(Y1, OUT2[,1]))),
     main="Multistep forecasts of US core inflation \n SVM-VAR(18)");grid()
rug(as.vector(Y1), ticksize = 0.01, side = 2, quiet = TRUE)
abline(v=as.POSIXct("2022-01-01"),col="red",lty=3)
lines(OUT2[,1],col="red",lwd=2)
text(as.POSIXct("2024-05-01"),2.5,"forecasts",col="red")
text(as.POSIXct("2023-08-01"),5.6,"actual",col="steelblue")
text(as.POSIXct("2018-01-01"),6.1,TEXT2a.svm,col="blue")
text(as.POSIXct("2018-01-01"),5.5,TEXT2b.svm,col="blue")
par(mfrow=c(1,1))


#4.2 Limited dependent variable

data(bc)
tail(bc)

output1.svm = tts.caret(y=bc[,1],
                        x=bc[,-1],
                        arOrder=c(1:3),
                        xregOrder=c(1:6),
                        method="svm", 
                        train.end="2017-12-01",
                        type=c("none","trend","season","both")[4],
                        tuneLength =14,
                        resampling=c("boot","cv","repeatedcv")[1],
                        preProcess = "center")

tail(output1.svm$dataused)
output1.svm$training.Pred
output1.svm$output$bestTune

testData=window(output1.svm$dataused,start="2018-01-01",end=end(bc))
fcst1.svm=iForecast(Model=output1.svm,Type="static",newdata = testData) # dynamic forecasts generated
fcst1.svm=as.timeSeries(fcst1.svm,time(testData))
confusionMatrix(table(Actual=testData[,1],Prediction=fcst1.svm))


