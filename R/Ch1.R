Sys.setlocale(category = "LC_ALL", locale = "English_United States.1252")
y=rnorm(200)
ts(y,start=c(1990,1),freq=12)
ts(y,start=c(1990,1),freq=4)
ts(y, end=2024,freq=1)

dat0=read.csv("data/GDP.csv")
timestamp=as.Date(dat0[,1])
y1=timeSeries::timeSeries(dat0[,-1],timestamp)
y2=xts::xts(dat0[,-1],timestamp)
y3=zoo::zoo(as.matrix(dat0[,-1]),timestamp)
y4=ts(dat,start=c(1947,1),freq=4)

dev.new();par(mfrow=c(2,2)) #Figure 1.2
plot(y1,xlab="",ylab="",main="(A) timeSeries()",col="steelblue");grid()
plot(y2,xlab="",ylab="",main="(B) xts()",col="steelblue");grid()
plot(y3,xlab="",ylab="",main="(C) zoo()",col="steelblue");grid()
plot(y4,xlab="",ylab="",main="(D) ts()",col="steelblue");grid()
par(mfrow=c(1,1))


dat0=read.csv("data/DJI.csv")
timestamp=as.Date(dat0[,1])
Y=dat0[,4]
y1=timeSeries::timeSeries(Y,timestamp)
y2=xts::xts(Y,timestamp)
y3=zoo::zoo(as.matrix(Y),timestamp)

dev.new();par(mfrow=c(2,2)) #Figure 1.4
plot(y1,xlab="",ylab="",main="(A) timeSeries()",col="steelblue");grid()
plot(y2,xlab="",ylab="",main="(B) xts()",col="steelblue");grid()
plot(y3,xlab="",ylab="",main="(C) zoo()",col="steelblue");grid()
par(mfrow=c(1,1))




quantmod::getSymbols(c("^DJI","^GSPC","^N225"),from = "1980-01-04")

quantmod::getFX("EUR/USD",src='oanda')
EURUSD

quantmod::getMetals(c("XAU", "silver")) #Gold and silver
cbind(XAUUSD,XAGUSD)

quantmod::getSymbols('CPIAUCNS',src='FRED')
CPIAUCNS

cpi_m=JFE::getFed("CPIAUCNS")
cpi_q=JFE::getFed("CPIAUCNS",freq="Quarterly")
cpi_a=JFE::getFed("CPIAUCNS",freq="Annual")

timeSeries::as.timeSeries(cpi_m)
xts::as.xts(cpi_m,order.by = as.Date(rownames(cpi_m)))

f1=JFE::getFed("DEXUSEU",freq="Daily")
f1=xts::as.xts(f1, order.by=as.Date(rownames(f1)))
f1["2024-08-05"]

#US EER, Daily Nominal and Broad
DNB_urls="https://stats.bis.org/api/v2/data/dataflow/BIS/WS_EER/1.0/D.N.B.US?format=csv"
DNB=read.csv(neer_urls)[,c("TIME_PERIOD","OBS_VALUE")]
DNB_us=xts::as.xts(DNB[-1],
                   order.by=as.Date(DNB[,1]))
colnames(DNB_us)="US"
DNB_us=na.omit(DNB_us)
dev.new();plot(DNB_us)

#G20 EER, Daily, Nominal and Broad
main="https://stats.bis.org/api/v2/data/dataflow/BIS/WS_EER/1.0/D.N.B."
end="?format=csv"
areas=c("AR","AU","BR","CA","CN","DE","FR","GB","ID","IN","IT","JP","KR","MX","RU","SA","TR","US","ZA")
AREA=paste0(main, areas, end)
DNB_G20=NULL
for (i in AREA) {
tmp=read.csv(i)[,c("TIME_PERIOD","OBS_VALUE")]

tmp=xts::as.xts(tmp[-1],
                   order.by=as.Date(tmp[,1]))
DNB_G20=cbind(DNB_G20, tmp)
print(i)
}
colnames(DNB_G20)=areas


#Bilateral exchange rate
fx_urls="https://stats.bis.org/api/v2/data/dataflow/BIS/WS_XRU/1.0/D.XM.EUR.A?format=csv"
EuroUSD=read.csv(fx_urls)[,c("TIME_PERIOD","OBS_VALUE")]
EuroUSD=xts::as.xts(EuroUSD[,-1],
                    order.by=as.Date(EuroUSD[,1]))
colnames(EuroUSD)="EuroUSD"


output=JFE::getEER(Areas=c("US","JP"),Freq="Monthly",Type="Real",Basket="Broad")
head(output)



# JFE::getFrench.Factors

Factors_filenames=c(
  "F-F_Research_Data_Factors_weekly",
  "F-F_Research_Data_Factors_daily",
  "F-F_Research_Data_5_Factors_2x3_daily",
  "F-F_Momentum_Factor_daily",
  "F-F_Research_Data_Factors",
  "F-F_Research_Data_5_Factors_2x3",
  "F-F_Momentum_Factor"
) 

output1=JFE::getFrench.Factors(filename="F-F_Research_Data_Factors")
output1$table.names

head(output1$data$`Monthly Factors`)
tail(output1$data$`Monthly Factors`)
output1$data$`Annual Factors: January-December`

output$file.name

# JFE::getFrench.Portfolios
Portfolios_filenames=c(
  "Portfolios_Formed_on_ME",#Portfolios Formed on Size
  "Portfolios_Formed_on_BE-ME",#Portfolios Formed on Book-to-Market 
  "Portfolios_Formed_on_OP", #Portfolios Formed on Operating Profitability
  "Portfolios_Formed_on_INV",#Portfolios Formed on Investment
  "6_Portfolios_2x3",        #6 Portfolios Formed on Size and Book-to-Market
  "25_Portfolios_5x5",       #25 Portfolios Formed on Size and Book-to-Market
  "100_Portfolios_10x10",    #100 Portfolios Formed on Size and Book-to-Market
  "6_Portfolios_ME_INV_2x3", #6 Portfolios Formed on Size and Investment 
  "25_Portfolios_ME_INV_5x5",#25 Portfolios Formed on Size and Investment 
  "100_Portfolios_ME_INV_10x10",#100 Portfolios Formed on Size and Investment 
  "25_Portfolios_BEME_OP_5x5",#25 Portfolios Formed on Book-to-Market and Operating Profitability
  "25_Portfolios_BEME_INV_5x5",#25 Portfolios Formed on Book-to-Market and Investment
  "25_Portfolios_OP_INV_5x5",#25 Portfolios Formed on Operating Profitability and Investment
  "32_Portfolios_ME_BEME_OP_2x4x4",#32 Portfolios Formed on Size, Book-to-Market, and Operating Profitability
  "32_Portfolios_ME_BEME_INV_2x4x4",#32 Portfolios Formed on Size, Book-to-Market, and Investment
  "32_Portfolios_ME_OP_INV_2x4x4",#32 Portfolios Formed on Size, Operating Profitability, and Investment 
  paste0(c(5,10,12,17,30,38,48,49),"_Industry_Portfolios"), # Industry Portfolios
  paste0(c(5,10,12,17,30,38,48,49),"_Industry_Portfolios_daily")
) 

output2=JFE::getFrench.Portfolios(filename="Portfolios_Formed_on_ME")

dim(output2$data[[1]])
head(output2$data[[1]])
tail(output2$data[[1]])
output2$table.names
output2$file.name




