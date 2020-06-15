#### Ghetti Omar 793181 ####
library(tseries)
library(zoo)
library(PerformanceAnalytics)

####DATA SUMMARY####

###TECNOLOGIA###
###Apple-Tecnologia###
AAPL.df=read.csv("C:/Users/OMAR/Desktop/BISF/AAPL.csv", header=TRUE,stringsAsFactors = FALSE)
td_aapl=as.Date(AAPL.df$Date)
AAPL=zoo(AAPL.df$Adj.Close, order.by = td_aapl)
index(AAPL)=as.yearmon(index(AAPL))

###Telecom Italia-Tecnologia###
TIM.df=read.csv("C:/Users/OMAR/Desktop/BISF/TIT.MI.csv", header = TRUE, stringsAsFactors = FALSE)
td_tim=as.Date(TIM.df$Date)
TIM=zoo(TIM.df$Adj.Close, order.by = td_tim)
index(TIM)=as.yearmon(index(TIM))

###ENERGIA###
###A2A.MI-Energia####
A2A.df=read.csv("C:/Users/OMAR/Desktop/BISF/A2A.MI.csv", header = TRUE, stringsAsFactors = FALSE)
td_a2a=as.Date(A2A.df$Date)
A2A=zoo(A2A.df$Adj.Close, order.by = td_a2a)
index(A2A)=as.yearmon(index(A2A))

###SAIPEM-Energia###
SAIP.df=read.csv("C:/Users/OMAR/Desktop/BISF/SPM.MI.csv", header = TRUE, stringsAsFactors = FALSE)
td_spm=as.Date(SAIP.df$Date)
SAIP=zoo(SAIP.df$Adj.Close, order.by = td_spm)
index(SAIP)=as.yearmon(index(SAIP))

###BANCHE###
###UniCredit-Banche###
UNI.df=read.csv("C:/Users/OMAR/Desktop/BISF/UCG.MI.csv", header = TRUE, stringsAsFactors = FALSE)
td_uni=as.Date(UNI.df$Date)
UNI=zoo(UNI.df$Adj.Close, order.by = td_uni)
index(UNI)=as.yearmon(index(UNI))

###Intesa Sanpaolo-Banche###
ISP.df=read.csv("C:/Users/OMAR/Desktop/BISF/ISP.MI.csv", header = TRUE, stringsAsFactors = FALSE)
td_isp=as.Date(ISP.df$Date)
ISP=zoo(ISP.df$Adj.Close, order.by = td_isp)
index(ISP)=as.yearmon(index(ISP))


###Merge dei dati###
TECH.z=merge(AAPL,TIM)
ENERGY.z=merge(A2A,SAIP)
BANKS.z=merge(UNI,ISP)
DATA.z=merge(TECH.z,ENERGY.z,BANKS.z)
colnames(DATA.z)=c("APPLE", "TIM", "A2A", "SAIPEM", "UNICREDIT", "ISP")

###Plot Dati Raccolti###
plot(DATA.z, main="Chiusure Aggiustate", lwd=2, col = "green")

###DESCRIPTIVE ANALYTICS###

###RETURNS###
###Return settore Tecnologia###
AAPL.ret=diff(AAPL)/lag(AAPL, k=-1) #simple
AAPLcc.ret=diff(log(AAPL)) #compound
TIM.ret=diff(TIM)/lag(TIM, k= -1)
TIMcc.ret=diff(log(TIM))

###Return settore energia###
A2A.ret=diff(A2A)/lag(A2A, k = -1)
A2Acc.ret=diff(log(A2A))
SAIP.ret=diff(SAIP)/lag(SAIP, k = -1)
SAIPcc.ret=diff(log(SAIP))

###Return Settore banche###
UNI.ret=diff(UNI)/lag(UNI, k = -1)
UNIcc.ret=diff(log(UNI))
ISP.ret=diff(ISP)/lag(ISP, k = -1)
ISPcc.ret=diff(log(ISP))

###Return Merge###
TECH.ret=merge(AAPLcc.ret, TIMcc.ret)
ENERGY.ret=merge(A2Acc.ret, SAIPcc.ret)
BANKS.ret=merge(UNIcc.ret, ISPcc.ret)
DATA.ret=merge(TECH.ret,ENERGY.ret,BANKS.ret)
colnames(DATA.ret)=c("APPLE", "TIM", "A2A", "SAIPEM", "UNICREDIT", "ISP")


###plot cc returns###
plot(DATA.ret,main="MonthlyCCReturns",lwd=2,col="darkred",panel=function(...){lines(...);abline(h=0);})

###GRAFICO UNICO PER I RETURNS###
plot(DATA.ret,plot.type="single",main="MonthlyCCReturnsUniqueGraph",
     col=c("darkgreen","darkgray","brown","aquamarine","red","cyan"),
     lty=c("solid","solid","solid","solid","solid","solid"),
     lwd=2,ylab="CCReturns")
abline(h=0)
legend(x="topright",legend=c("APPLE","TIM","A2A","SAIPEM", "UNICREDIT", "ISP" ),
       lty=c("solid","solid","solid","solid","solid","solid"),
       lwd=1,
       cex = 0.6,
       col=c("darkgreen","darkgray","brown","aquamarine","red","cyan"))
###grafico tecnologia###
plot(TECH.ret, plot.type = "single", main = "TechCCReturns",
     col = c("yellow","blue"), 
     lty = c("solid", "dotted"), 
     lwd=2, ylab = "CCReturns")
abline(h=0)
legend(x="topleft",legend = c("APPLE", "TIM"),
       lty = c("solid", "dotted"),
       lwd=1,
       cex=0.6,
       col=c("yellow", "blue"))
###grafico energia###
plot(ENERGY.ret, plot.type = "single", main = "ENERGYCCReturns",
     col = c("yellow","blue"), 
     lty = c("solid", "dotted"), 
     lwd=2, ylab = "CCReturns")
abline(h=0)
legend(x="topleft",legend = c("A2A", "SAIPEM"),
       lty = c("solid", "dotted"),
       lwd=1,
       cex=0.6,
       col=c("yellow", "blue"))
###grafico banche###
plot(BANKS.ret, plot.type = "single", main = "BANKSCCReturns",
     col = c("yellow","blue"), 
     lty = c("solid", "dotted"), 
     lwd=2, ylab = "CCReturns")
abline(h=0)
legend(x="topleft",legend = c("UNICREDIT", "ISP"),
       lty = c("solid", "dotted"),
       lwd=1,
       cex=0.6,
       col=c("yellow", "blue"))

###cONVERSIONE ZOO-MATRIX###
TECH.ret.mat=coredata(TECH.ret)
ENERGY.ret.mat=coredata(ENERGY.ret)
BANKS.ret.mat=coredata(BANKS.ret)
DATA.ret.mat=coredata(DATA.ret)

###ISTOGRAMMI NON SCALATI###
##TECH##
par(mfrow=c(2,1))
hist(TECH.ret.mat[,1], main = "APPLE",xlab = "returns", probability = TRUE, col="blue")
hist(TECH.ret.mat[,2], main = "TIM", xlab="returns",  probability = TRUE, col = "yellow")
par(mfrow=c(1,1))
##ENERGY##
par(mfrow=c(2,1))
hist(ENERGY.ret.mat[,1], main="A2A", xlab = "returns", probability = TRUE, col = "red")
hist(ENERGY.ret.mat[,2], main = "SAIPEM", xlab = "returns", probability = TRUE, col="cyan")
par(mfrow=c(1,1))
##BANKS##
par(mfrow=c(2,1))
hist(BANKS.ret.mat[,1], main = "UNICREDIT", xlab = "returns", probability = TRUE, col = "brown")
hist(BANKS.ret.mat[,2], main = "ISP", xlab ="returns", probability = TRUE, col = "black")
par(mfrow=c(1,1))

###ISTOGRAMMI SCALATI PER SETTORE###
##TECH##
intvls=seq(-0.4,0.4,0.05)
par(mfrow=c(2,1))
hist(TECH.ret.mat[,1], main = "APPLE",xlab = "returns", probability = TRUE, col="blue", breaks=intvls)
hist(TECH.ret.mat[,2], main = "TIM", xlab="returns",  probability = TRUE, col = "yellow",breaks = intvls)
par(mfrow=c(1,1))
##ENERGY##
intvls=seq(-0.6,0.3,0.05)
par(mfrow=c(2,1))
hist(ENERGY.ret.mat[,1], main="A2A", xlab = "returns", probability = TRUE, col = "red",breaks = intvls)
hist(ENERGY.ret.mat[,2], main = "SAIPEM", xlab = "returns", probability = TRUE, col="cyan", breaks = intvls)
par(mfrow=c(1,1))
##BANKS##
intvls=seq(-2, 2, 0.08)
par(mfrow=c(2,1))
hist(BANKS.ret.mat[,1], main = "UNICREDIT", xlab = "returns", probability = TRUE, col = "brown",breaks = intvls)
hist(BANKS.ret.mat[,2], main = "ISP", xlab ="returns", probability = TRUE, col = "black", breaks = intvls)
par(mfrow=c(1,1))

###DIAGNOSTIC PLOTS DIVISI PER TITOLO###

###APPLE###
dens=density(na.omit(TECH.ret.mat[,1]))
par(mfrow=c(2,2))
hist(TECH.ret.mat[,1], main= paste("APPLE","monthly cc returns"), probability=T,xlab ="returns",  ylab="Density", col="red")
boxplot(TECH.ret.mat[,1], main=paste("Boxplot"), ylab="cc return", col="red")
plot(dens,type="l",xlab="cc return", col="blue", lwd=2, ylab="density estimate", main="Smoothed density")
qqnorm(TECH.ret.mat[,1], col="blue")
qqline(TECH.ret.mat[,1])
par(mfrow=c(1,1))
###TIM###
par(mfrow=c(2,2))
hist(TECH.ret.mat[,2], main= paste("TIM","monthly cc returns"), probability=T,xlab ="returns",  ylab="Density", col="red")
boxplot(TECH.ret.mat[,2], main=paste("Boxplot"), ylab="cc return", col="red")
plot(density(na.omit(TECH.ret.mat[,2])),type="l",xlab="cc return", col="blue", lwd=2, ylab="density estimate", main="Smoothed density")
qqnorm(TECH.ret.mat[,2], col="blue")
qqline(TECH.ret.mat[,2])
par(mfrow=c(1,1))
###A2A###
par(mfrow=c(2,2))
hist(ENERGY.ret.mat[,1], main= paste("A2A","monthly cc returns"), probability=T,xlab ="returns",  ylab="Density", col="red")
boxplot(ENERGY.ret.mat[,1], main=paste("Boxplot"), ylab="cc return", col="red")
plot(density(na.omit(ENERGY.ret.mat[,1])),type="l",xlab="cc return", col="blue", lwd=2, ylab="density estimate", main="Smoothed density")
qqnorm(ENERGY.ret.mat[,1], col="blue")
qqline(ENERGY.ret.mat[,1])
par(mfrow=c(1,1))
###SAIPEM###
par(mfrow=c(2,2))
hist(ENERGY.ret.mat[,2], main= paste("SAIPEM","monthly cc returns"), probability=T,xlab ="returns",  ylab="Density", col="red")
boxplot(ENERGY.ret.mat[,2], main=paste("Boxplot"), ylab="cc return", col="red")
plot(density(na.omit(ENERGY.ret.mat[,2])),type="l",xlab="cc return", col="blue", lwd=2, ylab="density estimate", main="Smoothed density")
qqnorm(ENERGY.ret.mat[,2], col="blue")
qqline(ENERGY.ret.mat[,2])
par(mfrow=c(1,1))
###UNICREDIT###
par(mfrow=c(2,2))
hist(BANKS.ret.mat[,1], main= paste("UNICREDIT","monthly cc returns"), probability=T,xlab ="returns",  ylab="Density", col="red")
boxplot(BANKS.ret.mat[,1], main=paste("Boxplot"), ylab="cc return", col="red")
plot(density(na.omit(BANKS.ret.mat[,1])),type="l",xlab="cc return", col="blue", lwd=2, ylab="density estimate", main="Smoothed density")
qqnorm(BANKS.ret.mat[,1], col="blue")
qqline(BANKS.ret.mat[,1])
par(mfrow=c(1,1))
###ISP###
par(mfrow=c(2,2))
hist(BANKS.ret.mat[,2], main= paste("ISP","monthly cc returns"), probability=T,xlab ="returns",  ylab="Density", col="red")
boxplot(BANKS.ret.mat[,2], main=paste("Boxplot"), ylab="cc return", col="red")
plot(density(na.omit(BANKS.ret.mat[,2])),type="l",xlab="cc return", col="blue", lwd=2, ylab="density estimate", main="Smoothed density")
qqnorm(BANKS.ret.mat[,2], col="blue")
qqline(BANKS.ret.mat[,2])
par(mfrow=c(1,1))

###UNIVARIATE DESCRIPTIVE STATISTICS###

##mean##
mean(na.omit(TECH.ret.mat[,1]))
mean(na.omit(TECH.ret.mat[,2]))
mean(ENERGY.ret.mat[,1])
mean(ENERGY.ret.mat[,2])
mean(BANKS.ret.mat[,1])
mean(BANKS.ret.mat[,2])
##variance##
var(na.omit(TECH.ret.mat[,1]))
var(na.omit(TECH.ret.mat[,2]))
var(ENERGY.ret.mat[,1])
var(ENERGY.ret.mat[,2])
var(BANKS.ret.mat[,1])
var(BANKS.ret.mat[,2])
##standart deviation##
sd(na.omit(TECH.ret.mat[,1]))
sd(na.omit(TECH.ret.mat[,2]))
sd(ENERGY.ret.mat[,1])
sd(ENERGY.ret.mat[,2])
sd(BANKS.ret.mat[,1])
sd(BANKS.ret.mat[,2])
##skewness##
skewness(TECH.ret.mat[,1])
skewness(TECH.ret.mat[,2])
skewness(ENERGY.ret.mat[,1])
skewness(ENERGY.ret.mat[,2])
skewness(BANKS.ret.mat[,1])
skewness(BANKS.ret.mat[,2])
##kurtosis##
kurtosis(TECH.ret.mat[,1])
kurtosis(TECH.ret.mat[,2])
kurtosis(ENERGY.ret.mat[,1])
kurtosis(ENERGY.ret.mat[,2])
kurtosis(BANKS.ret.mat[,1])
kurtosis(BANKS.ret.mat[,2])

###covariance and correlations##
cov(na.omit(DATA.ret.mat))
cor(na.omit(DATA.ret.mat))


###scatterplot###
pairs(DATA.ret.mat, col= "red", pch=18, cex=1.5, cex.axis=1.5)

###FORECASTING###
library(forecast)
##APPLE##
retTrains=AAPL[1:(0.6*length(AAPL))] #training data
retTesting=AAPL[(0.6*length(AAPL)):(0.8*length(AAPL))] #testing data
fit=arima(retTrains, order = c(4,0,2))
predicts=predict(fit, n.ahead = (length(AAPL)-(0.6*length(AAPL))))$pred
fcast=forecast(fit, h = 25)
plot(fcast, main="ARIMA forecast for apple")
accuracy(predicts,retTesting)
lines(retTesting)
##TIM##
retTrains=TIM[1:(0.6*length(TIM))] #training data
retTesting=TIM[(0.6*length(TIM)):(0.8*length(TIM))] #testing data
fit=arima(retTrains, order = c(3,0,4))
predicts=predict(fit, n.ahead = (length(TIM)-(0.6*length(TIM))))$pred
fcast=forecast(fit, h = 25)
plot(fcast, main="ARIMA forecast for TIM")
accuracy(predicts,retTesting)
lines(retTesting)
##A2A##
retTrains=A2A[1:(0.6*length(A2A))] #training data
retTesting=A2A[(0.6*length(A2A)):(0.8*length(A2A))] #testing data
fit=arima(retTrains, order = c(2,0,2))
predicts=predict(fit, n.ahead = (length(A2A)-(0.6*length(A2A))))$pred
fcast=forecast(fit, h = 25)
plot(fcast, main="ARIMA forecast for A2A")
accuracy(predicts,retTesting)
lines(retTesting)
##SAIPEM##
retTrains=SAIP[1:(0.6*length(SAIP))] #training data
retTesting=SAIP[(0.6*length(SAIP)):(0.8*length(SAIP))] #testing data
fit=arima(retTrains, order = c(3,0,3))
predicts=predict(fit, n.ahead = (length(SAIP)-(0.6*length(SAIP))))$pred
fcast=forecast(fit, h = 25)
plot(fcast, main="ARIMA forecast for SAIPEM")
accuracy(predicts,retTesting)
lines(retTesting)
##UNICREDIT##
retTrains=UNI[1:(0.6*length(UNI))] #training data
retTesting=UNI[(0.6*length(UNI)):(0.8*length(UNI))] #testing data
fit=arima(retTrains, order = c(2,0,3))
predicts=predict(fit, n.ahead = (length(UNI)-(0.6*length(UNI))))$pred
fcast=forecast(fit, h = 25)
plot(fcast, main="ARIMA forecast for UNICREDIT")
accuracy(predicts,retTesting)
lines(retTesting)
##ISP##
retTrains=ISP[1:(0.6*length(ISP))] #training data
retTesting=ISP[(0.6*length(ISP)):(0.8*length(ISP))] #testing data
fit=arima(retTrains, order = c(2,0,4))
predicts=predict(fit, n.ahead = (length(ISP)-(0.6*length(ISP))))$pred
fcast=forecast(fit, h = 25)
plot(fcast, main="ARIMA forecast for ISP")
accuracy(predicts,retTesting)
lines(retTesting)

###EOF###

