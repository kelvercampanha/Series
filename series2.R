#importanto pacotes
require(dplyr)

#lendo os dados
dados=read.csv('C:/Users/klc/Desktop/Monitoria Séries/petr4_treinamento_ex.csv',sep=",",dec=".")
dados$Anomes=paste(substr(dados$Date,1,4),substr(dados$Date,6,7),sep='')
dados=subset(dados,dados$Ano>=201701)

#Análise Descritiva
plot(as.numeric(as.character(dados$Open)),type = 'l',col='royalblue4',
     xlab="Index",ylab="Valor (R$)",main="Valor de abertura das ações")
boxplot(as.numeric(as.character(dados$Open))~dados$Anomes,main="Boxplot",xlab="",ylab="Valor (R$)",col='royalblue4',las=2)
yt=as.numeric(as.character(dados$Open))
summary(yt)

#Teste para estacionaridade com respeito à média
library(tseries)
adf.test(yt, alternative="stationary", k=0)#A série não é estacionária

#Tirando a diferença se necessario
diferenca1<- diff(yt,lag=1,differences=1)
adf.test(diferenca1, alternative="stationary", k=0)#A série é estacionária

#Identificação do modelo
#Função de Autocorrelação
par(mfrow=c(1,2))
acf(diferenca1,30,plot=F)
acf(diferenca1,30,plot=T,main='Autocorrelação')

#Função de Autocorrelação  Parcial 
pacf(diferenca1,30,plot=T,main='Autocorrelação Parcial')		
pacf(diferenca1,30,plot=F)

#Estimação do modelo
#Componentes da séries
weatherarima <- ts(diferenca1, frequency = 30)
components <- decompose(weatherarima)
components;plot(components,xaxt="n",col="blue")

library(forecast)# auto.arima
fit<-auto.arima(weatherarima, trace=TRUE, test="kpss", ic="bic")
autoArimaModel = auto.arima(yt, d = 1)
autoArimaModel

pdqParam = c(5, 1, 4)
fit <- arima(yt, pdqParam)

autoPred = forecast(fit, h=19)
pred=as.vector(predict(fit,19)$pred)
plot(autoPred,col='royalblue4',ylab='Valor (R$)', main='Arima')

#lendo os dados de teste
dados2=read.csv('C:/Users/klc/Desktop/Monitoria Séries/petr4_teste_ex.csv',sep=",",dec=".")

par(mfrow=c(1,1))
plot(pred,type='l',ylim=c(10,25),col="blue",xlab='Tempo',ylab='Valor',cex=2,lwd=2)
lines(as.numeric(dados2$Open),col="red",lwd=2)
legend('bottomleft', legend=c("Preço real", "Previsões"),
              col=c("red", "blue"), lty=1:1, cex=0.8)

#Verificação do modelo
res<-residuals(fit)
par(mfrow=c(2,3))
hist(res,main='Histograma dos resíduos',ylab='Frequência',xlab='Resíduo')
qqnorm(res,col='royalblue4',pch=19)
qqline(res,col=2)
shapiro.test(res)
ts.plot(res,col='royalblue4',main='Indepedência dos resíduos',xlab='Ordem',ylab='Resíduo')
abline(h=0,col="red")
#plot(as.numeric(yt+res),as.numeric(res),pch=19)
acf(res,12,plot=T,main='Autocorrelação')
pacf(res,12,plot=T,main='Autocorrelação Parcial')		

Box.test(res,type="Ljung-Box",lag=12)

B = NULL
for(i in 1:120)
{
  B = c(B,Box.test(res,lag = i,type = "Ljung-Box")$p.value)
}
plot(B,main = "Ljung-Box tests",ylab = "p-value",xlab = "lag",pch = 16,col='royalblue4',ylim = c(0,1))
abline(h = 0.05,lty = 2,col='blue')
