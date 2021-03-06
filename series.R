#importanto pacotes
require(dplyr)

#lendo os dados
dados=read.csv('C:/Users/klc/Desktop/Monitoria S�ries/dados_climaticos_bh.csv',sep=";",dec=",")

#Criando vari�vel de refer�ncia mmyyyy
dados$ref=paste(substr(dados$data,7,10),substr(dados$data,4,5),sep="")

#criando novo dataset com as vari�veis sumarizadas por ano m�s
dados_mes=dados %>% 
  group_by(ref) %>% 
  summarise(temperatura=mean(tMax)) %>% 
  as.data.frame()

#criando uma vari�vel para o ano
dados_mes$ano=paste(substr(dados_mes$ref,1,4))

#plot da m�dia da temperatura m�xima mensal em BH
label=NULL
for (i in 1:nrow(dados_mes))
{
  label[i]=ifelse(i%%6==1,dados_mes$ref[i],'')
}
plot(dados_mes$temperatura,type = 'l',col='royalblue4', xaxt="n",
     xlab="Ano/M�s",ylab="Temperatura (�C)",main="Temperatura m�xima em BH (m�dia mensal)")
axis(side=1,at=seq(1,132),labels=label,las=2,cex.axis=0.7)
points(dados_mes$temperatura,pch=19,col='royalblue4')
abline(v=c(1,(seq(1,10,1)*12)+1),col="lightblue",lty=2)

yt=dados_mes$temperatura[1:(nrow(dados_mes)-12)]

#An�lise descritiva
summary(yt)
#boxplot(yt,main="Boxplot Temperatura",xlab="",ylab="Temperatura (�C)",col='royalblue4')
boxplot(dados_mes$temperatura~dados_mes$ano,main="Boxplot Temperatura",xlab="",ylab="Temperatura (�C)",col='royalblue4',las=2)
#qqnorm(yt,col='royalblue4',pch=19)
#qqline(yt,col=2)
shapiro.test(yt)

#Teste para estacionaridade com respeito � m�dia
library(tseries)
adf.test(yt, alternative="stationary", k=0)

#Tirando a diferen�a se necessario
diferenca1<- diff(yt,lag=1,differences=1)

#Identifica��o do modelo
#Fun��o de Autocorrela��o
par(mfrow=c(1,2))
acf(yt,12,plot=F)
acf(yt,12,plot=T,main='Autocorrela��o')

#Fun��o de Autocorrela��o  Parcial 
pacf(yt,12,plot=T,main='Autocorrela��o Parcial')		
pacf(yt,12,plot=F)

#Estima��o do modelo
#Componentes da s�ries
weatherarima <- ts(yt, frequency = 12)
components <- decompose(weatherarima)
components;plot(components,xaxt="n",col="blue")

library(forecast)# auto.arima
fit<-auto.arima(weatherarima, trace=TRUE, test="kpss", ic="bic")
autoArimaModel = auto.arima(yt, d = 0)
autoArimaModel

pdqParam = c(2, 0, 0)# ajuste manual
PDQParam = c(2, 1, 0)
fit <- arima(yt, pdqParam, seasonal = list(order = PDQParam, period = 12))

autoPred = forecast(fit, h=12)
plot(autoPred,col='royalblue4', xaxt="n",ylab='Temperatura (�C)', main='Arima(2,0,0)(2,1,0)[12]')
axis(side=1,at=seq(1,132),labels=label,las=2,cex.axis=0.7)
points(dados_mes$temperatura,pch=19,col='royalblue4')
lines(seq(1,132,1),c(dados_mes$temperatura[1:132]),type='l',col='royalblue4')
abline(v=c(1,(seq(1,10,1)*12)+1),lty=2,col='royalblue4')

#fit<-arima0(yt, order = c(9, 1, 5))
#previsoes=predict(fit, n.ahead = 12)
#plot(dados_mes$temperatura,type = 'l',col='royalblue4', xaxt="n",
#     xlab="Ano/M�s",ylab="Temperatura (�C)",main="Temperatura m�xima em BH (m�dia mensal)")
#axis(side=1,at=seq(1,132),labels=label,las=2,cex.axis=0.7)
#points(dados_mes$temperatura,pch=19,col='royalblue4')
#abline(v=c(1,(seq(1,10,1)*12)+1),col="lightblue",lty=2)
#lines(seq(1,132,1),c(dados_mes$temperatura[1:120]+residuals(fit),previsoes$pred),type='l',col='red')

#Verifica��o do modelo
res<-residuals(fit)
par(mfrow=c(2,3))
hist(res,main='Histograma dos res�duos',ylab='Frequ�ncia',xlab='Res�duo')
qqnorm(res,col='royalblue4',pch=19)
qqline(res,col=2)
shapiro.test(res)
ts.plot(res,col='royalblue4',main='Indeped�ncia dos res�duos',xlab='Ordem',ylab='Res�duo')
abline(h=0,col="red")
#plot(as.numeric(yt+res),as.numeric(res),pch=19)
acf(res,12,plot=T,main='Autocorrela��o')
pacf(res,12,plot=T,main='Autocorrela��o Parcial')		

Box.test(res,type="Ljung-Box",lag=12)

B = NULL
for(i in 1:120)
{
  B = c(B,Box.test(res,lag = i,type = "Ljung-Box")$p.value)
}
plot(B,main = "Ljung-Box tests",ylab = "p-value",xlab = "lag",pch = 16,col='royalblue4',ylim = c(0,1))
abline(h = 0.05,lty = 2,col='blue')
