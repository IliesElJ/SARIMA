#Chargement des donn�es
path <- "C:/Users/Hermes/Documents/ENSAE/SerieTemp"
setwd(path)

#I.
data <- read.csv("valeurs_mensuelles.csv",sep = ";")
View(data)

require(zoo)
require(tseries)
library(caschrono)
require(FitAR)
require(lmtest)
require(forecast)
#Transformation en ST:
data = zoo(x = data[[2]])
data = data[4:length(data)]
data=ts(as.numeric(data))

par(mfrow=c(1,1))
plot(log(data))
plot(data, main="IPI - Extraction d'hydrocarbures")
acf(data,lag.max = 60)

m<-decompose(data)
plot(m)
#analyse des saisonnlit�s:
acf(data,lag.max=50)
# On observe des fortes autocorr�lations aux ordres
# multiples de 12. On tente de supprimer la saison-
# nalit� apparente en diff�renciant � l'ordre 12.
ldata = log(data)
plot(ldata)

x <- ldata
y <- ldata - lag(ldata,-1)


plot(y)
acf(y,lag.max=30)
pacf(y,lag.max=30)



adf.test(y)
# pvalue < 0.01 donc on rejette � 5% l'hypoth�se nulle 
# de racine unitaire.
# On dira que la s�rie est stationnaire.
pp.test(y)
# pvalue < 0.01 donc on rejette � 5% l'hypoth�se nulle 
# de racine unitaire.
# On dira que la s�rie est stationnaire.
kpss.test(y)



#II.
par(mfrow=c(1,2))
acf(y,50)
pacf(y,50)

i= 1
#i sert ici d'indice sur l'ensemble des mod�les significatifs � 5%
m = list() 
#m contiendra les vecteurs des param�tres (p,d,q) des mod�les significatifs � 5%

for(p in c(0,1,2,3,4,5,6)){
  for (q in c(0,1,2,3)){
    
    #Certains param�tres ne s'appliquent � cause d'absences de convergence pour les estimateurs d'o� l'utilisation de "try"
    try(model <- arima(x=x, order=c(p,1,q), method="CSS-ML"))


    if(p>=1|q>=1)
      
      {
      #coeftest nous renvois un tableau contenant les coefficients du mod�le et les z-tests correspondants
      coef <- coeftest(model)
      #On v�rifie ici que la significativit� 
      try(if (max(coef[,4])<0.05)
        {m[[i]] = (c(p,1,q))
        i=i+1})
    }
    
  }
}

#Nous v�rifions maintenant la blancheur des r�sidus pour chaque mod�le s�lectionn� pr�cedemment
#Le lag maximal choisi est de ici 50 

j=1
m2 = list()


for(s in m){
  
  try(model <- arima(x=x, order=s, method="CSS-ML"))
  i=1
  pvals=list()
  while(i<51){
    t=Box.test(model$residuals,lag = i)
  pvals[[i]]=t$p.value
  i=i+1}
  
  if(min(pvals>0.05)){m2[[j]]=s
  j=j+1}
  
}

#3 mod�les semblent significatifs et poss�dent des r�sidus blancs
#La selection s'effectue en utilisant les crit�res d'information AIC et BIC
#Notre choix se portera sur le mod�le minimisant les deux (s'il en existe un)


#Nous pouvons effectuer un test de normalit� comme celui de Shapiro-Wilk pour observer la normalit� des r�sidus 


model <- arima(x=x, order=m2[[1]], method="CSS-ML")
AIC(model)
BIC(model)
model <- arima(x=x, order=m2[[2]], method="CSS-ML")
AIC(model)
BIC(model)
model <- arima(x=x, order=m2[[3]], method="CSS-ML")
AIC(model)
BIC(model)

#Le mod�le  les deux crit�res d'information est l'AR(3) pour notre s�rie stationnires Xt
#Nous utiliserons donc celui dans la suite de l'�tude d�di�e � la pr�diction de Y_{t+1} et Y_{t+2}



#Observons les caract�ristiques des r�sidus 

par(mfrow=c(1,1))
plot(model$residuals)
title("R�sidus")
qqnorm(model$residuals)
qqline(model$residuals)
plot(LBQPlot(model$residuals,lag.max = 50))
plot(density(model$residuals))

#Tests de normalit�

shapiro.test(model$residuals)
install.packages('nortest')
library(nortest)
set.seed(1)
ad.test(model$residuals)

#III.

forecast(model,2)
plot(forecast(model,2),xlim=c(330,400),main="Pr�dictions et intervalles de confiance en T+1 et T+2")

