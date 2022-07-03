#Chargement des données
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
#analyse des saisonnlités:
acf(data,lag.max=50)
# On observe des fortes autocorrélations aux ordres
# multiples de 12. On tente de supprimer la saison-
# nalité apparente en différenciant à l'ordre 12.
ldata = log(data)
plot(ldata)

x <- ldata
y <- ldata - lag(ldata,-1)


plot(y)
acf(y,lag.max=30)
pacf(y,lag.max=30)



adf.test(y)
# pvalue < 0.01 donc on rejette à 5% l'hypothèse nulle 
# de racine unitaire.
# On dira que la série est stationnaire.
pp.test(y)
# pvalue < 0.01 donc on rejette à 5% l'hypothèse nulle 
# de racine unitaire.
# On dira que la série est stationnaire.
kpss.test(y)



#II.
par(mfrow=c(1,2))
acf(y,50)
pacf(y,50)

i= 1
#i sert ici d'indice sur l'ensemble des modèles significatifs à 5%
m = list() 
#m contiendra les vecteurs des paramètres (p,d,q) des modèles significatifs à 5%

for(p in c(0,1,2,3,4,5,6)){
  for (q in c(0,1,2,3)){
    
    #Certains paramètres ne s'appliquent à cause d'absences de convergence pour les estimateurs d'où l'utilisation de "try"
    try(model <- arima(x=x, order=c(p,1,q), method="CSS-ML"))


    if(p>=1|q>=1)
      
      {
      #coeftest nous renvois un tableau contenant les coefficients du modèle et les z-tests correspondants
      coef <- coeftest(model)
      #On vérifie ici que la significativité 
      try(if (max(coef[,4])<0.05)
        {m[[i]] = (c(p,1,q))
        i=i+1})
    }
    
  }
}

#Nous vérifions maintenant la blancheur des résidus pour chaque modèle sélectionné précedemment
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

#3 modèles semblent significatifs et possèdent des résidus blancs
#La selection s'effectue en utilisant les critères d'information AIC et BIC
#Notre choix se portera sur le modèle minimisant les deux (s'il en existe un)


#Nous pouvons effectuer un test de normalité comme celui de Shapiro-Wilk pour observer la normalité des résidus 


model <- arima(x=x, order=m2[[1]], method="CSS-ML")
AIC(model)
BIC(model)
model <- arima(x=x, order=m2[[2]], method="CSS-ML")
AIC(model)
BIC(model)
model <- arima(x=x, order=m2[[3]], method="CSS-ML")
AIC(model)
BIC(model)

#Le modèle  les deux critères d'information est l'AR(3) pour notre série stationnires Xt
#Nous utiliserons donc celui dans la suite de l'étude dédiée à la prédiction de Y_{t+1} et Y_{t+2}



#Observons les caractéristiques des résidus 

par(mfrow=c(1,1))
plot(model$residuals)
title("Résidus")
qqnorm(model$residuals)
qqline(model$residuals)
plot(LBQPlot(model$residuals,lag.max = 50))
plot(density(model$residuals))

#Tests de normalité

shapiro.test(model$residuals)
install.packages('nortest')
library(nortest)
set.seed(1)
ad.test(model$residuals)

#III.

forecast(model,2)
plot(forecast(model,2),xlim=c(330,400),main="Prédictions et intervalles de confiance en T+1 et T+2")

