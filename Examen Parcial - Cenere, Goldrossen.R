#############################################PARCIAL RIESGOS##############################################

###############################################EJERCICIO 1################################################

install.packages("ROCR")
install.packages("e1071")

library(readxl)
library(urca)
library(ggplot2)
library(corrplot)
library(caTools)
library(MASS)
library(forecast)
library(gets)
library(dplyr)
library(caret)
library(e1071)
library(caTools)
library(ROCR)

#Import Data
setwd("/Users/Lucas/Desktop/Tp Riesgos")
data <- read_excel("data_ex1.xlsx")

#EJERCICIO A#

###1###

#Missing Values. 
sum(is.na(data$DURATION))
sum(is.na(data$AMOUNT))
summary(data$DURATION)
summary(data$AMOUNT)
#No hay missing values en ninguna. 

#Outliers
summary(data$DURATION)
summary(data$AMOUNT)

boxplot(data$DURATION, plot = FALSE)$out
boxplot(data$AMOUNT, plot = FALSE)$out
boxplot(data$DURATION, xlab ="Duration",ylab = "Months")
boxplot.stats(data$DURATION,coef = 2)
boxplot(data$AMOUNT, xlab ="Amount",ylab = "Dollars")
boxplot.stats(data$AMOUNT,coef = 2)

isat(data$DURATION, iis=TRUE, sis=FALSE, tis=FALSE, t.pval=0.01, plot = TRUE)
isat(data$AMOUNT, iis=TRUE, sis=FALSE, tis=FALSE, t.pval=0.01, plot = TRUE)

#Vemos que todos los valores que se ubican por debajo del q1 o por encima del q3 son considerados outliers. 
#En el caso de DURATION el q1 es igual a 12 meses y el q3 es 24 meses. Encontramos 70 observaciones por encima de los 24 meses.
#En el caso de AMOUNT el q1 es igual a 1366(miles) de dolares mientras que el q3 es igual a 3972(miles) de dolares. Nos encontramos con 72 observaciones por encima de los 3972(miles) de dolares.


###2###

#Histogram
qplot(data$DURATION, geom = 'histogram')
qplot(data$AMOUNT, geom = 'histogram')


###3###

plot(data$DURATION, data$RESPONSE)
plot(data$AMOUNT, data$RESPONSE)
#No observamos que haya mucha variaci??n en cuanto a la relaci??n de credit score/duration o credit score/amount. 
#En cuanto a la duration, podemos ver que se concentrar mayores datos en cr??ditos otorgados con menores duraciones, pero entre ellos no se percibe una relaci??n muy significiativa para aquellos que son considerados buenos cr??ditos o malos cr??ditos. Es decir que no es un parametreo para definir si de acuerdo a la duracion del credito el credit score es bueno o malo. 
#En cuanto al amount, lo ??nico que podemos decir es que se otorgan mas cr??ditos de bajas cantidades, pero, al igual que con la duration, no podemos determinar que haya una relaci??n precisa entre el amount del cr??dito ortorgado y su credit score.


#EJERCICIO B#

###4###

set.seed(99)
index = sample(1:1000, size = trunc(.6*1000))
train = data[index,]
test = data[-index,]


###5###

trainB = data.frame(train$DURATION, train$AMOUNT, train$RESPONSE)
colnames(trainB) = c("DURATION", "AMOUNT", "RESPONSE")

#Good (A). Calculo las medias de los creditos buenos. 
meanX1A <- with(trainB,mean(DURATION[RESPONSE==1]))
meanX2A <- with(trainB,mean(AMOUNT[RESPONSE==1]))
varianceX1A <- with(trainB,var(DURATION[RESPONSE==1])) 
varianceX2A <- with(trainB,var(AMOUNT[RESPONSE==1]))
covX1X2A <- with(trainB,cov(cbind(DURATION[RESPONSE==1],AMOUNT[RESPONSE==1])))

#Bad (B). Calculo la media de los creditos malos. 
meanX1B <- with(trainB,mean(DURATION[RESPONSE==0]))
meanX2B <- with(trainB,mean(AMOUNT[RESPONSE==0]))
varianceX1B <- with(trainB,var(DURATION[RESPONSE==0]))
varianceX2B <- with(trainB,var(AMOUNT[RESPONSE==0]))
covX1X2B <- with(trainB,cov(cbind(DURATION[RESPONSE==0],AMOUNT[RESPONSE==0])))

# Group diff. Calculo la diferencia de Xa Y Xb y los pongo en una matriz. 
diffX1 <- meanX1A-meanX1B
diffX2 <- meanX2A-meanX2B
diffMatrix <- matrix(c(diffX1,diffX2),nrow = 2, ncol = 1,byrow = TRUE)
diffMatrix

#Numero de observaciones de cada grupo. 
nA<-length(trainB$DURATION[trainB$RESPONSE==1]) #420
nB<-length(trainB$DURATION[trainB$RESPONSE==0]) #180

#Calculo la matriz de varianzas y covarianzas de ambos grupos y saco su inversa. 
wcovX1X2 <- (covX1X2A*(nA-1)+covX1X2B*(nB-1))/(nA+nB-2)
IWMatrix  <-ginv(wcovX1X2)
IWMatrix

#Calculo el gamma.  
Gamma <- IWMatrix%*%diffMatrix
Gamma

# Saco el score. 
trainB$zi <- trainB$DURATION*Gamma[1, 1]+trainB$AMOUNT*Gamma[2, 1]
mean (trainB$zi)


###6###

#Priors. Obtengo la proporcion de cada grupo sobre el total 
Priors <- matrix(c(nA/(nA+nB),nB/(nA+nB)),nrow = 2, ncol = 1,byrow = TRUE)
Priors

#Sum of group means 
Summean = matrix(c(meanX1A+meanX1B,meanX2A+meanX2B),nrow = 2, ncol = 1,byrow = TRUE)
Summean

#alpha no priors. Primer umbral.
tGamma <- t(Gamma)
alphao <- (tGamma%*%Summean)/2
alphao
#el cut off point alpha no prior es -0.9988814. Lo que significa que por debajo de este umbral, las companias son rechazadas por ser consideradas riesgosas.  

#Saco el score y la probabilidad de test. 
testB = data.frame(test$DURATION, test$AMOUNT, test$RESPONSE)
colnames(testB) = c("DURATION", "AMOUNT", "RESPONSE")

testB$zi <- testB$DURATION*Gamma[1, 1]+testB$AMOUNT*Gamma[2, 1]

testB$pi <- as.vector(1/(1+(Priors[1, 1]/Priors[2, 1])*exp(testB$zi-alphaotest)))
meanpitest = mean(testB$pi)
1-0.3452573
testB$grant1 <- as.numeric(testB$pi<1-0.3452573)
sum(testB$grant1 ==1) #373 personas consiguieron el credito
sum(testB$grant1 ==0)


###7###

testB$grant2 <- as.numeric(testB$pi<0.5)
sum(testB$grant2 ==1) #373 personas consiguieron el credito
sum(testB$grant2 ==0) #27 personas no lo consiguieron
length(testB$pi[testB$pi<0.5])/length(testB$pi)
##6,75% de los aplicantes se le hubiese negado el credito, 93,25% se le hubiese otorgado
plot(testB$pi)
abline(h = 0.5, col = "red")


#EJERCICIO C#

###8###

logtrainB = data.frame(train$DURATION, train$AMOUNT, train$RESPONSE)
colnames(logtrainB) = c("DURATION", "AMOUNT", "RESPONSE")

logtrain <- glm(RESPONSE~DURATION+AMOUNT, data=logtrainB, family="binomial")
summary(logtrain)
##Amount no es significativa pero la dejamos porque el problema lo pide. 
## En cuanto a la duracion, podemos determinar que ante un aumento de ella, la probabilidad de que el credito otorgado sea malo aumenta. Esto nos lo indica el signo negativo del estimador. 


###9###

logtestB = data.frame(test$DURATION, test$AMOUNT, test$RESPONSE)
colnames(logtestB) = c("DURATION", "AMOUNT", "RESPONSE")
logtestB$predicted.risk <- predict(logtrain, newdata=logtestB, type="response") #Probabiidades de no ser un high risk applicant. 
mean(logtestB$predicted.risk) #Probabilidad media de que una compania no sea riesgosa es 0,7001426. 
#Debemos sacar el complemento de ambos para predecir la probabilidad de que la compania sea de high risk
risky = 1- logtestB$predicted.risk
mean (risky) #0,2998574 es la probabilidad de que una compania sea considerada de high risk.
plot(risky)

###10###

#Mirando las tablas de contingencia.
logtestB$grant = as.numeric(risky < 0.5)
table (logtestB$grant) #A 35 personas no se les otorga el credito por ser consideradas de alto riesgo (probabilidad por encima de 50.) A 365 personas se les otorga el credito por ser consideradas de bajo riesgo (probabilidad por debajo de 50)
365/(365+35) 
#A un 8,75% de los aplicantes se le hubiese negado el credito, mientras que a un 91,25% se les hubiese otorgado. 
table(logtestB$grant, logtestB$RESPONSE) 
#Podemos ver lo siguiente representado en un grafico. 
plot(risky)
abline(h = 0.5, col = "red")
#Todos los de arriba a 0,5 son considerados de high risk. 


###11###

#LDA#
# 373 aplicantes aceptados
# 27 aplicantes rechazados
#6,75% de los aplicantes se le hubiese negado el credito, 93,25% se le hubiese otorgado

#Logistic#
# 365 aplicantes aceptados
# 35 aplicantes rechazados
#A un 8,75% de los aplicantes se le hubiese negado el credito, mientras que a un 91,25% se les hubiese otorgado. 


#Ejercicio D#

###12###

#Confussion Matrix for model LDA
confusionMatrix(as.factor(testB$grant2), as.factor(testB$RESPONSE))

#Confussion Matrix for model Logit
confusionMatrix(as.factor(logtestB$grant), as.factor(logtestB$RESPONSE))


###13###

##Accuracy:
#LDA: 0,7075
#Logit: 0,7175

##Sensitivity:
#LDA: 0,1250
#Logit: 0,1750

##Specificity:
#LDA: 0,9571
#Logit: 0,95


###14###

#ROC curve for LDA model
pred_lda<- prediction(testB$pi, testB$RESPONSE)
# Creamos objetos de rendimiento
str(performance(pred_lda, "auc"))
as.numeric(performance(pred_lda, "auc")@y.values)
# Added
ROCRperf.test <- performance(pred_lda, "tpr", "fpr")
plot(ROCRperf.test, main="ROC Curve LDA Model", colorize=TRUE, print.cutoffs.at=seq(0,1,by=0.1), text.adj=c(-0.2,1.7))

#ROC curve for Logti model
pred_logit<- prediction(logtestB$predicted.risk, logtestB$RESPONSE)
# Creamos objetos de rendimiento
str(performance(pred_logit, "auc"))
as.numeric(performance(pred_logit, "auc")@y.values)
# Added
ROCRperf.test <- performance(pred_logit, "tpr", "fpr")
plot(ROCRperf.test, main="ROC Curve Logit Model", colorize=TRUE, print.cutoffs.at=seq(0,1,by=0.1), text.adj=c(-0.2,1.7))

#Ejercicio E#

###15###

#A continuacion vamos a crear otro modelo con el metodo directo, es decir, con una intuicion economica propia para determinar que variables de las observadas considerar dentro del modelo. 
glmtrainE = data.frame(train$CHK_ACCT,train$DURATION,train$HISTORY,train$AMOUNT,train$SAV_ACCT,train$EMPLOYMENT,train$GUARANTOR,train$RESPONSE)
colnames(glmtrainE) = c("CHK_ACCT","DURATION","HISTORY","AMOUNT","SAV_ACCT","EMPLOYMENT","GUARANTOR", "RESPONSE")

glmtrain = glm(RESPONSE~ CHK_ACCT+DURATION+HISTORY+AMOUNT+SAV_ACCT+EMPLOYMENT+GUARANTOR, data = glmtrainE, family = "binomial")
summary(glmtrain)

glmtrain$predicted.risk <- predict(glmtrain, newdata=test, type="response") #Probabiidades de no ser un high risk applicant. 
risky = 1- mean(glmtrain$predicted.risk)

glmtrain$grant = as.numeric(risky < 0.5)
table (logtestB$grant)


###16###
#Poniendo todas las variables del modelo como variables independientes, tenemos que las unicas significativas, es decir, las mayores predictoras, son las variables CHK_ACCT, DURATION, HISTORY, SAV_ACCT, GUARANTOR. Pero las que mas influyen de ellas son el estado de su cuenta bancaria, la duracion de los creditos en meses, el hisotrial crediticio del aplicante. 
#la manera de identificarlo fue a traves de la significatividad de las variables en el modelo logistico creado, que contiene todas las cariables obvservadas. 


###17###


###############################################EJERCICIO 2################################################


#Importamos Librerias.
install.packages('readr')
install.packages('psych')
library(tidyverse)
library(dplyr)
library(caret)
library(ROCR)
library(caTools)
library(psych)
library(e1071)


#Importamos dataset.
data <- read_csv("credit_data_ex2.csv")
colnames(data) <- c("GENDER", "AGE", "DEBT", "MARITALSTATUS", "BANKCOSTUMER", "EDUCATIONLEVEL", "ETHNICITY", "YEARSEMPLOYED", "PRIORDEFAULT", "EMPLOYED", "CREDITSCORE", "DRIVERSLICENSE", "CITIZEN", "ZIPCODE", "INCOME", "APPROVAL")

###1###

#Missing Values
sum(is.na(data))

##Podemos omitir todas las lineas en donde se encuentren missing values o podemos cambiarla por el valor medio de la columna asi no eliminamos 67 lineas.
#Decidimos eliminar todas las observaciones ya que hay muchas categorias dummies por lo que un promedio no se puede llevar a cabo.
data = na.omit(data)
# Lineas con na son eliminadas.

#Decidimos transformar todas las variables numericas para que se haga masa facil interpretar resultados y lograr una mejor comprension del codigo
##Transformo todas las variables en numericas.

data$PRIORDEFAULT = as.numeric(data$PRIORDEFAULT == "TRUE")
data$EMPLOYED = as.numeric(data$EMPLOYED == "TRUE")
data$DRIVERSLICENSE = as.numeric(data$DRIVERSLICENSE == "TRUE")
data$GENDER = as.numeric((data$GENDER == "b"))

{
  data$MARITALSTATUS = as.character(data$MARITALSTATUS)
  for (i in 1:length(data$MARITALSTATUS))
    if(data$MARITALSTATUS[i] == 'l'){
      data$MARITALSTATUS[i] = 0
    } else if (data$MARITALSTATUS[i] == 'u'){
      data$MARITALSTATUS[i] = 1
    } else if(data$MARITALSTATUS[i] == 'y'){
      data$MARITALSTATUS[i] = 2
    }
}

{
  data$BANKCOSTUMER = as.character(data$BANKCOSTUMER)
  for (i in 1:length(data$BANKCOSTUMER))
    if(data$BANKCOSTUMER[i] == 'g'){
      data$BANKCOSTUMER[i] = 0
    } else if (data$BANKCOSTUMER[i] == 'gg'){
      data$BANKCOSTUMER[i] = 1
    } else if(data$BANKCOSTUMER[i] == 'p'){
      data$BANKCOSTUMER[i] = 2
    }
}

{
  data$CITIZEN = as.character(data$CITIZEN)
  for (i in 1:length(data$CITIZEN))
    if(data$CITIZEN[i] == 'g'){
      data$CITIZEN[i] = 0
    } else if (data$CITIZEN[i] == 'p'){
      data$CITIZEN[i] = 1
    } else if(data$CITIZEN[i] == 's'){
      data$CITIZEN[i] = 2
    }
}

{
  data$EDUCATIONLEVEL <-as.character(data$EDUCATIONLEVEL)
  for (i in 1:length(data$EDUCATIONLEVEL))
    if(data$EDUCATIONLEVEL[i] == 'q'){
      data$EDUCATIONLEVEL[i] = 0
    }else if(data$EDUCATIONLEVEL[i] =='w'){
      data$EDUCATIONLEVEL[i] = 1
    }else if(data$EDUCATIONLEVEL[i] =='m'){
      data$EDUCATIONLEVEL[i]=2
    }else if(data$EDUCATIONLEVEL[i]=='r'){
      data$EDUCATIONLEVEL[i]=3
    }else if(data$EDUCATIONLEVEL[i]=='cc'){
      data$EDUCATIONLEVEL[i]=4
    }else if(data$EDUCATIONLEVEL[i]=='k'){
      data$EDUCATIONLEVEL[i]=5
    }else if(data$EDUCATIONLEVEL[i]=='q'){
      data$EDUCATIONLEVEL[i]=6
    }else if(data$EDUCATIONLEVEL[i]=='c'){
      data$EDUCATIONLEVEL[i]=7
    }else if(data$EDUCATIONLEVEL[i]=='d'){
      data$EDUCATIONLEVEL[i]=8
    }else if(data$EDUCATIONLEVEL[i]=='x'){
      data$EDUCATIONLEVEL[i]=9
    }else if(data$EDUCATIONLEVEL[i]=='i'){
      data$EDUCATIONLEVEL[i]=10
    }else if(data$EDUCATIONLEVEL[i]=='aa'){
      data$EDUCATIONLEVEL[i]=11
    }else if(data$EDUCATIONLEVEL[i]=='e'){
      data$EDUCATIONLEVEL[i]=12
    }else if(data$EDUCATIONLEVEL[i]=='ff'){
      data$EDUCATIONLEVEL[i]=13
    }else if(data$EDUCATIONLEVEL[i]=='j'){
      data$EDUCATIONLEVEL[i]=14
    }
}  
list(data$EDUCATIONLEVEL)
{ 
  data$ETHNICITY <-as.character(data$ETHNICITY)
  for (i in 1:length(data$ETHNICITY)) 
    if (data$ETHNICITY[i]=='h'){
      data$ETHNICITY[i]=0
    }else if(data$ETHNICITY[i]=='v'){
      data$ETHNICITY[i] = 1
    }else if(data$ETHNICITY[i]=='bb'){
      data$ETHNICITY[i] = 2
    }else if(data$ETHNICITY[i]=='ff'){
      data$ETHNICITY[i]=3
    }else if(data$ETHNICITY[i]=='j'){
      data$ETHNICITY[i]=4
    }else if(data$ETHNICITY[i]=='z'){
      data$ETHNICITY[i]=5
    }else if(data$ETHNICITY[i]=='o'){
      data$ETHNICITY[i]=6
    }else if(data$ETHNICITY[i]=='dd'){
      data$ETHNICITY[i]=7
    }else if(data$ETHNICITY[i]=='n'){
      data$ETHNICITY[i]=8
    }
}


###2###

#Graficos de las distribuciones de las siguientes variables. Podemos Agregar Histograma

plot(data$AGE)
hist(data$AGE, breaks = 'FD')
skewness(data$AGE)
kurtosi(data$AGE)
#Skew positiva indica que la distribucion esta desviada hacia la derecha

plot(data$DEBT)
hist(data$DEBT, breaks = 'FD')
skewness(data$DEBT)
kurtosi(data$DEBT)

plot(data$CREDITSCORE)
hist(data$CREDITSCORE)
skewness(data$CREDITSCORE)
kurtosi(data$CREDITSCORE)

plot(data$INCOME)
hist(data$INCOME, breaks = 15)
skewness(data$INCOME)
kurtosi(data$INCOME)

plot(data$YEARSEMPLOYED)
hist(data$YEARSEMPLOYED)
skewness(data$YEARSEMPLOYED)
kurtosi(data$YEARSEMPLOYED)
#Observando los graficos y teniendo en cuenta que son variable de 0 a +inf podemos ver que los outliers son desviadas a la derecha. Lo corroboramos con la Skewness y la Kurtosis.
#Por eso podemos proponer una transformacion logaritmica o aplicacion de la raiz a las variables para suavizarla y generar una funcion mas normal y con menos Skewness y Kurtosis.
#Dado que esto modificaria nuestro modelo de credit scoring no creemos necesario modificar las variables para el modelo.


###3###

plot(data$PRIORDEFAULT,data$APPROVAL)
table(data$PRIORDEFAULT, data$APPROVAL,dnn=c("Prior Default","Approval") )
#Prior Default es una  dummy por eso realizamos una tabla para ver la relacion entre las variables. Discutir.
plot(data$EMPLOYED, data$APPROVAL)
table(data$EMPLOYED, data$APPROVAL,dnn=c("Employed","Approval"))
#Tambien dummy
table(data$EDUCATIONLEVEL, data$APPROVAL,dnn=c("Education Level","Approval"))
#Creamos una tabla con las 15 letras posibles
plot(data$ETHNICITY, data$APPROVAL)
table(data$ETHNICITY, data$APPROVAL,dnn=c("Ethinicty","Approval"))
#dummy...

plot(data$CREDITSCORE, data$APPROVAL)
table(data$CREDITSCORE,data$APPROVAL, dnn=c("Credit Score","Approval"))

plot(data$INCOME, data$APPROVAL)
table(data$INCOME, data$APPROVAL,dnn=c("Income","Approval"))
#Comentar cada una de estas


###4###

#Dividimos el data set en train y test.
set.seed(123)
index = sample(1:652, size = trunc(.65*652))
train = data[index,]
test = data[-index, ]


###5###

#Estimamos un modelo de regresion logistica para la probabilidad de obtener una aplicacion aprobada (1)
logtrain = glm(APPROVAL~., data = train, family = "binomial")
summary(logtrain)
#dan significativas algunas si otras no.
#pero dan algunos numeros de las variables si asi que haremos lo siguiente: Creamos variables nuevas que sean unicamente la signiicancia de las variables.
#Nos vimos forzados a hacer esto debido a que consideramos que el modelo (logicamente) no toma de la  misma manera cada nivel, es decir,
#No es lo mismo si el nivel de educacion de un aplicante es universitario o secundario o de posgrado.

#Convierto a las letras de las variables significativas en binarias
data$EDUCATIONLEVEL12 =ifelse(data$EDUCATIONLEVEL=="12",1,0)
data$EDUCATIONLEVEL13=ifelse(data$EDUCATIONLEVEL=="13",1,0)
data$EDUCATIONLEVEL14=ifelse(data$EDUCATIONLEVEL=="14",1,0)
data$EDUCATIONLEVEL9=ifelse(data$EDUCATIONLEVEL=="9",1,0)
data$ETHNICITY5=ifelse(data$ETHNICITY=="5",1,0)
data$ETHNICITY4=ifelse(data$ETHNICITY=="4",1,0)
data$ETHNICITY8=ifelse(data$ETHNICITY=="8",1,0)

#Agrego las dummies al train y al set de nuevo.
set.seed(123)
index = sample(1:652, size = trunc(.65*652))
train = data[index,]
test = data[-index, ]

#nuevo modelo logistic solo con las significativas
logtrain <- glm(APPROVAL ~EDUCATIONLEVEL9+AGE+ETHNICITY8+ETHNICITY5+YEARSEMPLOYED+PRIORDEFAULT+DRIVERSLICENSE+INCOME, data=train, family="binomial") 
summary(logtrain) #dan todas significativas A 10%, GENIAL.
train$predicted.risk = predict(logtrain,newdata = train, type = 'response')


###6###

#Aplicamos un treshold de 0.5 tanto en el train y en el el test y computamos la matriz de confusion en el test. 

#Train.
table(as.numeric(train$predicted.risk>=0.5),train$APPROVAL)
#esta es la tabla para el train
sum(train$APPROVAL == 0)
sum(train$APPROVAL ==1)

#Test
test$predicted.risk = predict(logtrain, newdata = test, type = "response")
table(as.numeric(test$predicted.risk>=0.5),test$APPROVAL)
#Armo una nueva variable de 1 y 0 con los valores predichos
test$APPROVALPRED = as.numeric(test$predicted.risk>= 0.5)

#Creamos matriz de confusion entre los aprovados del teset y los predichos aprovados
confusionMatrix(as.factor(test$APPROVAL), as.factor(test$APPROVALPRED))


###7### esta en esa confussionmatrix, interpretar...


###8###

pred = prediction(test$predicted.risk, test$APPROVAL)
#con el approval o con el approvalpred?
# Function to create performance objects
str(performance(pred, "auc"))
as.numeric(performance(pred, "auc")@y.values)
# Added
ROCRperf.test <- performance(pred, "tpr", "fpr")
plot(ROCRperf.test)


# Add colors
plot(ROCRperf.test, colorize=TRUE)
# Add threshold labels 
plot(ROCRperf.test, colorize=TRUE, print.cutoffs.at=seq(0,1,by=0.1), text.adj=c(-0.2,1.7))
# text.adj= allows adjustment of the text position with respect to (x, y). 


##Nueral Network (NN)

#Importamos Librerias que utilizaremos.
library(caret)
install.packages("verification")
library(verification)
library(nnet)
library(NeuralNetTools)
library(ROCR)

##Importo base de datos de nuevo pero con distinto formato.
data <- read.csv("credit_data_ex2.csv")
colnames(data) <- c("GENDER", "AGE", "DEBT", "MARITALSTATUS", "BANKCOSTUMER", "EDUCATIONLEVEL", "ETHNICITY", "YEARSEMPLOYED", "PRIORDEFAULT", "EMPLOYED", "CREDITSCORE", "DRIVERSLICENSE", "CITIZEN", "ZIPCODE", "INCOME", "APPROVAL")

#Missing Values
sum(is.na(data))
##Podemos omitir todas las lineas en donde se encuentren missing values o podemos cambiarla por el valor medio de la columna asi no eliminamos 67 lineas.
data = na.omit(data)


###9###

#Para estimar el modelo de redes neuronales decidimos converitr todas las variables en numericas, por la misma razon que en la logistica: mejor compresion del lenguaje y el codigo

data$GENDER=as.numeric((data$GENDER=="b"))
data$PRIORDEFAULT<-as.numeric((data$PRIORDEFAULT=="t"))
data$EMPLOYED=as.numeric((data$EMPLOYED=="t"))
data$DRIVERSLICENSE=as.numeric((data$DRIVERSLICENSE=="f"))

{
  data$MARITALSTATUS = as.character(data$MARITALSTATUS)
  for (i in 1:length(data$MARITALSTATUS))
    if(data$MARITALSTATUS[i] == 'l'){
      data$MARITALSTATUS[i] = 0
    } else if (data$MARITALSTATUS[i] == 'u'){
      data$MARITALSTATUS[i] = 1
    } else if(data$MARITALSTATUS[i] == 'y'){
      data$MARITALSTATUS[i] = 2
    }
}

{
  data$BANKCOSTUMER = as.character(data$BANKCOSTUMER)
  for (i in 1:length(data$BANKCOSTUMER))
    if(data$BANKCOSTUMER[i] == 'g'){
      data$BANKCOSTUMER[i] = 0
    } else if (data$BANKCOSTUMER[i] == 'gg'){
      data$BANKCOSTUMER[i] = 1
    } else if(data$BANKCOSTUMER[i] == 'p'){
      data$BANKCOSTUMER[i] = 2
    }
}

{
  data$CITIZEN = as.character(data$CITIZEN)
  for (i in 1:length(data$CITIZEN))
    if(data$CITIZEN[i] == 'g'){
      data$CITIZEN[i] = 0
    } else if (data$CITIZEN[i] == 'p'){
      data$CITIZEN[i] = 1
    } else if(data$CITIZEN[i] == 's'){
      data$CITIZEN[i] = 2
    }
}


{
  data$EDUCATIONLEVEL <-as.character(data$EDUCATIONLEVEL)
  for (i in 1:length(data$EDUCATIONLEVEL))
    if(data$EDUCATIONLEVEL[i] == 'q'){
      data$EDUCATIONLEVEL[i] = 0
    }else if(data$EDUCATIONLEVEL[i] =='w'){
      data$EDUCATIONLEVEL[i] = 1
    }else if(data$EDUCATIONLEVEL[i] =='m'){
      data$EDUCATIONLEVEL[i]=2
    }else if(data$EDUCATIONLEVEL[i]=='r'){
      data$EDUCATIONLEVEL[i]=3
    }else if(data$EDUCATIONLEVEL[i]=='cc'){
      data$EDUCATIONLEVEL[i]=4
    }else if(data$EDUCATIONLEVEL[i]=='k'){
      data$EDUCATIONLEVEL[i]=5
    }else if(data$EDUCATIONLEVEL[i]=='q'){
      data$EDUCATIONLEVEL[i]=6
    }else if(data$EDUCATIONLEVEL[i]=='c'){
      data$EDUCATIONLEVEL[i]=7
    }else if(data$EDUCATIONLEVEL[i]=='d'){
      data$EDUCATIONLEVEL[i]=8
    }else if(data$EDUCATIONLEVEL[i]=='x'){
      data$EDUCATIONLEVEL[i]=9
    }else if(data$EDUCATIONLEVEL[i]=='i'){
      data$EDUCATIONLEVEL[i]=10
    }else if(data$EDUCATIONLEVEL[i]=='aa'){
      data$EDUCATIONLEVEL[i]=11
    }else if(data$EDUCATIONLEVEL[i]=='e'){
      data$EDUCATIONLEVEL[i]=12
    }else if(data$EDUCATIONLEVEL[i]=='ff'){
      data$EDUCATIONLEVEL[i]=13
    }else if(data$EDUCATIONLEVEL[i]=='j'){
      data$EDUCATIONLEVEL[i]=14
    }
}  
list(data$EDUCATIONLEVEL)
{ 
  data$ETHNICITY <-as.character(data$ETHNICITY)
  for (i in 1:length(data$ETHNICITY)) 
    if (data$ETHNICITY[i]=='h'){
      data$ETHNICITY[i]=0
    }else if(data$ETHNICITY[i]=='v'){
      data$ETHNICITY[i] = 1
    }else if(data$ETHNICITY[i]=='bb'){
      data$ETHNICITY[i] = 2
    }else if(data$ETHNICITY[i]=='ff'){
      data$ETHNICITY[i]=3
    }else if(data$ETHNICITY[i]=='j'){
      data$ETHNICITY[i]=4
    }else if(data$ETHNICITY[i]=='z'){
      data$ETHNICITY[i]=5
    }else if(data$ETHNICITY[i]=='o'){
      data$ETHNICITY[i]=6
    }else if(data$ETHNICITY[i]=='dd'){
      data$ETHNICITY[i]=7
    }else if(data$ETHNICITY[i]=='n'){
      data$ETHNICITY[i]=8
    }
}

data$EDUCATIONLEVEL<-as.numeric((data$EDUCATIONLEVEL))
data$ETHNICITY<-as.numeric((data$ETHNICITY))
data$MARITALSTATUS = as.numeric(data$MARITALSTATUS)
data$BANKCOSTUMER = as.numeric(data$BANKCOSTUMER)
data$CITIZEN = as.numeric(data$CITIZEN)
str(data) #son todos num

#dividimos en train y test 
set.seed(12)

index = sample(1:652, size = trunc(.65*652))
train = data[index,]
test = data[-index, ]
train$APPROVAL <- as.factor(train$APPROVAL)
test$APPROVAL <- as.factor(test$APPROVAL)

#Estimamos redes neuronales.
#Entrenamos modelo en la base train primero.
par(mfrow=c(1,1))
nnet_train <- train(APPROVAL~., data=train,method="nnet")
str(nnet_train)
print(nnet_train)
##podemos ver que con hidden layers = 5 es el que tiene mejor accuracy seguido por hidden layers = 3.
plot(nnet_train)
#Aca se puede ver perfectamente en un grafico lo que dice en print(nnet_train).
plotnet(nnet_train$finalModel, y_names = "response")
title("Graphical Representation of our Neural Network")
#Representacion grafica del modelo de redes neuronales.

#Con estas dos funciones observamos la importancia relativa de cada variable. La funcion identifica la importancia deconstruyendo los weights de cada una del modelo.
garson(nnet_train)
olden(nnet_train)
#Finalmente decidimos que para el modelo de redes neuronales lo conveniente es dejar todas las variables dado el modelo de redes neuronales.


###10###

##ADD PCUT 
pcut<- 0.5

#In sample (Train)
prob.nnet= predict(nnet_train,type='prob')
pred.nnet = (prob.nnet[,2] >=pcut)*1
table(train$APPROVAL,pred.nnet, dnn=c("Observed","Predicted"))
mean(ifelse(train$APPROVAL != pred.nnet, 1, 0))

#Out of sample (Test)
prob.nnet.test= predict(nnet_train,test,type='prob')
test$pred.nnet.test = as.numeric(prob.nnet.test[,2] > pcut)
table(test$APPROVAL,test$pred.nnet.test, dnn=c("Observed","Predicted")) 
mean(ifelse(test$APPROVAL != test$pred.nnet.test, 1, 0))


###11###

#Confussion Matrix
confusionMatrix(table(test$APPROVAL,test$pred.nnet.test, dnn=c("Observed","Predicted")))
#Specifity, sensivity, accuracy todo eso aca.


###12###

#utilizamos los  graficos ROC de cada modelo para comparar. Tambien utilizaremos la matriz de confusion de cada uno para comparar y predecir cual funciona mejor.
##Roc curve for Neural Network Model.
par(mfrow=c(1,2))
#Train.
roc.nnet <- roc.plot(x=(train$APPROVAL == "1"), pred =prob.nnet[,2], main = "Train Model ROC Curve")
roc.nnet$roc.vol
#Test.
roc.nnet.test <- roc.plot(x=(test$APPROVAL == "1"), pred =prob.nnet.test[,2], main = "Test Model ROC Curve")
roc.nnet.test$roc.vol



#KNN
#Importamos paquetes que necesitaremos.
install.packages("class")
library(class)
library(ISLR)


###13###

setwd("C:\\Users\\Nicolas\\Desktop\\UDESA\\Admin de Riesgos Financieros\\Examen Parcial")
data <- read_csv("credit_data_ex2.csv")
colnames(data) <- c("GENDER", "AGE", "DEBT", "MARITALSTATUS", "BANKCOSTUMER", "EDUCATIONLEVEL", "ETHNICITY", "YEARSEMPLOYED", "PRIORDEFAULT", "EMPLOYED", "CREDITSCORE", "DRIVERSLICENSE", "CITIZEN", "ZIPCODE", "INCOME", "APPROVAL")
data = data[,c("AGE","DEBT","YEARSEMPLOYED","CREDITSCORE","INCOME","APPROVAL")]
summary(data)
data = na.omit(data)
#Normalizamos la funcion a atributos normales
normalize <- function(x) {
  norm <- ((x - min(x))/(max(x) - min(x)))
  return (norm)
}
#Aplicamos esta normalizacion a data, le sacamos la variable a predecir.
norm_data = apply(data[,-6],2, normalize)
head(norm_data)
summary(norm_data)

#dividimos la data en test y set
set.seed(123)
index = sample(1:652, size = trunc(.8*652))
train = data[index,]
test = data[-index,]

###13###

##otra forma de hacer KNN con CV

set.seed(400)
ctrl <- trainControl(method="repeatedcv",repeats = 3) #,classProbs=TRUE,summaryFunction = twoClassSummary)
knnFit <- train(APPROVAL ~ ., data = train, method = "knn", trControl = ctrl, preProcess = c("center","scale"), tuneLength = 20)

#Output of kNN fit
knnFit
#por CV se definio que el K debe ser 23,  dado el menor RMSE.
plot(knnFit)
#Grafico que muestra como se minimiza el error en k = 23.
test$knnpred = predict(knnFit, newdata = test)


###14###

#TreshHold mayor a 0,5
test$APPROVAL_PRED = as.numeric(test$knnpred>=0.5)
confusionMatrix(table(test$APPROVAL_PRED, test$APPROVAL))
#toda la info en este confussion matrix