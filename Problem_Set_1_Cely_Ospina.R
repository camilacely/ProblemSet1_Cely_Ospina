###################################
## Big Data - Problem Set 1 #######
# Maria Camila Cely , Sara Ospina #
###### Junio 2022 #################
###################################

#####################
# 1. Data Acquisition
#####################

#Prep

rm(list=ls())

library(tidyverse)
library(rvest)
library(dplyr)
library(skimr)
library(ggplot2)
library(caret)

#Set

GEIH1 <- data.frame(stringsAsFactors = FALSE) 
url1 <- paste0("https://ignaciomsarmiento.github.io/GEIH2018_sample/pages/geih_page_1.html")
temp<-read_html(url1)%>%html_table()
GEIH1 <- rbind (GEIH1, temp)
geih1 <- rename(GEIH1, X1.3218="X1.3217")

GEIH2 <- data.frame(stringsAsFactors = FALSE) 
url2 <- paste0("https://ignaciomsarmiento.github.io/GEIH2018_sample/pages/geih_page_2.html")
temp<-read_html(url2)%>%html_table()
GEIH2 <- rbind (GEIH2, temp)
geih2<-GEIH2

GEIH3 <- data.frame(stringsAsFactors = FALSE) 
url3 <- paste0("https://ignaciomsarmiento.github.io/GEIH2018_sample/pages/geih_page_3.html")
temp<-read_html(url3)%>%html_table()
GEIH3 <- rbind (GEIH3, temp)
geih3<-GEIH3

GEIH4 <- data.frame(stringsAsFactors = FALSE) 
url4 <- paste0("https://ignaciomsarmiento.github.io/GEIH2018_sample/pages/geih_page_4.html")
temp<-read_html(url4)%>%html_table()
GEIH4 <- rbind (GEIH4, temp)
geih4<-GEIH4

GEIH5 <- data.frame(stringsAsFactors = FALSE) 
url5 <- paste0("https://ignaciomsarmiento.github.io/GEIH2018_sample/pages/geih_page_5.html")
temp<-read_html(url5)%>%html_table()
GEIH5 <- rbind (GEIH5, temp)
geih5<-GEIH5

GEIH6 <- data.frame(stringsAsFactors = FALSE) 
url6 <- paste0("https://ignaciomsarmiento.github.io/GEIH2018_sample/pages/geih_page_6.html")
temp<-read_html(url6)%>%html_table()
GEIH6 <- rbind (GEIH6, temp)
geih6<-GEIH6

GEIH7 <- data.frame(stringsAsFactors = FALSE) 
url7 <- paste0("https://ignaciomsarmiento.github.io/GEIH2018_sample/pages/geih_page_7.html")
temp<-read_html(url7)%>%html_table()
GEIH7 <- rbind (GEIH7, temp)
geih7<-GEIH7

GEIH8 <- data.frame(stringsAsFactors = FALSE) 
url8 <- paste0("https://ignaciomsarmiento.github.io/GEIH2018_sample/pages/geih_page_8.html")
temp<-read_html(url8)%>%html_table()
GEIH8 <- rbind (GEIH8, temp)
geih8<-GEIH8

GEIH9 <- data.frame(stringsAsFactors = FALSE) 
url9 <- paste0("https://ignaciomsarmiento.github.io/GEIH2018_sample/pages/geih_page_9.html")
temp<-read_html(url9)%>%html_table()
GEIH9 <- rbind (GEIH9, temp)
geih9 <- rename(GEIH9, X1.3218="X1.3217")

GEIH10 <- data.frame(stringsAsFactors = FALSE) 
url10 <- paste0("https://ignaciomsarmiento.github.io/GEIH2018_sample/pages/geih_page_10.html")
temp<-read_html(url10)%>%html_table()
GEIH10 <- rbind (GEIH10, temp)
geih10 <- rename(GEIH10, X1.3218="X1.3217")

geih<-rbind(geih1, geih2, geih3, geih4, geih5, geih6, geih7, geih8, geih9, geih10) #32.177 observaciones

#save(geih, file = "C:/Users/Camila Cely/Documents/GitHub/ProblemSet1_Cely_Ospina/geih.Rdata")
#load("C:/Users/Camila Cely/Documents/GitHub/ProblemSet1_Cely_Ospina/geih.Rdata")
#load("C:/Users/SARA/Documents/ESPECIALIZACIÓN/BIG DATA/GITHUB/ProblemSet1_Cely_Ospina/geih.Rdata")

#^Shortcut para cargar la base sin hacer scraping


#####################
# 2. Data Cleaning
#####################


#Focus on individuals older than eighteen
geih18<-geih[!(geih$age<18),] #24.568 observaciones

#Focus on employed individuals

geih18e<-geih18[!(geih18$ocu<1),] #16.542 observaciones #ocu = 1 if occupied, 0 otherwise



#ELECCION DE VARIABLE Y
#justificación: Se escoge la variable ingtot pues está teniendo en cuenta tanto valores observados para el ingreso como valores imputados. Se tiene en cuenta el ingreso laboral, ingresos de otras fuentes (como arriendos) e ingresos que debería tener de acuerdo con las características observadas.  
#Se asume que el DANE hace un ejercicio confiable en la imputación al ser una fuente confiable. 
ing<-geih18e$ingtot 
geih_Y<-geih18e #se le saca copia a la base para modificarle lo pertinente en variable Y


#Se evalúa la simetría y distribución de la variable
geih_Y$ingtot[geih_Y$ingtot == 0] <- 1 #se cambian los valores de ingreso 0 a ingreso 1 para que el ln no salga como menos infinito
BoxCoxTrans(geih_Y$ingtot)

ggplot () + geom_boxplot(data=geih_Y, aes(x=ingtot), fill ="tomato", alpha=0.5)

geih_Y <- geih_Y %>% mutate(logingtot = log(ingtot))
summary(geih_Y$logingtot)

#Se toma el logaritmo del ingreso con el fin de normalizar la distribución de este y mejorar su interpretacion.

ggplot () + geom_boxplot(data=geih_Y, aes(x=logingtot), fill ="tomato", alpha=0.5)
#Se puede observar que la distribución mejora considerablemente, pero continúan presentandose outliers que modifican el resultado. 

#Resolver outliers: 
quantile(x=geih_Y$logingtot , na.rm=T)
#Se puede observar que hay un salto mas grande entre el primer y segundo cuartil 

IQR(x=geih_Y$logingtot , na.rm=T)

#Se utiliza el rango intercuartilico (entre el 25% y el 75% )
iqr <- IQR(x=geih_Y$logingtot , na.rm=T)

#Modificamos la base para tomar unicamente las observaciones que son iguales o mayores al primer cuartil, de esta forma se descartan 265 observaciones. 
#se decide descartar estas observaciones pues no tiene sentido que personas que se declararon ocupadas y que reportaron horas trabajadas hayan reportado que su ingreso es = a 0
geih_e<- geih_Y %>% subset(logingtot >= 1*iqr) #corre bien

#como se puede observar la variable tiene una mejor distribución
quantile(x=geih_e$logingtot , na.rm=T)
ggplot () + geom_boxplot(data=geih_e, aes(x=logingtot), fill ="tomato", alpha=0.5)

##A PARTIR DE AQUI geih_e
####Nota: geih_e es una copia de geih18e pero en la que modificamos los valores de ingreso, de ahora en adelante se usara esta


#ELECCION DE VARIABLES EXPLICATIVAS RELEVANTES (X)

edad<-geih_e$age
educ<-geih_e$maxEducLevel
gen<-geih_e$sex #gen = sex, o sea que valor de 1 corresponde a hombre

head(geih_e$sex)

#se recodifica la variable genero con el fin de encontrar cual es el efecto de ser mujer, al especificar este como 1 y hombre como 0
geih_e   <- geih_e %>% 
  mutate(fem = ifelse(test = sex > 0 , #notar que sex tomaba valor de 1 para hombre
                      yes = 0, 
                      no = 1))

head(geih_e$fem)  #fem toma valor de 1 para individuo mujer #si comparamos este head con el de sex vemos que se invierte

fem<-geih_e$fem 

#se incluye la variable de Experiencia potencial
#En la literatura se ha utilizado como proxy de la experiencia la experiencia potencial.
#Esta nace de restarle a la edad de la persona, los a?os que ha estudiado 
#Y ademas cinco a?os adicionales, pues en su primera infancia ni estudia ni trabaja

#maxEducLevel	1	None
#maxEducLevel	2	preschool
#maxEducLevel	3	primary incomplete (1-4)
#maxEducLevel	4	primary complete (5)
#maxEducLevel	5	secondary incomplete (6-10)
#maxEducLevel	6	secondary complete (11)
#maxEducLevel	7	terciary
#maxEducLevel	9	N/A

educ_time<-case_when(educ <= 1 ~ 0,
                     educ <= 2 ~ 1,
                     educ <= 3 ~ 2.5,
                     educ <= 4 ~ 5,
                     educ <= 5 ~ 7.5,
                     educ <= 6 ~ 12,
                     educ <= 7 ~ 17,
                     educ <= 9 ~ 0,) 

exp_potencial<-edad-educ_time-5

tipo_oficio<-geih_e$oficio
tipo_oficio<-as.factor(tipo_oficio)
formal<-geih_e$formal
estrato<-geih_e$estrato
estrato<-as.factor(estrato)


#Missing values analysis

#Para la base < 18
is.na(geih_e)
colSums(is.na(geih_e))
colSums(is.na(geih_e))>0
colnames(geih_e)[colSums(is.na(geih_e))>0] #Aqu? nos aparecen los nombres de las columnas que tienen missing values

#Ahora analizando las variables que escogimos como explicativas
#edad, educ, gen, exp_potencial

is.na(geih_e$age) 
sum(is.na(geih_e$age)) #no hay missing values

is.na(geih_e$maxEducLevel) 
sum(is.na(geih_e$maxEducLevel)) #1 missing value

is.na(geih_e$sex) 
sum(is.na(geih_e$sex)) #no hay missing values

is.na(exp_potencial) 
sum(is.na(exp_potencial)) #Como esta variable depende de edad y de maxEducLevel, tiene 1 missing value

#Este unico missing value no representa problemas en una muestra de mas de 16000 observaciones

dim(geih_e)
str(geih_e)
names(geih_e)

head(geih_e[,c("age","maxEducLevel","sex")])
tail(geih_e[,c("age","maxEducLevel","sex")])

summary(geih_e$sex)

summary(geih_e$age) #el 75% de los encuestados tiene menos de 50 a?os

as.factor(geih_e$maxEducLevel)
summary(geih_e$maxEducLevel) #Este sum no tiene mucho sentido porque es variable categ?rica -> 1 missing 
summary(educ_time) #en promedio, las personas encuestadas tienen 12,42 a?os de educaci?n (secundaria completa)
summary(geih_e$sex) #53% de los encuestados son hombres
summary(exp_potencial) #en promedio, las personas encuestadas tienen 22 a?os de experiencia laboral

as.factor(geih_e$estrato)
as.factor(geih_e$oficio)

summary(geih_e$estrato) #75% de los encuestados viven en estrato 1, 2 o 3
summary(geih_e$oficio) #categorica, no se pueden sacar conclusiones con summary
summary(geih_e$formal) #59% de los encuestados tienen trabajo formal


skim(geih_e) #Esto saca estad?sticas de todas las variables pero la base tiene muchas columnas, por lo cual crear? un subset
subset <- geih_e %>% select(age, maxEducLevel, sex)
skim(subset) #En todo caso no dice mucho porque dos de las variables son categ?ricas


#Luego de revisar la muestra se seleccionan las siguientes variables como X - variables explicativas


subset2 <- geih_e[which(geih_e$sex == 1 | geih_e$sex == 0 ),names(geih_e) %in% c("age","sex","maxEducLevel", "formal", "estrato", "oficio")]

skim(subset2)

#Adicionalmente, se creo la variable de experiencia potencial la cual se comporta de la siguiente forma: 

summary(exp_potencial) #en promedio, las personas encuestadas tienen 22 a?os de experiencia laboral


##Distribución en densidad de ingreso, variable Y
d <- ggplot(geih_e, aes(x=logingtot)) + 
  geom_density()
d+ geom_vline(aes(xintercept=mean(logingtot)),
              color="steelblue", linetype="dashed", size=1.25)

#Podemos ver como está distribuido el ingreso de acuerdo con la edad de la persona encuestada. 
#podemos observar que los salarios se vuelven mas altos a mayor edad 
ggplot(data = geih_e , mapping = aes(x = age , y = logingtot))+
  geom_point(col = "tomato" , size = 0.75)

#Distribucion del ingreso de acuerdo con el genero
p <- ggplot(data=geih_e) + 
  geom_histogram(bins=30, mapping = aes(x=logingtot , group=as.factor(fem) , fill=as.factor(fem)))
  
p + scale_fill_manual(values = c("0"="tomato" , "1"="steelblue") , label = c("0"="Hombre" , "1"="Mujer") , name = "Genero")

#Distribución del ingreso de acuerdo con el nivel de educación diferenciado por genero: 
box_plot <- ggplot(data=geih_e , mapping = aes(as.factor(educ_time) , logingtot)) + 
  geom_boxplot() + geom_point(aes(colour=as.factor(fem))) +
  scale_color_manual(values = c("0"="tomato" , "1"="steelblue") , label = c("0"="Hombre" , "1"="Mujer") , name = "Genero")
box_plot

##Creo que se puede diferenciar mejor en el boxplot y en el geom_point creo que con solo una estaría bien. 

#En este histograma podemos ver la distribucion del logaritmo del ingreso contra el nivel de educacion maximo de la muestra diferenciado por el genero de los encuestados. 
ggplot(geih_e, aes(x= educ_time)) + geom_bar(width=2, colour="steelblue", fill="steelblue1") +  
  geom_text(aes(label=..count..), stat='count',position=position_dodge(0.9), vjust=-0.5,  size=2.5)+  
  facet_wrap(~fem)  

#En este histograma podemos ver la distribucion del logaritmo del ingreso contra el estrato de las observaciones de la muestra diferenciado por el genero de los encuestados. 
ggplot(geih_e, aes(x= estrato)) + geom_bar(width=0.5, colour="tomato", fill="steelblue1") +  
  geom_text(aes(label=..count..), stat='count',position=position_dodge(0.9), vjust=-0.5,  size=2.5)+  #le cambie el tama?o de la letra para mejor comprension
  facet_wrap(~fem)

box_plot <- ggplot(data=geih_e , mapping = aes(as.factor(estrato) , logingtot)) + 
  geom_boxplot() + geom_point(aes(colour=as.factor(fem))) +
  scale_color_manual(values = c("0"="tomato" , "1"="steelblue") , label = c("0"="Hombre" , "1"="Mujer") , name = "Genero")
box_plot

#No se si se vea mejor en un boxplot


#####################
# 3. Age-earnings profile
#####################

#ELECCION DE Y (INCOME) - recordemos que esto ya lo habiamos definido bien en el punto 2 para sacar las estadisticas descriptivas

summary(geih_e$ingtot) #El 75% de los encuestados gana menos de 1'723.000, sin embargo el promedio es de 1'769.000, por lo cual
#podemos concluir que el 25% de mayores ingresos est? arrastrando ese promedio
head(geih18e$ingtot )

#Correr OLS de income y age ############ OLS1

edad2<-edad^2
geih_e<-geih_e%>%mutate(age2=age^2)

#observamos la distribucion de ingreso contra edad en la muestra: 

ggplot(data = geih_e , mapping = aes(x = age , y = ingtot))+
  geom_point(col = "red" , size = 0.5)

#ahora sacamos la regresion, incluyendo age2

ols1<-lm(geih_e$ingtot~geih_e$age+geih_e$age2)

ols1prueba<-lm(ing~edad+edad2) #Esto lo corr? para probar si daba lo mismo haci?ndolo directo en la base o extrayendo las variables

ols1
ols1prueba #s? da lo mismo

summary(ols1) #R^2 0.017 #Residual standard error: 2653000 on 16539 degrees of freedom
#age= 89516.9

#r^2 = fraction of the total variability in the response that is accounted for by the model

#Por cada a?o adicional de vida, las personas ganan en promedio 89.000 pesos adicionales
#Edad^2 tiene coeficiente negativo, por lo cual sabemos que esta funci?n no es lineal sino decreciente

require("stargazer")
stargazer(ols1) #salida ols1 en latex #######################################

#There are many statistical tools for model validation,
#but the primary tool for most process modeling applications is graphical residual analysis.
#Graphical methods have an advantage over numerical methods for model validation because they readily illustrate a broad range 
#of complex aspects of the relationship between the model and the data.

resid1<-resid(ols1)
plot(edad,resid1) #este corre pero la presentaci?n no es muy buena

ggplot(data = geih_e , mapping = aes(x = age , y = resid1))+
  geom_point(col = "red" , size = 0.5) #mejor esta salida #####################

#Interpretacion= if the residuals appear to behave randomly, it suggests that the model fits the data well. 
#On the other hand, if non-random structure is evident in the residuals, it is a clear sign that the model fits the data poorly.
#En este caso los valores no parecen comportarse de manera aleatoria pues se acumulan casi todos cerca del valor cero
#Lo anterior sumado al R^2 de la regresi?n permite concluir que este modelo no tiene muy buen ajuste con esta muestra
#Intuicion= hay otros factores que pesan mucho mas en la distribucion de ingresos que la edad, los exploraremos mas adelante

fit1<-fitted(ols1)
par(mfrow=c(2,2))
plot(ols1) #aqui encontramos otros analisis que soportan que este modelo no ajusta tan bien

#Plot predict, aqui encontramos la curvatura de age-income y vemos que aproximadamente tiene un maximo a los 40 a?os

#esta es una prediccion con una submuestra
geih_pre<-geih_e
geih_pre<-data.frame(age=runif(30,18,80))
geih_pre<- geih_pre %>% mutate(age2=age^2,
                             ingtot=rnorm(30,mean=12+0.06*age-0.001*age2)) #crear ingtot en esta submuestra 
reg_1<-lm(ingtot~age+age2,geih_pre)
ggplot(geih_pre , mapping = aes(x = age , y = predict(reg_1))) +
  geom_point(col = "red" , size = 0.5)

#esta es una prediccion con la muestra total, considero que queda mejor porque el peak_age se observa alrededor de 55 a?os
#lo cual coincide con los resultados matematicos que obtenemos mas adelante

ggplot(geih_e, aes(x=age, y=predict(ols1))) + 
  geom_point(col = "red" , size = 0.5) #esta grafica es mejor por lo tanto usaremos esta metodologia


##### Para proyectar peak-age


# Recordar: ols1<-lm(geih_e$ingtot~geih_e$age+geih_e$age2)

#derivada de ingtot con respecto a la edad
# b1 + b2*edad*2
#entonces edad***
# edad*** = -b1 / 2*b2 ######
#b1 es el de edad y b2 el de edad^2

coefs1<-ols1$coefficients #sacamos los coeficientes de la regresion que corrimos
coefs1

b0<-coefs1[1]
b1<-coefs1[2]
b2<-coefs1[3]

peak_age<--(b1/(2*b2))
# peak_age = 57.99338 

#Bootstrap= resample from the sample

#Manual (lo realizamos solo en este punto para comprobar si entendemos la metodologia)

set.seed(123)
R<-1000 #num de repeticiones

eta_mod1<-rep(0,R) #vector de ceros de numero R

for(i in 1:R){
  geih_sample<-sample_frac(geih_e,size=1,replace=TRUE) #muestra del mismo tama?o que la original - con reemplazo
  f<-lm(ingtot~age+age2,geih_sample)
  coefs<-f$coefficients #agarrame los coeficientes de f, o sea de la reg lineal
  eta_mod1[i]<-coefs[2] #irlos reemplazando en mi vector de ceros, y el 2 corresponde al coeficiente que nos interesa, obviamente ese 2 depende del orden en que uno escribi? la regresi?n
  
}

plot(hist(eta_mod1)) #centrado alrededor de 90000 mas o menos, se nota distribuci?n normal aunque la cola izquierda es mas larga

mean(eta_mod1) #Da 90046.18 y en la regresi?n daba 91143
sqrt(var(eta_mod1)) #Da 13139.65 y en la regresi?n daba 8886,41 (error est?ndar - medida de incertidumbre)
quantile(eta_mod1,c(0.025,0.975)) #intervalo de confianza al 95% sabemos que est? entre 64.385 y 114.331


#Boot package

require("boot")

eta.fn<-function(geih_e,index){
  coef(lm(ingtot~age+age2, data = geih_e, subset = index))
}


boot(geih_e, eta.fn, R)

#Bootstrap Statistics :
#      original       bias    std. error
#t1* -391598.363 -538.7261414 229565.4194
#t2*   89516.853   29.8520719  13126.1386
#t3*    -771.785   -0.4411129    168.1169 

#Del paquete boot obtenemos coeficiente 89516.853 y en la regresi?n nos daba 91143,460 (se acerca m?s que de la manera manual)
#De error est?ndar obtenemos 13127 y en la regresi?n daba 8886,41(REVISAR EN LA SALIDA DE LATEX), en ambos casos da mayor que en la regresion sobre muestra

#Standard error: By calculating standard error, you can estimate how representative your sample is of your population and make valid conclusions.
#A high standard error shows that sample means are widely spread around the population mean-your sample may not closely represent your population. 
#Como el error est?ndar aumenta al hacer bootstrap concluimos que puede que tampoco ajusta bien fuera de muestra



#Ahora construirle los intervalos de confianza al peak_age (57)

#para age
lower<-b1-1.96*13126.1386 #63789.62 
upper<-b1+1.96*13126.1386 # 115244.1 
#vemos que este intervalo da muy cercano al que sacamos a mano= entre 64.385 y 114.331 

#para age2
lower2<-b2-1.96*166.9154 #-1098.939
upper2<-b2+1.96*166.9154 # -444.6309 

peak_agel<--(lower/(2*lower2)) #29.02327 

peak_ageu<--(upper/(2*upper2)) #129.5952 

#debido a que los errores estandares son tan altos, observamos que el peak_age variaria entre 29 a?os y 129 a?os


#####################
# 4. The earnings gap
#####################

#Estimate the unconditional earnings gap

#comprobar que esta bien creada la variable fem
head(geih_e$sex) #sex toma valor de 1 para individuo hombre
head(geih_e$fem) #fem toma valor de 1 para individuo mujer

#variables ingtot y logingtot
head(geih_e$ingtot)
head(geih_e$logingtot)

summary(geih_e$ingtot)
summary(geih_e$logingtot)

#Plantear modelo

ols2<-lm(geih_e$logingtot~geih_e$fem) #Regresion propuesta en el taller
ols2 #se corre el modelo y sale coeficiente negativo para mujer

summary(ols2) #R^2 0.012 #Residual standard error: Residual standard error: 0.8723 on 16275 degrees of freedom (cuando teniamos outliers era 1.95)

require("stargazer")
stargazer(ols2) ############## salida ols2 en latex #############

resid2<-resid(ols2)
plot(edad,resid2)

ggplot(data = geih_e , mapping = aes(x = age , y = resid2))+
  geom_point(col = "red" , size = 0.5) #aqui tambien observamos que los datos no se distribuyen aleatoriamente pero mejoran mucho comparado que cuando teniamos outliers

fit2<-fitted(ols2)
par(mfrow=c(2,2))
plot(ols2) #no tiene buen ajuste


#####predict by gender

#lo voy a plantear sacando dos predicts distintos, uno por cada genero, y los compararemos

#primero, con subset de solo mujeres
geih_ef <- select(filter(geih_e, fem == 1),c(logingtot,age,age2,fem))

ols1f<-lm(geih_ef$logingtot~geih_ef$age+geih_ef$age2)

summary(ols1f) #r^2= 0.023
stargazer(ols1f)

ggplot(geih_ef, aes(x=age, y=predict(ols1f))) + 
  geom_point(col = "red" , size = 0.5) #aqui vemos que la peak_age se observa hacia los 40 a?os

#ahora, con subset de solo hombres
geih_em <- select(filter(geih_e, fem == 0),c(logingtot,age,age2,fem))

ols1m<-lm(geih_em$logingtot~geih_em$age+geih_em$age2)

summary(ols1m) #r^2= 0.045
stargazer(ols1m)

ggplot(geih_em, aes(x=age, y=predict(ols1m))) + 
  geom_point(col = "red" , size = 0.5) #aqui vemos que la peak_age se observa hacia los 48 a?os!

#con lo anterior observamos que el peak_age es distinto para hombres y mujeres
#y ademas observamos que ese peak_age corresponde a ingresos mayores para hombres


##ahora vamos a calcular el valor de esos peak_age espec?ficamente
#Recordar: 

#ols1f<-lm(geih_ef$logingtot~geih_ef$age+geih_ef$age2)
#ols1m<-lm(geih_em$logingtot~geih_em$age+geih_em$age2)
#se plantean igual pero recordar que no usan los mismos datos porque tenemos datos filtrados por genero


#derivada de ingtot con respecto a la edad
# b1 + b2*edad*2
#entonces edad***
# edad*** = -b1 / 2*b2 ######
#b1 es el de edad y b2 el de edad^2

#sacamos los coeficientes de las regresiones que corrimos

#para mujeres
coefs1f<-ols1f$coefficients 
coefs1f

b0f<-coefs1f[1]
b1f<-coefs1f[2]
b2f<-coefs1f[3]

peak_agef<--(b1f/(2*b2f))
# peak_agef = 39.76167


#para hombres
coefs1m<-ols1m$coefficients 
coefs1m

b0m<-coefs1m[1]
b1m<-coefs1m[2]
b2m<-coefs1m[3]

peak_agem<--(b1m/(2*b2m))
# peak_agem = 48.35461    #comprobamos que si es mayor el peak age para hombres
 

#resampleo con bootstrap
require("boot")

#bootstrap para mujeres

set.seed(123)
R<-1000

eta.fnf<-function(geih_ef,index){
  coef(lm(logingtot~age+age2, data = geih_ef, subset = index))
}


boot(geih_ef, eta.fnf, R) 

#Bootstrap Statistics :
#        original        bias     std. error
#t1* 12.8497894303 -5.092648e-03 9.977185e-02
#t2*  0.0574768216  3.038761e-04 5.297921e-03
#t3* -0.0007227667 -4.072150e-06 6.500539e-05


#intervalos de confianza al peak_age de mujeres (38)

#para age
lowerf<-b1f-1.96*5.297921e-03 # 0.0470929 
upperf<-b1f+1.96*5.297921e-03 # 0.06786075  

#para age2
lower2f<-b2f-1.96*0.0001691201 # -0.001054242 
upper2f<-b2f+1.96*0.0001691201 # -0.0003912913 
 
peak_agelf<--(lowerf/(2*lower2f)) # 22.33495  
peak_ageuf<--(upperf/(2*upper2f)) # 86.71385   #nuevamente notamos que los intervalos de confianza son demasiado amplios


#bootstrap para hombres

set.seed(123)
R<-1000

eta.fnm<-function(geih_em,index){
  coef(lm(logingtot~age+age2, data = geih_em, subset = index))
}


boot(geih_em, eta.fnm, R) 

#Bootstrap Statistics :
#        original        bias     std. error
#t1* 12.6392737877 -2.054551e-03 9.046846e-02
#t2*  0.0665319340  1.166231e-04 4.872507e-03
#t3* -0.0006879586 -1.758050e-06 5.953093e-05

#intervalos de confianza al peak_age de hombres (46)

#para age
lowerm<-b1m-1.96*4.872507e-03 # 0.05698182
upperm<-b1m+1.96*4.872507e-03 # 0.07608205 

#para age2
lower2m<-b2m-1.96*5.953093e-05 # -0.0008046392 
upper2m<-b2m+1.96*5.953093e-05 # -0.000571278

peak_agelm<--(lowerm/(2*lower2m)) # 35.4083
peak_ageum<--(upperm/(2*upper2m)) # 66.58934

#peak mujeres= entre 22 y 86 a?os
#peak hombres= entre 35 y 66 a?os

#notamos que los intervalos de hombres y de mujeres si tienen overlap,
#en todo caso la edad peak de mujeres continua siendo menor que la de hombres
#los intervalos de confianza son demasiado grandes 
#para el caso de los hombres el intervalo de confianza es mas acotado, esto daria cuenta de que los datos para los hombres tienen mejor ajuste
#lo anterior lo comprobamos al ver que el r^2 del modelo de solo mujeres es de 0.23
#y el de solo hombres es de 0.45 
#quiere decir que en el caso de las mujeres se requieren aun mas variables explicativas que en el caso de los hombres para explicar el ingreso



###Incorporate control variables

#Modelos adicionales 

#recordar que las variables que elegimos inicialmente fueron:
#("age","sex","maxEducLevel", "formal", "estrato", "oficio")] y exp_potencial

#Primer modelo: el mismo de age y age2, pero incorporandole la variable fem (porque en el punto anterior lo que hicimos fue dividir la muestra)

ols3<-lm(logingtot~fem+age+age2,geih_e) #coeficiente fem negativo
summary(ols3) #r^2= 0.037
stargazer(ols3)

#siguiente modelo: incorporando educacion 
ols4<-lm(logingtot~fem+age+age2+maxEducLevel,geih_e) #coeficiente fem negativo, el coef de edad baja porque el de educ es positivo
summary(ols4) #r^2= 0.21 #el r^2 mejora mucho
stargazer(ols4)

#siguiente modelo: controlamos por oficio
ols5<-lm(logingtot~fem+age+age2+maxEducLevel+oficio,geih_e) #la interpretacion de oficio no es clara porque es categorica, pero lo que importa 
#es que ayuda a explicar mejor el modelo y los coeficientes de las otras variables se continuan comportando segun lo esperado
summary(ols5) #r^2= 0.26 #el r^2 continua mejorando
stargazer(ols5)

#siguiente modelo: ver si el trabajo formal implica mejores ingresos
ols6<-lm(logingtot~fem+age+age2+maxEducLevel+oficio+formal,geih_e)
summary(ols6) #r^2= 0.36 #el r^2 continua mejorando y efectivamente el trabajo formal implica mayor ingreso
stargazer(ols6)

#ultimo modelo: ver como se comporta con la variable estrato
ols7<-lm(logingtot~fem+age+age2+maxEducLevel+oficio+formal+estrato,geih_e)
summary(ols7) #r^2 aumenta a 0.44 y la variable de estrato es significativa
#nota: hay que tener cuidado porque estrato podria ser una variable endogena (menor ingreso lleva a elegir menor estrato de residencia)
#sin embargo vivir en barrios de menores estratos puede estar generando dificultad de acceso a trabajos mejor pagos, por lo tanto afectando el ingreso
stargazer(ols7)


#Use FWL to repeat the above estimation, where the interest lies on b2 

#Sacamos un plot rapido de los datos (no es necesario en este caso pero guardo el script)
ggplot(geih_e) +
  geom_point(aes(x=age,y=logingtot))

#lo voy a correr primero solo con la de age, age2 y fem

#recordar que este es el modelo que solo incluye el coeficiente de fem
#ols2<-lm(geih_e$logingtot~geih_e$fem)
summary(ols2) #coeficiente -0.193

### Ahora lo que vamos es hacer probar el FWL Theorem


#Modelo a probar
#Notamos en el analisis anterior que la variable estrato pesa bastante en la especificacion del modelo
#probemos con esta variable de control

ols8<-lm(logingtot~fem+estrato,geih_e)
summary(ols8) #coeficiente -0.226


stargazer(ols2,ols8,type="text") #vemos los dos modelos lado a lado, comparamos coeficiente de fem

#regresion de residuales en residuales

geih_fwl<-geih_e %>% mutate(res_y_e=lm(logingtot~estrato,geih_e)$residuals,
                  res_x_e=lm(fem~estrato,geih_e)$residuals,
)
ols9<-lm(res_y_e~res_x_e,geih_fwl) #y luego se corren los residuales de y contra los de x
stargazer(ols2,ols8,ols9,type="text") #y aqu? vemos que el B es el mismo! o sea que "eliminamos" estrato


#En el modelo de residuales en residuales vemos que el coeficiente no cambia (continua siendo -0.226)
#pero el r^2 aumenta de 0.012 a 0.021, el modelo ajusta mejor
#este mejor ajuste estaria indicando que el problema no es de seleccion y que efectivamente hay un gap de income para las mujeres


#####################
# 5. Predicting earnings
#####################
require("fabricatr")

# a. dos muestras (train y test) - plantear modelos cada vez m?s complejos (5) e irlos comparando

###TODAVÍA NO FUNCIONA, SOLO ES EL CODIGO DE LA CLASE 
geih_pe<-geih_e
#Semilla para siempre obtener los mismos resultados
set.seed(123)
#transormar el ingreso de logaritmo al ingreso estandar  y se genera un indicador lógico que divida la muestra en train y test. Si esta dentro del 30% verdadero, si no falso
geih_pe <- geih_pe %>% 
                  mutate (ing=exp(logingtot), 
                  holdout=as.logical(1:nrow(geih_pe) %in%
                  sample(nrow(geih_pe), nrow(geih_pe)*.3))
                  )

#Definir las submuestras test y train
#podemos utilizar una muestra para entrenar y una para evaluar. 30% de la muestra se va a test y 70% a train

test <-geih_pe[geih_pe$holdout==T,]
train <-geih_pe[geih_pe$holdout==F,]

#i. modelo que solo incluye una constante: Intercepto - media de los ingtot de las observaciones 
model1<-lm(ing~1,data=train)
stargazer(model1, type ="text")


#ii. Estimar los modelos del punto anterior

#NO ESTOY SEGURA PERO CREO QUE SON ESAS
Modeledad<-lm(geih_e$ingtot~geih_e$age+geih_e$age2) #Modelo de Age eargnings profile 
Modelgen<-lm(geih_e$logingtot~geih_e$fem
Modelgap<-logingtot~age+age2+fem


#ols2<-
#ols3<-lm(,geih_f) #continua siendo coeficiente fem negativo
#ols4<-lm(logingtot~age+age2+sex,geih_f) #estas dos regresiones dan lo mismo, solo que para fem el coeficiente es negativo y para sex positivo
#ols5<-lm(logingtot~age+age2+fem+oficio,geih_f)


#iii.Incluir variables que lo complejisen transformando las X:

model2<-lm(logingtot~maxEducLevel+age+fem+estrato,geih_pe) 
model3<-lm(geih_pe$logingtot~educ_time+#educ^2+age+age2+gen+estrato) 
model4<-lm(geih_pe$logingtot~educ_time+exp_potencial+age+age2+gen+estrato) 
model5<-lm(geih_pe$logingtot~educ_time+exp_potencial+age+age2+gen+gen:tipo_oficio+estrato) 
model6<-lm(geih_pe$logingtot~educ_time+exp_potencial+age+age2+gen+estrato+gen:tipo_oficio+estrato+gen:estrato) 

#iv.Reportar y comparar los MSE de todos los modelos (para comparar un grafico sería muy explicativo)

#Vamos a predecir FUERA de muestra
test$model1<-predict(model1,newdata=test)
with (test, mean((ing-model1)^2))

test$model2<-predict(model1,newdata=test)
with (test, mean((ing-model2)^2))

test$model3<-predict(model1,newdata=test)
with (test, mean((ing-model3)^2))

test$model4<-predict(model1,newdata=test)
with (test, mean((ing-model4)^2))

test$model5<-predict(model1,newdata=test)
with (test, mean((ing-model5)^2))

test$model6<-predict(model1,newdata=test)
with (test, mean((ing-model6)^2))



##FALTARÍAN LOS DEL PUNTO ii



#v. 


#para el peor modelo, buscar outliers


##Este para MRE: 
GIH<-data.frame(age=runif(30,18,80))
GIH<- GIH %>% mutate(age2=age^2,
                     income=rnorm(30,mean=12+0.06*age-0.001*age2))                



#b. repetir usando k-fold

N<-1000
GIH<-data.frame(age=runif(N,18,80))
GIH<- GIH %>% mutate(age2=age^2,
                     income=rnorm(N,mean=12+0.06*age-0.001*age2))                


# model to fit
# Method: crossvalidation, 5 folds
# specifying regression model
model1<-train(income~.,                                                     
              data = GIH,
              trControl = trainControl(method = "cv", number = 5),     
              method = "null")                                            

model1

#c. repetir usando LOOCV pero solo con un modelo de los 5 planteados

for(i in 1:dim(GIH)[1]){
  #Estimate the regression model using all but the i − th observation
  reg_1<-lm(income~age+age2,GIH[-i,])
  #Calculate the prediction error for the i − th observation, i.e. (yi − yˆi)
  y_hat<-predict(reg_1,newdata=GIH[i,])
  u<-(GIH[i,]$income-y_hat)^2
}





