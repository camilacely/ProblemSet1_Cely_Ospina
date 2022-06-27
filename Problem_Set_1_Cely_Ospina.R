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

#^Shortcut para cargar la base sin hacer scraping


#####################
# 2. Data Cleaning
#####################


#Focus on individuals older than eighteen
geih18<-geih[!(geih$age<18),] #24.568 observaciones

#Focus on employed individuals

geih18e<-geih18[!(geih18$ocu<1),] #16.542 observaciones #ocu = 1 if occupied, 0 otherwise

oc<-geih18$ocu

########Base final elegida= geih18e#################

#ELECCION DE VARIABLES EXPLICATIVAS RELEVANTES (X)


edad<-geih18e$age
educ<-geih18e$maxEducLevel
gen<-geih18e$sex

head(geih18e$sex)

#se recodifica la variable genero con el fin de encontrar cual es el efecto de ser mujer, al especificar este como 1 y hombre como 0
geih18e <- geih18e %>% 
  mutate(fem = ifelse(test = sex > 0 , #notar que sex tomaba valor de 1 para hombre
                      yes = 0, 
                      no = 1))

head(geih18e$fem)  #fem toma valor de 1 para individuo mujer #si comparamos este head con el de sex vemos que se invierte


#
is.na(geih_f$logingtot)
sum(is.na(geih_f$logingtot)) #no hay missing values

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
                     educ <= 9 ~ 0,) ##Nota, pendiente usar esto para sacar promedios e imputar valores a los que reportan ingresos de cero
  
exp_potencial<-edad-educ_time-5

tipo_oficio<-geih18e$oficio
formal<-geih18e$formal
dpto<-geih18e$dpto

#Notas para m?s adelante (revisemos) 
#de pronto en los modelos m?s complejos podemos controlar por depto, ya que en las regiones los salarios son distintos
#otra: variable "informal", por mas que una persona trabaje x horas si es de manera informal seguramente recibe menos, ver si esto se cubre con la interaccion educ--hoursWorkUsual 
#otra: second job? hoursWorkActualSecondJob 
#otra: P6050 es la variable que dice la relaci?n con "jefe del hogar", ver si sacamos de ac? los hijos
#otra: ingreso por arriendos? > ya la incluimos con ingtot
#otra: controlar por oficio?


#Missing values analysis

#Para la base < 18
is.na(geih18e)
colSums(is.na(geih18e))
colSums(is.na(geih18))>0
colnames(geih18e)[colSums(is.na(geih18e))>0] #Aqu? nos aparecen los nombres de las columnas que tienen missing values

#Ahora analizando las variables que escogimos como explicativas
#edad, educ, gen, exp_potencial

is.na(geih18e$age) 
sum(is.na(geih18e$age)) #no hay missing values

is.na(geih18e$maxEducLevel) 
sum(is.na(geih18e$maxEducLevel)) #1 missing value

is.na(geih18e$sex) 
sum(is.na(geih18e$sex)) #no hay missing values

is.na(exp_potencial) 
sum(is.na(exp_potencial)) #Como esta variable depende de edad y de maxEducLevel, tiene 1 missing value

#Nota: (Considero que al tener un ?nico missing value, por ahora podemos no hacer nada al respecto porque no creo que esa ?nica observaci?n afecte los resultados)

dim(geih18e)
str(geih18e)
names(geih18e)

head(geih18e[,c("age","maxEducLevel","sex")])
tail(geih18e[,c("age","maxEducLevel","sex")])

summary(geih18e$sex, geih18e$gen)
summary(geih18e$age) #el 75% de los encuestados tiene menos de 50 a?os
summary(geih18e$maxEducLevel) #Este sum no tiene mucho sentido porque es variable categ?rica -> 1 missing 
summary(educ_time) #en promedio, las personas encuestadas tienen 12,42 a?os de educaci?n (secundaria completa)
summary(geih18e$sex) #53% de los encuestados son hombres
summary(exp_potencial) #en promedio, las personas encuestadas tienen 22 a?os de experiencia laboral

skim(geih18e) #Esto saca estad?sticas de todas las variables pero la base tiene muchas columnas, por lo cual crear? un subset
subset <- geih18e %>% select(age, maxEducLevel, sex)
skim(subset) #En todo caso no dice mucho porque dos de las variables son categ?ricas

#ELECCION DE VARIABLE Y
geih_e<-geih18e
ing<-geih18e$ingtot 
geih_e$ingtot[geih_e$ingtot == 0] <- 1
geih_e <- geih_e %>% mutate(logingtot = log(ingtot))
#Se toma el logaritmo del ingreso con el fin de normalizar la distribución de este 

summary(geih18e$ingtot)

logingtot<-geih_e$logingtot 
#justificación: Se escoge la variable ingtot pues está teniendo en cuenta tanto valores observados para el ingreso como valores imputados. Se tiene en cuenta el ingreso laboral, ingresos de otras fuentes (como arriendos) e ingresos que debería tener de acuerdo con las características observadas.  
#Se asume que el DANE hace un ejercicio confiable en la imputación al ser una fuente confiable. 

##obtenemos estadísticas descriptivas de las variables seleccionadas
#NOTA: HARÍA FALTA EXP_POTENCIAL
subset2 <- geih_e %>% select(age, sex, maxEducLevel, formal, dpto, oficio)
skim(subset2)

##Distribución en densidad de ingreso
d <- ggplot(geih_e, aes(x=logingtot)) + 
  geom_density()
d+ geom_vline(aes(xintercept=mean(logingtot)),
              color="blue", linetype="dashed", size=1)


#Podemos ver como está distribuido el ingreso de acuerdo con el genero de la persona encuestada. Podemos ver de forma muy introductoria que se presentan outliers.  
ggplot(data = geih_e , mapping = aes(x = age , y = logingtot))+
  geom_point(col = "tomato" , size = 0.5)

p <- ggplot(data=geih_e) + 
  geom_histogram(bins=30, mapping = aes(x=logingtot , group=as.factor(gen) , fill=as.factor(gen)))
  
p + scale_fill_manual(values = c("0"="tomato" , "1"="steelblue") , label = c("0"="Hombre" , "1"="Mujer") , name = "Genero")

box_plot <- ggplot(data=geih_e , mapping = aes(as.factor(educ_time) , logingtot)) + 
  geom_boxplot() + geom_point(aes(colour=as.factor(gen))) +
  scale_color_manual(values = c("0"="tomato" , "1"="steelblue") , label = c("0"="Hombre" , "1"="Mujer") , name = "Genero")
box_plot
##Creo que se puede diferenciar mejor en el boxplot y en el geom_point creo que con solo una estaría bien. 

#En este histograma podemos ver la distribucion del nivel de educacion maximo de la muestra diferenciado por el genero de los encuestados. 
ggplot(geih_e, aes(x= educ_time)) + geom_bar(width=0.5, colour="blue", fill="steelblue") +  
  geom_text(aes(label=..count..), stat='count',position=position_dodge(0.9), vjust=-0.5,  size=5.0)+  
  facet_wrap(~gen)


#simetría de la variable ingreso: 
BoxCoxTrans(geih_e$ing)

#Pendiente completar pero en general es cacharrearle, ya no necesita concatenaci?n
#?tiles: clase del 11 de junio y Intro_to_R (bloque ne?n)



#####################
# 3. Age-earnings profile
#####################

#ELECCION DE Y (INCOME) 

summary(geih18e$ingtot) #El 75% de los encuestados gana menos de 1'723.000, sin embargo el promedio es de 1'769.000, por lo cual
#podemos concluir que el 25% de mayores ingresos est? arrastrando ese promedio

ing<-geih18e$ingtot 

#Justificacion: Se escoge la variable ingtot pues esta teniendo en cuenta tanto valores observados para el ingreso como valores imputados. Se tiene en cuenta el ingreso laboral, ingresos de otras fuentes (como arriendos) e ingresos que deberia tener de acuerdo con las caracteristicas observadas.  
#Se asume que el DANE hace un ejercicio confiable en la imputacion al ser una fuente confiable. 


#Correr OLS de income y age

edad2<-edad^2

ols1p<-lm(geih18eb$ingtot~geih18eb$age+geih18eb$age2)
ols1<-lm(ing~edad+edad2) 
ols1
summary(ols1) #R^2 0.017





#Por cada a?o adicional de vida, las personas ganan en promedio 91.000 pesos adicionales
#Edad^2 tiene coeficiente negativo, por lo cual sabemos que esta funci?n no es lineal sino decreciente


resid1<-resid(ols1)
plot(edad,resid1)

ggplot(data = geih18e , mapping = aes(x = age , y = resid1))+
  geom_point(col = "red" , size = 0.5)

fit1<-fitted(ols1)
par(mfrow=c(2,2))
plot(ols1)

ggplot(data = geih18e , mapping = aes(x = age , y = ingtot))+
  geom_point(col = "red" , size = 0.5)

ggplot(data = geih18e , mapping = aes(x = age , y = ingtot))+
  geom_point(col = "red" , size = 0.5) + stat_smooth(method= "lm", col="red")


#que tan bien ajusta sin partir la muestra

#graficar






#Bootstrap= resample from the sample

#Manual

require("stargazer")
stargazer(ols1)

set.seed(123)
length(geih18e$age)
R<-1000 #num de repeticiones

eta_mod1<-rep(0,R) #vector de ceros de numero R

geih18eb<-geih18e #No quiero modificar la base original entonces le creo una copia
geih18eb$age2<-geih18eb$age^2 #Aqu? si meto age^2

for(i in 1:R){
  geih_sample<-sample_frac(geih18eb,size=1,replace=TRUE) #muestra del mismo tama?o que la original - con reemplazo
  f<-lm(ingtot~age+age2,geih_sample)
  coefs<-f$coefficients #agarrame los coeficientes de f, o sea de la reg lineal
  eta_mod1[i]<-coefs[2] #irlos reemplazando en mi vector de ceros, y el 2 corresponde al coeficiente que nos interesa, obviamente ese 2 depende del orden en que uno escribi? la regresi?n
  
}

plot(hist(eta_mod1)) #centrado alrededor de 90000 mas o menos, se nota distribuci?n normal

mean(eta_mod1) #Da 90936 y en la regresi?n daba 91143
sqrt(var(eta_mod1)) #Da 12286,59 y en la regresi?n daba 8886,41 (error est?ndar - medida de incertidumbre)
quantile(eta_mod1,c(0.025,0.975)) #intervalo de confianza al 95% sabemos que est? entre 65.667 y 113.925

#Boot package

require("boot")

eta.fn<-function(geih18eb,index){
  coef(lm(ingtot~age+age2, data = geih18eb, subset = index))
}


boot(geih18eb, eta.fn, R)

#Bootstrap Statistics :
#        original      bias    std. error
#t1* -436662.9269 429.0191126 216771.6646
#t2*   91143.4584  60.7704062  12339.3418  #este es el que analizamos
#t3*    -799.2612  -0.9719388    156.7125

#Del paquete boot obtenemos coeficiente 91143,458 y en la regresi?n nos daba 91143,460 (se acerca m?s que de la manera manual)
#De error est?ndar obtenemos 12339,34 y en la regresi?n daba 8886,41 


#Peak age= derivar e igualar a cero, construir los intervalos de confianza a partir de los errores est?ndares
# CI=[coef???1.96?SE,coef+1.96?SE] #CI: confidence intervals #SE: standard error

#Plot predict
geih_ig<-geih18e
geih_ig<-data.frame(age=runif(30,18,80))
geih_ig<- geih_ig %>% mutate(age2=age^2,
                     ingtot=rnorm(30,mean=12+0.06*age-0.001*age2)) #crear ingtot en esta submuestra 
reg_1<-lm(ingtot~age+age2,geih_ig)
ggplot(geih_ig , mapping = aes(x = age , y = predict(reg_1))) +
  geom_point(col = "red" , size = 0.5)



#####################
# 4. The earnings gap
#####################

#Estimate the unconditional earnings gap

geih_f<-geih18e

geih_f <- geih_f %>% 
  mutate(fem = ifelse(test = sex > 0 , 
                            yes = 0, 
                            no = 1))

summary(geih_f$sex, geih_f$fem)
head(geih_f$fem)  #fem toma valor de 1 para individuo mujer
head(geih_f$sex)  #sex toma valor de 1 para individuo hombre

geih_f$ingtot[geih_f$ingtot == 0] <- 1
geih_f <- geih_f %>% mutate(logingtot = log(ingtot))

logingtot<-geih_f$logingtot  

head(geih_f$ingtot)
head(geih_f$logingtot)
summary(geih_f$ingtot)
summary(geih_f$logingtot)

ols2<-lm(geih_f$logingtot~geih_f$fem) #Regresi?n propuesta en el taller
ols2
ols2<-lm(geih_f$logingtot~geih_f$fem) #Regresi?n propuesta en el taller
ols2 #se corre el modelo y sale coeficiente negativo para mujer


summary(ols2) #R^2 0.009
require("stargazer")
stargazer(ols2)

#pruebas
geih_f$age2<-geih_f$age^2 
ols3<-lm(logingtot~age+age2+fem,geih_f) #continua siendo coeficiente fem negativo
ols4<-lm(logingtot~age+age2+sex,geih_f) #estas dos regresiones dan lo mismo, solo que para fem el coeficiente es negativo y para sex positivo

#predict by gender (pendiente terminar)
geih_igf<-geih18e
geih_igf<-data.frame(age=runif(30,18,80),fem=runif(30,18,80)) ##PENDIENTE SACAR ESTO PORQUE NO SALE BIEN Y NO ENTIENDO EL CODIGO
geih_igf<- geih_igf %>% mutate(age2=age^2,
                             
                             ingtot=rnorm(30,mean=12+0.06*age-0.001*age2)) #aqui crea ingtot en esta submuestra 
reg_2<-lm(ingtot~age+age2+fem,geih_ig)
ggplot(geih_ig , mapping = aes(x = age , y = predict(reg_2))) +
  geom_point(col = "red" , size = 0.5)

#analysis, pendiente sacar para los otros modelos relevantes

resid2<-resid(ols2)
plot(edad,resid2)

ggplot(data = geih_f , mapping = aes(x = age , y = resid2))+
  geom_point(col = "red" , size = 0.5)

fit2<-fitted(ols2)
par(mfrow=c(2,2))
plot(ols2)

ggplot(data = geih_f , mapping = aes(x = age , y = logingtot))+
  geom_point(col = "red" , size = 0.5)

ggplot(data = geih_f , mapping = aes(x = age , y = logingtot))+
  geom_point(col = "red" , size = 0.5) + stat_smooth(method= "lm", col="red")


#bootstrap

require("boot")
set.seed(123)
R<-1000

eta.fnf<-function(geih_f,index){
  coef(lm(logingtot~age+age2+fem, data = geih_f, subset = index))
}


boot(geih_f, eta.fnf, R) 

#Bootstrap Statistics :
#       original        bias     std. error
#t1* 12.244988312  5.444330e-03 0.1683361961
#t2*  0.088162861 -2.915204e-04 0.0087396536
#t3* -0.001027477  3.550194e-06 0.0001055477
#t4* -0.398860921  1.291200e-03 0.0307740015

#^esto da muy parecido a la regresion ols3 entonces lo que hay que hacer es nuevamente comparar los std. errors



########variables de control, pendiente completar

summary(geih_f$oficio)
as.factor(geih_f$oficio)
ols5<-lm(logingtot~age+age2+fem+oficio,geih_f)
summary(ols5)



#FWL para sacar las variables de control pero teoricamente nos debe dar lo mismo

##########
##VOY A PEGAR AQU? MI SCRIPT COMENTADO DE JUNIO 8 DONDE NOS EXPLICARON COMO SACAR EL FWL THEOREM
#######
########pendiente corregir las variables

ggplot(db) +
  geom_point(aes(x=x,y=y))
#esto nos saca un plot de los datos, aes quiere decir aesthetic, ah? ponemos que va en cada eje

reg1<-lm(y~x,data=db)
summary(reg1)
#regresi?n lineal


require("stargazer")
stargazer(reg1,type="text")
#Esto es parecido a outreg, la salida es mas parecida a las comunes en econom?a 


### Ahora lo que vamos es hacer probar el FWL Theorem

#Primero "a mano"
#Crear dummy
db<- db %>% mutate(ej=c(rep(0,30),1)) #Esto crea un valor de 1 en la posici?n 31, donde sabemos que est? el outlier
head(db)
tail(db)

#regresi?n que incluye la dummy
reg2<-lm(y~x+ej,db)

#Aqu? vemos los resultados de reg1 y reg2
stargazer(reg1,reg2,type="text")

## Entonces lo que vemos es que poner la dummy para ESA OBSERVACI?N hace que la "desaparezcamos"

#Ahora lo que vamos a analizar es la regresi?n de residuales en residuales - FWL Theorem ahora s?

##Creamos los residuales

#Correr y contra ej y luego x contra ej, llamar los residuales de cada regresi?n
db<-db %>% mutate(res_y_e=lm(y~ej,db)$residuals,
                  res_x_e=lm(x~ej,db)$residuals,
)
reg3<-lm(res_y_e~res_x_e,db) #y luego se corren los residuales de y contra los de x
stargazer(reg1,reg2,reg3,type="text") #y aqu? vemos que el B es el mismo de cuando lo hicimos a mano!



## AHORA : leverage (ser? una cuarta regresi?n)

db<-db %>% mutate(res_y_x=lm(y~x,db)$residuals, #Aqu? sacamos los residuales de y contra x
                  res_e_x=lm(ej~x,db)$residuals, #y luego los de ej contra x
)
reg4<-lm(res_y_x~res_e_x,db) #y corremos esas dos cosas
stargazer(reg1,reg2,reg3,reg4,type="text") #y aqu? vemos que nos da el "PESO" (leverage), o sea cuanto me est? tirando esa observaci?n en los datos, que es el mismo B de la variable dummy que habiamos creado


##Calcular alfa a mano

u<-lm(y~x,data=db)$residual[31]
u

h<-lm.influence(reg1)$hat[31]
h

alpha<-u/(1-h)
alpha #Esto es lo mismo que nos da en la regresi?n, o sea estamos probando varias maneras de sacar lo mismo

#El FWL Theorem se cumple siempre porque es una propiedad num?rica, no estad?stica

#Podr?amos por ejemplo comparar el leverage entre esa observaci?n, la 31, y otra normalita, por decir la 29. El leverage, entre m?s cerca est?n a la media de x, va a ser menor.




#####################
# 5. Predicting earnings
#####################
require("fabricatr")

# a. dos muestras (train y test) - plantear modelos cada vez m?s complejos (5) e irlos comparando

###TODAVÍA NO FUNCIONA, SOLO ES EL CODIGO DE LA CLASE 
geih_pe<-geih18e
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

#i. modelo que solo incluye una constante: 
model1<-lm(ing~1,data=train)
summary (model1)

#Vamos a predecir FUERA de muestra
test$model1<-predict(model1,newdata=test)
with (test, mean((ing-model1)^2))

#ii.Incluir variables que lo complejisan:

#model#<-lm(ing~educ_time+exp_potencial+poly(exp_potencial,3)+gen+gen:tipo_oficio+depto) aquí faltaría la de hijos
#incluir del punto anterior

#iv.ggplot que compare los MRE en los 5 modelos

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



model1<-train(income~.,                                                     # model to fit
              data = GIH,
              trControl = trainControl(method = "cv", number = 5),     # Method: crossvalidation, 5 folds
              method = "null")                                            # specifying regression model

model1

#c. repetir usando LOOCV pero solo con un modelo de los 5 planteados

for(i 1:dim(GIH)[1]){
  #Estimate the regression model using all but the i − th observation
  reg_1<-lm(income~age+age2,GIH[-i,])
  #Calculate the prediction error for the i − th observation, i.e. (yi − yˆi)
  y_hat<-predict(reg_1,newdata=GIH[i,])
  u<-(GIH[i,]$income-y_hat)^2
}



#DE ESTA CLASE NO TOM? APUNTES EN R ENTONCES TOCA BUSCAR EL SCRIPT DIRECTAMENTE EN 
#Semana 2 - W2_02_Overfit_CrossVal


