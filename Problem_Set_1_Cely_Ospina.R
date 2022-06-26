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

geih<-rbind(geih1, geih2, geih3, geih4, geih5, geih6, geih7, geih8, geih9, geih10) #31.177 observaciones

#save(geih, file = "C:/Users/Camila Cely/Documents/GitHub/ProblemSet1_Cely_Ospina/geih.Rdata")
#load("C:/Users/Camila Cely/Documents/GitHub/ProblemSet1_Cely_Ospina/geih.Rdata")


#####################
# 2. Data Cleaning
#####################


#Focus on individuals older than eighteen
geih18<-geih[!(geih$age<18),] #24.568 observaciones

#Focus on employed individuals
oc<-geih18$ocu
geih18e<-geih18[!(geih18$ocu<1),] #16.542 observaciones #ocu = 1 if occupied, 0 otherwise


#ELECCION DE VARIABLES EXPLICATIVAS RELEVANTES (X)

edad<-geih18e$age
educ<-geih18e$maxEducLevel
gen<-geih18e$sex

#Experiencia potencial
#En la literatura se ha utilizado como proxy de la experiencia la experiencia potencial.
#Esta nace de restarla a la edad de la persona, los años que ha estudiado 
#Y además cinco años adicionales, pues en su primera infancia ni estudia ni trabaja

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

#Notas para más adelante (revisemos) 
#de pronto en los modelos más complejos podemos controlar por depto, ya que en las regiones los salarios son distintos
#otra: variable "informal", por mas que una persona trabaje x horas si es de manera informal seguramente recibe menos, ver si esto se cubre con la interaccion educ--hoursWorkUsual 
#otra: second job? hoursWorkActualSecondJob 
#otra: P6050 es la variable que dice la relación con "jefe del hogar", ver si sacamos de acá los hijos
#otra: ingreso por arriendos?
#otra: controlar por oficio?


#Missing values analysis

#Para la base < 18
is.na(geih18e)
colSums(is.na(geih18e))
colSums(is.na(geih18))>0
colnames(geih18e)[colSums(is.na(geih18e))>0] #Aquí nos aparecen los nombres de las columnas que tienen missing values

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

#Nota: (Considero que al tener un único missing value, por ahora podemos no hacer nada al respecto porque no creo que esa única observación afecte los resultados)




#Estadísticas descriptivas 

dim(geih18e)
str(geih18e)
names(geih18e)

head(geih18e[,c("age","maxEducLevel","sex")])

summary(geih18e$age) #el 75% de los encuestados tiene menos de 50 años
summary(geih18e$maxEducLevel) #Este sum no tiene mucho sentido porque es variable categórica
summary(educ_time) #en promedio, las personas encuestadas tienen 12,42 años de educación (secundaria completa)
summary(geih18e$sex) #53% de los encuestados son hombres
summary(exp_potencial) #en promedio, las personas encuestadas tienen 22 años de experiencia laboral

summary(geih18e$impa) #Propuesta de variable Y de ingreso: impa. 248 missing values, revisar, además hay varios valores de cero y teóricamente no tenemos desempleados!

skim(geih18e) #Esto saca estadísticas de todas las variables pero la base tiene muchas columnas, por lo cual crearé un subset
subset <- geih18e %>% select(age, maxEducLevel, sex)
skim(subset) #En todo caso no dice mucho porque dos de las variables son categóricas


#Gráficas (Pendiente! Principalmente porque debemos sacarlas contra ingreso, entonces pendiente definir la Y)
ggplot(data = subset , mapping = aes(x = age , y = age))+
  geom_point(col = "red" , size = 0.5)

#Pendiente completar pero en general es cacharrearle, ya no necesita concatenación
#útiles: clase del 11 de junio y Intro_to_R (bloque neón)



#####################
# 3. Age-earnings profile
#####################

#ELECCI?N DE Y (INCOME) #SARA puedes ver cuál variable usamos en el taller del año pasado? Yo propongo impa


summary(geih18e$impa)
ing<-geih18e$impa #esto es para tener el script, si algo cambiamos la variable si se requiere

#justificarla


#Correr OLS de income y age

edad2<-edad^2

ols1<-lm(ing~edad+edad2) #aquí lo puse con ing pero probablemente deberiamos crear el logaritmo del ingreso
ols1

#que tan bien ajusta sin partir la muestra

#graficar

#usar bootstrap (revisar bien la intuici?n)
#para esto, está el código en las diapositivas de Semana 2 W2_01_Uncertainty








#####################
# 4. The earnings gap
#####################

#gender

#lo mismo del anterior pero con gender, igualmente usar bootstrap, pendiente entender



#luego meter variables de control


#FWL para sacar las variables de control pero teoricamente nos debe dar lo mismo

##########
##VOY A PEGAR AQUÍ MI SCRIPT COMENTADO DE JUNIO 8 DONDE NOS EXPLICARON COMO SACAR EL FWL THEOREM
#######

ggplot(db) +
  geom_point(aes(x=x,y=y))
#esto nos saca un plot de los datos, aes quiere decir aesthetic, ahí ponemos que va en cada eje

reg1<-lm(y~x,data=db)
summary(reg1)
#regresión lineal


require("stargazer")
stargazer(reg1,type="text")
#Esto es parecido a outreg, la salida es mas parecida a las comunes en economía 


### Ahora lo que vamos es hacer probar el FWL Theorem

#Primero "a mano"
#Crear dummy
db<- db %>% mutate(ej=c(rep(0,30),1)) #Esto crea un valor de 1 en la posición 31, donde sabemos que está el outlier
head(db)
tail(db)

#regresión que incluye la dummy
reg2<-lm(y~x+ej,db)

#Aquí vemos los resultados de reg1 y reg2
stargazer(reg1,reg2,type="text")

## Entonces lo que vemos es que poner la dummy para ESA OBSERVACIÓN hace que la "desaparezcamos"

#Ahora lo que vamos a analizar es la regresión de residuales en residuales - FWL Theorem ahora sí

##Creamos los residuales

#Correr y contra ej y luego x contra ej, llamar los residuales de cada regresión
db<-db %>% mutate(res_y_e=lm(y~ej,db)$residuals,
                  res_x_e=lm(x~ej,db)$residuals,
)
reg3<-lm(res_y_e~res_x_e,db) #y luego se corren los residuales de y contra los de x
stargazer(reg1,reg2,reg3,type="text") #y aquí vemos que el B es el mismo de cuando lo hicimos a mano!



## AHORA : leverage (será una cuarta regresión)

db<-db %>% mutate(res_y_x=lm(y~x,db)$residuals, #Aquí sacamos los residuales de y contra x
                  res_e_x=lm(ej~x,db)$residuals, #y luego los de ej contra x
)
reg4<-lm(res_y_x~res_e_x,db) #y corremos esas dos cosas
stargazer(reg1,reg2,reg3,reg4,type="text") #y aquí vemos que nos da el "PESO" (leverage), o sea cuanto me está tirando esa observación en los datos, que es el mismo B de la variable dummy que habiamos creado


##Calcular alfa a mano

u<-lm(y~x,data=db)$residual[31]
u

h<-lm.influence(reg1)$hat[31]
h

alpha<-u/(1-h)
alpha #Esto es lo mismo que nos da en la regresión, o sea estamos probando varias maneras de sacar lo mismo

#El FWL Theorem se cumple siempre porque es una propiedad numérica, no estadística

#Podríamos por ejemplo comparar el leverage entre esa observación, la 31, y otra normalita, por decir la 29. El leverage, entre más cerca estén a la media de x, va a ser menor.




#####################
# 5. Predicting earnings
#####################


# a. dos muestras (train y test) - plantear modelos cada vez m?s complejos (5) e irlos comparando

#para el peor modelo, buscar outliers



#b. repetir usando k-fold


#c. repetir usando LOOCV pero solo con un modelo de los 5 planteados


#DE ESTA CLASE NO TOMÉ APUNTES EN R ENTONCES TOCA BUSCAR EL SCRIPT DIRECTAMENTE EN 
#Semana 2 - W2_02_Overfit_CrossVal


