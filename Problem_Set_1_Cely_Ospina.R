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

geih<-rbind(geih1, geih2, geih3, geih4, geih5, geih6, geih7, geih8, geih9, geih10)



#####################
# 2. Data Cleaning
#####################

#debe ser solo para mayores de 18 años
GEIH<-geih[!(geih$age<18),]
df2

#ELECCION DE X
#Elecci?n de variables significativas (fijarnos en los talleres del a?o pasado)
#Primero buscar las variables y aislarlas
#raza, genero, edad

#year<-US90$year
#ejemplo<-geih$p6760
#raza<-geih$xxxxxx   

edad<-geih$age
educ<-geih$maxEducLevel
#Revisar en qué esta medido, debería ser años, creo que esa la creo ignacio

gen<-geih$sex

Exp<-geih$(age-maxEducLevel)-5	
#eperiencia potencial (esta la usamos en el intersemestral): En la literatura se ha utilizado como proxy de 
#la experiencia la experiencia potencial. Esta nace de restarle a la edad de la persona los años que ha estudiado 
#y, además, cinco (5) años –pues en sus años de primera infancia ni estudió ni trabajó.
##No he logrado crearla

#Segundo, verificar en esas variables si hay missings

is.na(geih$educ)
#ver como se encuentran los missings
#analizar caso a caso (por variable) como los vamos a manejar


#Tercero, estad?sticas descriptivas, tablas y figuras











#####################
# 3. Age-earnings profile
#####################

#ELECCI?N DE Y (INCOME)

#justificarla


#Correr OLS de income y age
#que tan bien ajusta sin partir la muestra
#graficar

#usar bootstrap (revisar bien la intuici?n)








#####################
# 4. The earnings gap
#####################

#gender

#lo mismo del anterior pero con gender, igualmente usar bootstrap, pendiente entender



#luego meter variables de control


#FWL para sacar las variables de control pero teoricamente nos debe dar lo mismo







#####################
# 5. Predicting earnings
#####################


# a. dos muestras (train y test) - plantear modelos cada vez m?s complejos (5) e irlos comparando

#para el peor modelo, buscar outliers



#b. repetir usando k-fold


#c. repetir usando LOOCV pero solo con un modelo de los 5 planteados





