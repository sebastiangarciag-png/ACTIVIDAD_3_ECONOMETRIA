# Librerias ----------------

library("skimr") # Libreria para descriptivos generales y estadíticos de los datos
library("readxl") # Libreria para leer archivos EXCEL
library("stringr") # Libreria para trabajar ccon variables tipo cadena (Cualitativas)
library("haven") # Libreria para leer archivos .dta (Stata)
library("tidyverse") # Libreria para el analisis, transformación, manipulación de datos
library("openxlsx") # Libreria para cargar archivos de EXCEL pero tambien para guardar archivos en .xlsx
library("stringi") # Libreria para variables cadena
library("hrbrthemes") # Libreria para graficos
library("plyr") # Libreria para lamanipulación y transformación de los datos

## Cargamos base de datos
DATA <- read.csv(file.choose(), sep = ",") |> data.frame()
DATA = read.csv2("C:/Users/User/Desktop/QUINTO SEMESTRE/ECONOMETRIA/UNIDAD 2/ACTIVIDADES/ACTIVIDAD 3/ACTIVIDAD_3_ECONOMETRIA.csv" , sep = ",") |> data.frame()

## Descriptivo de la base de datos-----------------
head(DATA)
str( DATA , list.len = 492 )
skimr::skim(DATA)

###
table(DATA$AREA , useNA = "always")
table(DATA$MES , useNA = "always" )
table(DATA$OCI , useNA = "always" )

###
table(DATA$AREA , useNA = "always")
table(DATA$MES , useNA = "always" )
table(DATA$OCI , useNA = "always" )

## Depuración / Limpieza o transformación -------------------
for (N in names(DATA) ) {
  if (class( DATA[[N]]) == "integer") {
    DATA[[N]] = as.numeric(DATA[[N]])
  }
}

str(DATA$FEX_C18)
str(DATA$P3271)
table(DATA$P3271 , useNA = "always" )

DATA = DATA |> dplyr::mutate(
  SEXO = case_when(
    P3271 == # ¿Cuál fue su sexo al nacer?
      2 ~ "Mujer" ,
    P3271 == 1 ~ "Hombre" ,
    TRUE ~ "NN"
  ) ,
  SEXO_DUMMY = case_when(
    P3271 == # ¿Cuál fue su sexo al nacer?
      2 ~ 0 ,
    P3271 == 1 ~ 1 ,
    TRUE ~ NA ))

table(DATA$P3271 , DATA$SEXO , useNA = "always" )

table(DATA$P3271 , DATA$SEXO_DUMMY , useNA = "always" )