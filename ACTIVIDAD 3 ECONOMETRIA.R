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
library("car")
library("stargazer")


## Cargamos base de datos
DATA_FILTRAR = haven::read_dta("C:\Users\User\Desktop\QUINTO SEMESTRE\ECONOMETRIA\UNIDAD 2\ACTIVIDADES\ACTIVIDAD 3\ACTIVIDAD_3_ECONOMETRIA") |> data.frame()

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

  dplyr::mutate (
    CIIU = case_when(
      RAMA2D_R4 == 0 ~ "No informa"
      # A
      RAMA2D_R4 %in% seq( from = 1 , to = 3 , by = 1) ~ "Agricultura, ganaderia, caza silvicultura y pesca" ,
      #B
      RAMA2D_R4 %in% seq( from = 5 , to = 9 , by = 1) ~ "Explotacion de minas y canteras" ,
      #C
      RAMAD2D_R4 %in% seq( from = 10 , to = 33, by = 1) ~ "Industria manufacturera" , 
      #D
      RAMAD2D_R4 == 35 ~ "Suministro de electricidad, gas, vapor y aire acondicionado" , 
      #E
      RAMAD2D_R4 %in% seq( from = 36 , to = 39 , by = 1) ~ "Distribucion de agia; evacuacion y tratamiento de" , 
      #F
      RAMAD2D_R4 %in% seq( from = 41 , to = 43 , by = 1) ~ "Construccion" , 
      #G
      RAMAD2D_R4 %in% seq( from = 45 , to = 47 , by = 1) ~ "Comercio al por mayor y al por menor; reparacion de" , 
      #H
      RAMAD2D_R4 %in% seq( from = 49 , to = 53 , by = 1) ~ "Transporte y almacenamiento" , 
      #I 
      RAMAD2D_R4 %in% seq( from = 51 , to = 56 , by = 1) ~ "Alojamiento y servicios de comida" , 
      #J 
      RAMAD2D_R4 %in% seq( from= 58 , to = 63 , by = 1) ~ "Informacion y comunicaciones" , 
      #K 
      RAMAD2D_R4 %in% seq( from = 64 , to = 66 , by = 1) ~ "Actividades financieras y segurso" , 
      #L 
      RAMAD2D_R4 == 68 ~ "Actividades inmobiliria"
      #M 
      RAMAD2D_R4 %in% seq( from = 69 , to = 75 , by = 1 ) ~ "Actividades profesionales cientificas y tecnicas" , 
      #N 
      RAMAD2D_R4 %in% seq( from = 77 , to = 82 , by = 1 ) ~ "Actividades de servicios administrativos y de apoyo"  , 
      #O 
      RAMAD2D_R4 == 84 ~ "Administracion pública y defensa; planes de seguridad social de afiliacion obligatoria" , 
      #P 
      RAMAD2D_R4 == 85 ~ "Educacion" , 
      #Q 
      RAMAD2D_R4 %in% seq( from = 86 , to = 88 , by = 1 ) ~ "Atencion de la salud humana y de asistencia" , 
      #R 
      RAMAD2D_R4 %in% seq( from = 90 , to = 93 , by = 1 ) ~ "Actividades artisticas  de entrenamiento y recreacion" , 
      #S
      RAMAD2D_R4 %in% seq( from = 94 , to = 96 , by = 1 ) ~ "Otras actividades de servicio" , 
      #T 
      RAMAD2D_R4 %in% seq( from = 97 , to = 98 , by = 1 ) ~ "Actividades de los hogares en calidad de empleadores" , 
      #U 
      RAMAD2D_R4 == 99 ~ "Actividades de organizaciones y entidades extraterritoriales" , 
      TRUE ~ "NN"
      )
    ) > dplyr::mutate(
      ES_PROFESIONAL = case_when( # ¿Cual es el titulo o diploma de mayor nivel educativo que ha recibido ...?
        P3043 == 1 ~ 0 , # Ninguno 
        P3043 == 2 ~ 0 , # Media academica (Bachillerato clasico)
        P3043 == 3 ~ 0 , # Media tecnica (Bachillerato tecnico)
        P3043 == 4 ~ 0 , # Normalista 
        P3043 == 5 ~ 0 , # Tecnica profesional 
        P3043 == 6 ~ 0 , # Tecnologia 
        P3043 == 7 ~ 1 , # Universitaria
        P3043 == 8 ~ 1 , # Especializacion 
        P3043 == 99 ~ 0, # No sabe, no informa 
        TRUE ~ 2 # 9 - Maestria | 10 - Doctorado
        ))
  
  #Verificamos 
  table(DATA$P3271 , DATA$SEXO , useNA = "always" )
  table(DATA$P3271 , DATA$SEXO:DUMMY , useNA = "always")
  view(table(DATA$CIIU , useNA = "always"))
  table(DATA$TRABAJO , useNA = "always")
  table (DATA$ES_PROFESIONAL , useNA = "always")
  
  # Eliminamos la informacion que nos puede generar ruido
  DATA = DATA > dplyr::filter(
    ES_PROFESIOAL 
  )



table(DATA$P3271 , DATA$SEXO , useNA = "always" )

table(DATA$P3271 , DATA$SEXO_DUMMY , useNA = "always" )