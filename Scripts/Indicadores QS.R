##%######################################################%##
#                                                          #
####                Librerías requeridas                ####
#                                                          #
##%######################################################%##


library(UnalData)
library(UnalR)
library(readxl)
library(tidyverse)

### Indicadores 2022 ----

##%######################################################%##
#                                                          #
####         Indicador 1: Tasa de graduación            ####
#                                                          #
##%######################################################%##

# Cohorte 2015-1

MatriPV_20151 <- UnalData::Matriculados %>% 
  filter(TIPO_NIVEL == "Pregrado", MAT_PVEZ == "Sí", 
         YEAR == 2015, SEMESTRE == 1)

# Graduados en pregrado 2020

Gra_2020 <- UnalData::Graduados %>% 
  filter(TIPO_NIVEL == "Pregrado", YEAR %in% c(2020))


# Cruce Cohorte 1

Cohorte1 <- inner_join(MatriPV_20151, Gra_2020, by = "ID")

#
# Cohorte 2016-1
#

MatriPV_20161 <- UnalData::Matriculados %>% 
  filter(TIPO_NIVEL == "Pregrado", MAT_PVEZ == "Sí", 
         YEAR == 2016, SEMESTRE == 1)

# Graduados en pregrado 2021

Gra_2021 <- UnalData::Graduados %>% 
  filter(TIPO_NIVEL == "Pregrado", YEAR %in% c(2021))


# Cruce Cohorte 2

Cohorte2 <- inner_join(MatriPV_20161, Gra_2021, by = "ID")

# Ind1

Ind1 <- (nrow(Cohorte1) + nrow(Cohorte2))/(nrow(MatriPV_20151) + nrow(MatriPV_20161))
Ind1 <- paste0(round(Ind1*100, 2), "%")

# Resultado del indicador 1

print(Ind1)


##%######################################################%##
#                                                          #
####         Indicador 2: tasa de continuación          ####
#                                                          #
##%######################################################%##

Grad_18_21_pre <- UnalData::Graduados %>% 
  filter(TIPO_NIVEL == "Pregrado", YEAR %in% c(2018:2022))

Post_18_21 <- UnalData::Matriculados %>% 
              filter(TIPO_NIVEL == "Postgrado", YEAR >= 2018) %>% 
              select(c(ID, NIVEL)) %>% distinct()

Cruce_gra_pre <- inner_join(Grad_18_21_pre, Post_18_21, by = "ID")

Ind2 <- paste0(round((nrow(Cruce_gra_pre)/nrow(Grad_18_21_pre))*100, 2), "%")

# Resultado del indicador 2

print(Ind2)


##%######################################################%##
#                                                          #
####         Indicador 3: Tasa de retención             ####
#                                                          #
##%######################################################%##

# Matriculados en pregrado 2021-1

Matri_20211 <- UnalData::Matriculados %>% 
  filter(TIPO_NIVEL == "Pregrado", YEAR == 2021, SEMESTRE == 1)

# Graduados en pregrado 2021-2 y 2022-1

Gra_212_221 <- UnalData::Graduados %>% 
  filter(TIPO_NIVEL == "Pregrado" & ((YEAR == 2021 & SEMESTRE == 2)|(YEAR == 2022 & SEMESTRE == 1)))

# Matricuolados en pregrado 20211 menos graduados 20212 y 20221

Matri_20211_Final <- anti_join(Matri_20211, Gra_212_221, by = "ID")

# Matriculados en pregrado 2022-1

Matri_20221 <- UnalData::Matriculados %>% 
  filter(TIPO_NIVEL == "Pregrado", YEAR == 2022, SEMESTRE == 1)

# Cruzar bases de datos

Cruce_m211_m221 <- inner_join(Matri_20211_Final, Matri_20221, by = "ID")

# Calcular indicador 3

Ind3 <- paste0(round((nrow(Cruce_m211_m221)/nrow(Matri_20211_Final))*100, 2), "%")

# Resultado del indicador 2

print(Ind3)


##%######################################################%##
#                                                          #
####         Código Documentos Repetidos                ####
#                                                          #
##%######################################################%##

# Importar datos bases duplicados

Matriculados_2009_2015 <- read_excel("Datos/Matriculados_2009-2015.xlsx")
Matriculados_2016_2022 <- read_excel("Datos/Matriculados_2016-2022.xlsx")

# Unir bases con duplicados

Duplicados <- bind_rows(Matriculados_2009_2015, Matriculados_2016_2022)

# Eliminar filas con duplicados y ordenar por documentos

Duplicados <-  Duplicados %>% select(-c(PERIODO_MAT)) %>%
  distinct() %>% 
  arrange(DOCUMENTO, DOCUMENTO_ANTERIOR)


# Crear múltiples columnas por documento

Duplicados$id_conteo <- 0
Duplicados$id_conteo[1] <-  2
for(i in 1:(nrow(Duplicados)-1)){
  if(Duplicados$DOCUMENTO[i] == Duplicados$DOCUMENTO[i+1]){
    Duplicados$id_conteo[i+1] = Duplicados$id_conteo[i]+1
  }else{Duplicados$id_conteo[i+1] = 2}
}

Duplicados$id_conteo <- paste0("id", Duplicados$id_conteo)
Duplicados2 <- Duplicados

Duplicados2 <- Duplicados2 %>% pivot_wider(DOCUMENTO, names_from = id_conteo,
                                           values_from = DOCUMENTO_ANTERIOR)

### Indicadores 2023 ----

##%######################################################%##
#                                                          #
####         Indicador 1: Tasa de graduación            ####
#                                                          #
##%######################################################%##

# Cohorte 2016-1

MatriPV_20161 <- UnalData::Matriculados %>% 
  filter(TIPO_NIVEL == "Pregrado", MAT_PVEZ == "Sí", 
         YEAR == 2016, SEMESTRE == 1)

# Graduados en pregrado 2021

Gra_2021 <- UnalData::Graduados %>% 
  filter(TIPO_NIVEL == "Pregrado", YEAR %in% c(2021))


# Cruce Cohorte 1

Cohorte1 <- inner_join(MatriPV_20161, Gra_2021, by = "ID")

#
# Cohorte 2017-1
#

MatriPV_20171 <- UnalData::Matriculados %>% 
  filter(TIPO_NIVEL == "Pregrado", MAT_PVEZ == "Sí", 
         YEAR == 2017, SEMESTRE == 1)

# Graduados en pregrado 2022

Gra_2022 <- UnalData::Graduados %>% 
  filter(TIPO_NIVEL == "Pregrado", YEAR %in% c(2022))


# Cruce Cohorte 2

Cohorte2 <- inner_join(MatriPV_20171, Gra_2022, by = "ID")

# Ind1

Ind1 <- (nrow(Cohorte1) + nrow(Cohorte2))/(nrow(MatriPV_20161) + nrow(MatriPV_20171))
Ind1 <- paste0(round(Ind1*100, 2), "%")

# Resultado del indicador 1

print(Ind1)


##%######################################################%##
#                                                          #
####         Indicador 2: tasa de continuación          ####
#                                                          #
##%######################################################%##

Grad_19_23_pre <- UnalData::Graduados %>% 
  filter(TIPO_NIVEL == "Pregrado", YEAR %in% c(2019:2023))

Post_19_23 <- UnalData::Matriculados %>% 
  filter(TIPO_NIVEL == "Postgrado", YEAR >= 2019) %>% 
  select(c(ID, NIVEL)) %>% distinct()

Cruce_gra_pre <- inner_join(Grad_19_23_pre, Post_19_23, by = "ID")

Ind2 <- paste0(round((nrow(Cruce_gra_pre)/nrow(Grad_19_23_pre))*100, 2), "%")

# Resultado del indicador 2

print(Ind2)


##%######################################################%##
#                                                          #
####         Indicador 3: Tasa de retención             ####
#                                                          #
##%######################################################%##

# Matriculados en pregrado 2022-1

Matri_20221 <- UnalData::Matriculados %>% 
  filter(TIPO_NIVEL == "Pregrado", YEAR == 2022, SEMESTRE == 1)

# Graduados en pregrado 2022-2 y 2023-1

Gra_222_231 <- UnalData::Graduados %>% 
  filter(TIPO_NIVEL == "Pregrado" & ((YEAR == 2022 & SEMESTRE == 2)|(YEAR == 2023 & SEMESTRE == 1)))

# Matriculados en pregrado 20221 menos graduados 20212 y 20221

Matri_20221_Final <- anti_join(Matri_20221, Gra_222_231, by = "ID")

# Matriculados en pregrado 2023-1

Matri_20231 <- UnalData::Matriculados %>% 
  filter(TIPO_NIVEL == "Pregrado", YEAR == 2023, SEMESTRE == 1)

# Cruzar bases de datos

Cruce_m221_m231 <- inner_join(Matri_20221_Final, Matri_20231, by = "ID")

# Calcular indicador 3

Ind3 <- paste0(round((nrow(Cruce_m221_m231)/nrow(Matri_20221_Final))*100, 2), "%")

# Resultado del indicador 2

print(Ind3)



