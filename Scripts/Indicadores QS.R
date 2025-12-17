##%######################################################%##
#                                                          #
####                Librerías requeridas                ####
#                                                          #
##%######################################################%##


library(UnalData)
library(UnalR)
library(readxl)
library(tidyverse)
library(scales)

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

### Indicadores 2024 ----

##%######################################################%##
#                                                          #
####         Indicador 1: Tasa de graduación            ####
#                                                          #
##%######################################################%##

# Cohorte 2017-1

MatriPV_20171 <- UnalData::Matriculados %>% 
  filter(TIPO_NIVEL == "Pregrado", MAT_PVEZ == "Sí", 
         YEAR == 2017, SEMESTRE == 1)

# Graduados en pregrado 2022

Gra_2022 <- UnalData::Graduados %>% 
  filter(TIPO_NIVEL == "Pregrado", YEAR %in% c(2022))


# Cruce Cohorte 1

Cohorte1 <- inner_join(MatriPV_20171, Gra_2022, by = "ID")

#
# Cohorte 2018-1
#

MatriPV_20181 <- UnalData::Matriculados %>% 
  filter(TIPO_NIVEL == "Pregrado", MAT_PVEZ == "Sí", 
         YEAR == 2018, SEMESTRE == 1)

# Graduados en pregrado 2023

Gra_2023 <- UnalData::Graduados %>% 
  filter(TIPO_NIVEL == "Pregrado", YEAR %in% c(2023))


# Cruce Cohorte 2

Cohorte2 <- inner_join(MatriPV_20181, Gra_2023, by = "ID")

# Ind1

Ind1 <- (nrow(Cohorte1) + nrow(Cohorte2))/(nrow(MatriPV_20171) + nrow(MatriPV_20181))
Ind1 <- paste0(round(Ind1*100, 2), "%")

# Resultado del indicador 1

print(Ind1)


##%######################################################%##
#                                                          #
####         Indicador 2: tasa de continuación          ####
#                                                          #
##%######################################################%##

Grad_20_24_pre <- UnalData::Graduados %>% 
  filter(TIPO_NIVEL == "Pregrado", YEAR %in% c(2020:2024))

Post_20_24 <- UnalData::Matriculados %>% 
  filter(TIPO_NIVEL == "Postgrado", YEAR >= 2020) %>% 
  select(c(ID, NIVEL)) %>% distinct()

Cruce_gra_pre <- inner_join(Grad_20_24_pre, Post_20_24, by = "ID")

Ind2 <- paste0(round((nrow(Cruce_gra_pre)/nrow(Grad_20_24_pre))*100, 2), "%")

# Resultado del indicador 2

print(Ind2)


##%######################################################%##
#                                                          #
####         Indicador 3: Tasa de retención             ####
#                                                          #
##%######################################################%##

# NOTA: PARA LA VIGENCIA 2025 REVISAR LOS AÑOS 2022 Y 2023.
# EN EL AÑO 2024 SE TUVO QUE HACER UN AJUSTE POR NO TENER MATRICULADOS 
# EN EL 2024-1 DE MANERA OFICIAL

# Matriculados en pregrado 2022-2

Matri_20222 <- UnalData::Matriculados %>% 
  filter(TIPO_NIVEL == "Pregrado", YEAR == 2022, SEMESTRE == 2)

# Graduados en pregrado 2023-1 y 2023-2

Gra_231_232 <- UnalData::Graduados %>% 
  filter(TIPO_NIVEL == "Pregrado", YEAR == 2023 )

# Matriculados en pregrado 20222 menos graduados 2023

Matri_20222_Final <- anti_join(Matri_20222, Gra_231_232, by = "ID")

# Matriculados en pregrado 2023-2

Matri_20232 <- UnalData::Matriculados %>% 
  filter(TIPO_NIVEL == "Pregrado", YEAR == 2023, SEMESTRE == 2)

# Cruzar bases de datos

Cruce_m222_m232 <- inner_join(Matri_20222_Final, Matri_20232, by = "ID")

# Calcular indicador 3

#Ind3 <- paste0(round((nrow(Cruce_m222_m232)/nrow(Matri_20222_Final))*100, 2), "%")

Ind3 <- scales::percent(nrow(Cruce_m222_m232)/nrow(Matri_20222_Final),
                        accuracy = 0.1)

# Resultado del indicador 3

print(Ind3)


### Indicadores 2025 ----

##%######################################################%##
#                                                          #
####         Indicador 1: Tasa de graduación            ####
#                                                          #
##%######################################################%##

# Cohorte 2018-1

MatriPV_20181 <- UnalData::Matriculados %>% 
  filter(TIPO_NIVEL == "Pregrado", MAT_PVEZ == "Sí", 
         YEAR == 2018, SEMESTRE == 1)

# Graduados en pregrado 2023

Gra_2023 <- UnalData::Graduados %>% 
  filter(TIPO_NIVEL == "Pregrado", YEAR %in% c(2023))


# Cruce Cohorte 1

Cohorte1 <- inner_join(MatriPV_20181, Gra_2023, by = "ID")

#
# Cohorte 2019-1
#

MatriPV_20191 <- UnalData::Matriculados %>% 
  filter(TIPO_NIVEL == "Pregrado", MAT_PVEZ == "Sí", 
         YEAR == 2019, SEMESTRE == 1)

# Graduados en pregrado 2024

Gra_2024 <- UnalData::Graduados %>% 
  filter(TIPO_NIVEL == "Pregrado", YEAR %in% c(2024))


# Cruce Cohorte 2

Cohorte2 <- inner_join(MatriPV_20191, Gra_2024, by = "ID")

# Ind1

Ind1 <- (nrow(Cohorte1) + nrow(Cohorte2))/(nrow(MatriPV_20181) + nrow(MatriPV_20191))
Ind1 <- paste0(round(Ind1*100, 2), "%")

# Resultado del indicador 1

print(Ind1)


##%######################################################%##
#                                                          #
####         Indicador 2: tasa de continuación          ####
#                                                          #
##%######################################################%##

Grad_21_25_pre <- UnalData::Graduados %>% 
  filter(TIPO_NIVEL == "Pregrado", YEAR %in% c(2021:2025))

Post_21_25 <- UnalData::Matriculados %>% 
  filter(TIPO_NIVEL == "Postgrado", YEAR >= 2021) %>% 
  select(c(ID, NIVEL)) %>% distinct()

Cruce_gra_pre <- inner_join(Grad_21_25_pre, Post_21_25, by = "ID")

Ind2 <- paste0(round((nrow(Cruce_gra_pre)/nrow(Grad_21_25_pre))*100, 2), "%")

# Resultado del indicador 2

print(Ind2)


###%######################################################%##
#                                                          #
####         Indicador 3: Tasa de retención             ####
#                                                          #
##%######################################################%##

# Matriculados en pregrado 2024-1

Matri_20241 <- UnalData::Matriculados %>% 
  filter(TIPO_NIVEL == "Pregrado", YEAR == 2024, SEMESTRE == 1)

# Graduados en pregrado 2024-2 y 2025-1

Gra_242_251 <- UnalData::Graduados %>% 
  filter(TIPO_NIVEL == "Pregrado" & ((YEAR == 2024 & SEMESTRE == 2)|(YEAR == 2025 & SEMESTRE == 1)))

# Matriculados en pregrado 20241 menos graduados 20242 y 20251

Matri_20241_Final <- anti_join(Matri_20241, Gra_242_251, by = "ID")

# Matriculados en pregrado 2025-1

Matri_20251 <- UnalData::Matriculados %>% 
  filter(TIPO_NIVEL == "Pregrado", YEAR == 2025, SEMESTRE == 1)

# Cruzar bases de datos

Cruce_m241_m251 <- inner_join(Matri_20241_Final, Matri_20251, by = "ID")

# Calcular indicador 3

Ind3 <- paste0(round((nrow(Cruce_m241_m251)/nrow(Matri_20241_Final))*100, 2), "%")

# Resultado del indicador 2

print(Ind3)
