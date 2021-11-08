# Librerías requeridas ----

library(dplyr)
library(readxl)
library(tidyr)
library(stringr)
library(highcharter)
library(htmlwidgets)
library(UnalR)
library(googledrive)

# Función salvar widges ----

Salvar <- function(objeto, ruta, nombre){
  saveWidget(objeto,
             file = file.path(str_sub(getwd(), 1, str_length(getwd())),
                              ruta,
                              nombre),
             selfcontained = F, libdir = "libraryjs")
  
}

# Importar datos ----

drive_download("Rankings/Rankings.xlsx", overwrite = TRUE)
Rankings <- read_excel("Rankings.xlsx")
unlink(c("Rankings.xlsx"))

# VISUALIZACIONES

# QS Mundo ----

QSMundoDF <- Rankings %>% filter(RANKING == 'QSMundo') %>% select(-c("RANKING"))

QSMundo <- Plot.SeriesRev(datos = QSMundoDF, 
             categoria = "TOTAL", 
             col = c("#00BB2D"),
             libreria = "highcharter",
             titulo = "Evolución posiciones de la UNAL en QS World University Rankings",
             labelX = "Año",
             labelY = "Puesto Mundo",
             estilo    = list(hc.Tema = 4, hc.Slider = FALSE,
                             hc.Credits = "Periodo: 2011-2021"))

# QS Latino ----


QSLatinoDF <- Rankings %>% filter(RANKING == 'QSLatino') %>% select(-c("RANKING"))

QSLatino <- Plot.SeriesRev(datos = QSLatinoDF, 
               categoria = "TOTAL", 
               col = c("#00BB2D"),
               libreria = "highcharter",
               titulo = "Evolución posiciones de la UNAL en QS Latin America University Rankings",
               labelX = "Año",
               labelY = "Puesto en Latinoamérica",
               estilo    = list(hc.Tema = 4, hc.Slider = FALSE,
                                hc.Credits = "Periodo: 2011-2021"))


# QS Áreas ----


QSAreasDF <- Rankings %>% filter(RANKING == 'QSAreas') %>% select(-c("RANKING"))

QSAreas <- Plot.Series(datos = QSAreasDF, 
               categoria = "TOTAL", 
               col = c("#00BB2D"),
               libreria = "highcharter",
               titulo = " Evolución Número de Áreas Temáticas clasificadas de la UNAL en QS World University Rankings by Subject",
               labelX = "Año",
               labelY = "Número de Áreas Temáticas clasificadas",
               estilo    = list(hc.Tema = 4, hc.Slider = FALSE,
                                hc.Credits = "Periodo: 2015-2021"))



# THE Mundo ----

THEMundoDF <- Rankings %>% filter(RANKING == 'THEMundo') %>% select(-c("RANKING"))

THEMundo <- Plot.SeriesRev(datos = THEMundoDF, 
               categoria = "TOTAL", 
               col = c("#00BB2D"),
               libreria = "highcharter",
               titulo = "Evolución posiciones de la UNAL en Times Higher Education (THE)
                         World University Rankings",
               labelX = "Año",
               labelY = "Puesto Mundo",
               estilo    = list(hc.Tema = 4, hc.Slider = FALSE,
                                hc.Credits = "Periodo: 2017-2021"))

# THE Latino ----

THELatinoDF <- Rankings %>% filter(RANKING == 'THELatino') %>% select(-c("RANKING"))

THELatino <- Plot.SeriesRev(datos = THELatinoDF, 
               categoria = "TOTAL", 
               col = c("#00BB2D"),
               libreria = "highcharter",
               titulo = "Evolución posiciones de la UNAL en Times Higher Education (THE)
                         Latin America University Rankings",
               labelX = "Año",
               labelY = "Puesto en Latinoamérica",
               estilo    = list(hc.Tema = 4, hc.Slider = FALSE,
                                hc.Credits = "Periodo: 2017-2021"))

# THE Emergentes ----

THEEmergentesDF <- Rankings %>% filter(RANKING == 'THEEmergentes') %>% select(-c("RANKING"))

THEEmergentes <- Plot.SeriesRev(datos = THEEmergentesDF, 
                   categoria = "TOTAL", 
                   col = c("#00BB2D"),
                   libreria = "highcharter",
                   titulo = "Evolución posiciones de la UNAL en Times Higher Education (THE)
                         Emerging Economies Rankings",
                   labelX = "Año",
                   labelY = "Puesto Mundo",
                   estilo    = list(hc.Tema = 4, hc.Slider = FALSE,
                                hc.Credits = "Periodo: 2018-2022"))

# ARWUMundo ----


ARWUMundoDF <- Rankings %>% filter(RANKING == 'ARWUMundo') %>% select(-c("RANKING"))

ARWUMundo <- Plot.SeriesRev(datos = ARWUMundoDF, 
                                categoria = "TOTAL", 
                                col = c("#00BB2D"),
                                libreria = "highcharter",
                                titulo = "Evolución posiciones de la UNAL en Academic Ranking of World Universities
                                          ARWU - Shanghai",
                                labelX = "Año",
                                labelY = "Puesto Mundo",
                                estilo    = list(hc.Tema = 4, hc.Slider = FALSE,
                                                 hc.Credits = "Periodo: 2014-2021"))


# ARWULatino ----

ARWULatinoDF <- Rankings %>% filter(RANKING == 'ARWULatino') %>% select(-c("RANKING"))

ARWULatino <- Plot.SeriesRev(datos = ARWULatinoDF, 
                            categoria = "TOTAL", 
                            col = c("#00BB2D"),
                            libreria = "highcharter",
                            titulo = "Evolución posiciones de la UNAL en Latin America University Rankings
                                      ARWU - Shanghai",
                            labelX = "Año",
                            labelY = "Puesto en Latinoamérica",
                            estilo    = list(hc.Tema = 4, hc.Slider = FALSE,
                                             hc.Credits = "Periodo: 2013-2021"))

# MERCOEmpresas ----

MERCOEmpresasDF <- Rankings %>% filter(RANKING == 'MERCOEmpresas') %>% select(-c("RANKING"))

MERCOEmpresas <- Plot.SeriesRev(datos = MERCOEmpresasDF, 
                             categoria = "TOTAL", 
                             col = c("#00BB2D"),
                             libreria = "highcharter",
                             titulo = "Evolución posiciones de la UNAL en MERCO EMPRESAS",
                             labelX = "Año",
                             labelY = "Puesto en Colombia",
                             estilo    = list(hc.Tema = 4, hc.Slider = FALSE,
                                              hc.Credits = "Periodo: 2011-2021"))


# MERCOTalento ----

MERCOTalentoDF <- Rankings %>% filter(RANKING == 'MERCOTalento') %>% select(-c("RANKING"))

MERCOTalento <- Plot.SeriesRev(datos = MERCOTalentoDF, 
                                categoria = "TOTAL", 
                                col = c("#00BB2D"),
                                libreria = "highcharter",
                                titulo = "Evolución posiciones de la UNAL en MERCO TALENTO",
                                labelX = "Año",
                                labelY = "Puesto en Colombia",
                                estilo    = list(hc.Tema = 4, hc.Slider = FALSE,
                                                 hc.Credits = "Periodo: 2011-2021"))

# MERCORGC ----

MERCORGCDF <- Rankings %>% filter(RANKING == 'MERCORGC') %>% select(-c("RANKING"))

MERCORGC <- Plot.SeriesRev(datos = MERCORGCDF, 
                               categoria = "TOTAL", 
                               col = c("#00BB2D"),
                               libreria = "highcharter",
                               titulo = "Evolución posiciones de la UNAL en MERCO Responsabilidad y Gobierno Corporativo",
                               labelX = "Año",
                               labelY = "Puesto en Colombia",
                               estilo    = list(hc.Tema = 4, hc.Slider = FALSE,
                                                hc.Credits = "Periodo: 2013-2021"))


# MERCOSector ----

MERCOSectorDF <- Rankings %>% filter(RANKING == 'MERCOSector') %>% select(-c("RANKING"))

MERCOSector <- Plot.SeriesRev(datos = MERCOSectorDF, 
                           categoria = "TOTAL", 
                           col = c("#00BB2D"),
                           libreria = "highcharter",
                           titulo = "Evolución posiciones de la UNAL en MERCO Sector Educación Superior",
                           labelX = "Año",
                           labelY = "Puesto en Colombia",
                           estilo    = list(hc.Tema = 4, hc.Slider = FALSE,
                                            hc.Credits = "Periodo: 2011-2021"))

# USapiens ----

USapiensDF <- Rankings %>% filter(RANKING == 'USapiens') %>% select(-c("RANKING"))

USapiens <- Plot.SeriesRev(datos = USapiensDF, 
                              categoria = "USAPIENS", 
                              col = c("#8cc63f", "#f15a24", "#93278f"),
                              libreria = "highcharter",
                              titulo = "Evolución posiciones de las sedes de la UNAL en el Ranking U-Sapiens",
                              labelX = "Semestre",
                              labelY = "Puesto en Colombia",
                              estilo    = list(LegendTitle = "Posiciones sedes UNAL:", hc.Tema = 4, hc.Slider = FALSE,
                                               hc.Credits = "Periodo: 2011-2021"))

# Crear HTML ----

Salvar(QSMundo, "Rankings", "QSMundo.html")
Salvar(QSLatino, "Rankings", "QSLatino.html")
Salvar(QSAreas, "Rankings", "QSAreas.html")
Salvar(THEMundo, "Rankings", "THEMundo.html")
Salvar(THELatino, "Rankings", "THELatino.html")
Salvar(THEEmergentes, "Rankings", "THEEmergentes.html")
Salvar(ARWUMundo, "Rankings", "ARWUMundo.html")
Salvar(ARWULatino, "Rankings", "ARWULatino.html")
Salvar(MERCOEmpresas, "Rankings", "MERCOEmpresas.html")
Salvar(MERCOTalento, "Rankings", "MERCOTalento.html")
Salvar(MERCORGC, "Rankings", "MERCORGC.html")
Salvar(MERCOSector, "Rankings", "MERCOSector.html")
Salvar(USapiens, "Rankings", "USapiens.html")
