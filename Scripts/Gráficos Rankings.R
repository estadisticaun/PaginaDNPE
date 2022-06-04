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

QSMundoDF <- Rankings %>% filter(RANKING == 'QSMundo') %>% select(-c("RANKING", "SEMESTRE"))

Uyear <- QSMundoDF |> summarise(Max = max(YEAR)) |> as.numeric()
Msj <- paste0("Periodo: 2011-", Uyear)

QSMundo <- Plot.Series(
  datos     = QSMundoDF,
  categoria = "TOTAL", 
  invertir = TRUE,
  col = c("#00BB2D"),
  ylim = c(0, 500),
  libreria = "highcharter",
  titulo = "Evolución posiciones de la UNAL en QS World University Rankings",
  labelX = "Año",
  labelY = "Puesto Mundo",
  estilo    = list(hc.Tema = 1, hc.Slider = FALSE,
                   hc.Credits = Msj))

# QS Latino ----


QSLatinoDF <- Rankings %>% filter(RANKING == 'QSLatino') %>% select(-c("RANKING", "SEMESTRE"))

Uyear <- QSLatinoDF |> summarise(Max = max(YEAR)) |> as.numeric()
Msj <- paste0("Periodo: 2011-", Uyear)

QSLatino <- Plot.Series(
  datos     = QSLatinoDF,
  categoria = "TOTAL", 
  invertir = TRUE,
  col = c("#00BB2D"),
  ylim = c(0, 15),
  libreria = "highcharter",
  titulo = "Evolución posiciones de la UNAL en QS Latin America University Rankings",
  labelX = "Año",
  labelY = "Puesto en Latinoamérica",
  estilo    = list(hc.Tema = 1, hc.Slider = FALSE,
                   hc.Credits = Msj))


# QS Áreas ----


QSAreasDF <- Rankings %>% filter(RANKING == 'QSAreas') %>% select(-c("RANKING", "SEMESTRE"))

Uyear <- QSAreasDF |> summarise(Max = max(YEAR)) |> as.numeric()
Msj <- paste0("Periodo: 2015-", Uyear)

QSAreas <- Plot.Series(datos = QSAreasDF, 
               categoria = "TOTAL", 
               col = c("#00BB2D"),
               libreria = "highcharter",
               titulo = " Evolución Número de Áreas Temáticas clasificadas de la UNAL en QS World University Rankings by Subject",
               labelX = "Año",
               labelY = "Número de Áreas Temáticas clasificadas",
               estilo    = list(hc.Tema = 1, hc.Slider = FALSE,
                                hc.Credits = Msj))



# THE Mundo ----

THEMundoDF <- Rankings %>% filter(RANKING == 'THEMundo') %>% select(-c("RANKING", "SEMESTRE"))

Uyear <- THEMundoDF |> summarise(Max = max(YEAR)) |> as.numeric()
Msj <- paste0("Periodo: 2017-", Uyear)

THEMundo <- Plot.Series(datos = THEMundoDF, 
               categoria = "TOTAL", 
               invertir = TRUE,
               col = c("#00BB2D"),
               ylim = c(0, 1200),
               libreria = "highcharter",
               titulo = "Evolución posiciones de la UNAL en Times Higher Education (THE)
                         World University Rankings",
               labelX = "Año",
               labelY = "Puesto Mundo",
               estilo    = list(hc.Tema = 1, hc.Slider = FALSE,
                                hc.Credits = Msj))

# THE Latino ----

THELatinoDF <- Rankings %>% filter(RANKING == 'THELatino') %>% select(-c("RANKING", "SEMESTRE"))

Uyear <- THELatinoDF |> summarise(Max = max(YEAR)) |> as.numeric()
Msj <- paste0("Periodo: 2017-", Uyear)

THELatino <- Plot.Series(datos = THELatinoDF, 
               categoria = "TOTAL", 
               invertir = TRUE,
               col = c("#00BB2D"),
               ylim = c(0, 50),
               libreria = "highcharter",
               titulo = "Evolución posiciones de la UNAL en Times Higher Education (THE)
                         Latin America University Rankings",
               labelX = "Año",
               labelY = "Puesto en Latinoamérica",
               estilo    = list(hc.Tema = 1, hc.Slider = FALSE,
                                hc.Credits = Msj))

# THE Emergentes ----

THEEmergentesDF <- Rankings %>% filter(RANKING == 'THEEmergentes') %>% select(-c("RANKING", "SEMESTRE"))

Uyear <- THELatinoDF |> summarise(Max = max(YEAR)) |> as.numeric()
Msj <- paste0("Periodo: 2018-", Uyear)


THEEmergentes <- Plot.Series(datos = THEEmergentesDF, 
                   categoria = "TOTAL", 
                   invertir = TRUE,
                   col = c("#00BB2D"),
                   ylim = c(0, 400),
                   libreria = "highcharter",
                   titulo = "Evolución posiciones de la UNAL en Times Higher Education (THE)
                         Emerging Economies Rankings",
                   labelX = "Año",
                   labelY = "Puesto Mundo",
                   estilo    = list(hc.Tema = 1, hc.Slider = FALSE,
                                hc.Credits = Msj))

# ARWUMundo ----


ARWUMundoDF <- Rankings %>% filter(RANKING == 'ARWUMundo') %>% select(-c("RANKING", "SEMESTRE"))

Uyear <- ARWUMundoDF |> summarise(Max = max(YEAR)) |> as.numeric()
Msj <- paste0("Periodo: 2014-", Uyear)

ARWUMundo <- Plot.Series(datos = ARWUMundoDF, 
                                categoria = "TOTAL", 
                                invertir = TRUE,
                                col = c("#00BB2D"),
                                ylim = c(0, 1000),
                                libreria = "highcharter",
                                titulo = "Evolución posiciones de la UNAL en Academic Ranking of World Universities
                                          ARWU - Shanghai",
                                labelX = "Año",
                                labelY = "Puesto Mundo",
                                estilo    = list(hc.Tema = 1, hc.Slider = FALSE,
                                                 hc.Credits = Msj))


# ARWULatino ----

ARWULatinoDF <- Rankings %>% filter(RANKING == 'ARWULatino') %>% select(-c("RANKING", "SEMESTRE"))

Uyear <- ARWULatinoDF |> summarise(Max = max(YEAR)) |> as.numeric()
Msj <- paste0("Periodo: 2013-", Uyear)

ARWULatino <- Plot.Series(datos = ARWULatinoDF, 
                            categoria = "TOTAL",
                            invertir = TRUE,
                            col = c("#00BB2D"),
                            libreria = "highcharter",
                            titulo = "Evolución posiciones de la UNAL en Latin America University Rankings
                                      ARWU - Shanghai",
                            labelX = "Año",
                            labelY = "Puesto en Latinoamérica",
                            estilo    = list(hc.Tema = 1, hc.Slider = FALSE,
                                             hc.Credits = Msj))

# MERCOEmpresas ----

MERCOEmpresasDF <- Rankings %>% filter(RANKING == 'MERCOEmpresas') %>% select(-c("RANKING", "SEMESTRE"))

Uyear <- ARWULatinoDF |> summarise(Max = max(YEAR)) |> as.numeric()
Msj <- paste0("Periodo: 2011-", Uyear)

MERCOEmpresas <- Plot.Series(datos = MERCOEmpresasDF, 
                             categoria = "TOTAL",
                             invertir = TRUE,
                             col = c("#00BB2D"),
                             ylim = c(0, 70),
                             libreria = "highcharter",
                             titulo = "Evolución posiciones de la UNAL en MERCO EMPRESAS",
                             labelX = "Año",
                             labelY = "Puesto en Colombia",
                             estilo    = list(hc.Tema = 1, hc.Slider = FALSE,
                                              hc.Credits = Msj))


# MERCOTalento ----

MERCOTalentoDF <- Rankings %>% filter(RANKING == 'MERCOTalento') %>% select(-c("RANKING", "SEMESTRE"))

Uyear <- MERCOTalentoDF |> summarise(Max = max(YEAR)) |> as.numeric()
Msj <- paste0("Periodo: 2011-", Uyear)

MERCOTalento <- Plot.Series(datos = MERCOTalentoDF, 
                                categoria = "TOTAL", 
                                col = c("#00BB2D"),
                                invertir = TRUE,
                                ylim = c(0, 30),
                                libreria = "highcharter",
                                titulo = "Evolución posiciones de la UNAL en MERCO TALENTO",
                                labelX = "Año",
                                labelY = "Puesto en Colombia",
                                estilo    = list(hc.Tema = 1, hc.Slider = FALSE,
                                                 hc.Credits = Msj))

# MERCORGC ----

MERCORGCDF <- Rankings %>% filter(RANKING == 'MERCORGC') %>% select(-c("RANKING", "SEMESTRE"))

Uyear <- MERCORGCDF |> summarise(Max = max(YEAR)) |> as.numeric()
Msj <- paste0("Periodo: 2013-", Uyear)

MERCORGC <- Plot.Series(datos = MERCORGCDF, 
                               categoria = "TOTAL", 
                               col = c("#00BB2D"),
                               invertir = TRUE,
                               libreria = "highcharter",
                               titulo = "Evolución posiciones de la UNAL en MERCO Responsabilidad y Gobierno Corporativo",
                               labelX = "Año",
                               labelY = "Puesto en Colombia",
                               estilo    = list(hc.Tema = 1, hc.Slider = FALSE,
                                                hc.Credits = Msj))


# MERCOSector ----

MERCOSectorDF <- Rankings %>% filter(RANKING == 'MERCOSector') %>% select(-c("RANKING", "SEMESTRE"))

Uyear <- MERCOSectorDF |> summarise(Max = max(YEAR)) |> as.numeric()
Msj <- paste0("Periodo: 2011-", Uyear)

MERCOSector <- Plot.Series(datos = MERCOSectorDF, 
                           categoria = "TOTAL", 
                           col = c("#00BB2D"),
                           invertir = TRUE,
                           ylim = c(0, 10),
                           libreria = "highcharter",
                           titulo = "Evolución posiciones de la UNAL en MERCO Sector Educación Superior",
                           labelX = "Año",
                           labelY = "Puesto en Colombia",
                           estilo    = list(hc.Tema = 1, hc.Slider = FALSE,
                                            hc.Credits = Msj))

# USapiens ----

USapiensDF <- Rankings %>% filter(RANKING == 'USapiens') %>% select(-c("RANKING"))

Uyear <- USapiensDF |> summarise(Max = max(YEAR)) |> as.numeric()
Msj <- paste0("Periodo: 2011-", Uyear)

USapiens <- Plot.Series(datos = USapiensDF, 
                              categoria = "USAPIENS", 
                              col = c("#8cc63f", "#f15a24", "#93278f"),
                              invertir = TRUE,
                              libreria = "highcharter",
                              titulo = "Evolución posiciones de las sedes de la UNAL en el Ranking U-Sapiens",
                              labelX = "Semestre",
                              labelY = "Puesto en Colombia",
                              estilo    = list(LegendTitle = "Posiciones sedes UNAL:", hc.Tema = 1, hc.Slider = FALSE,
                                               hc.Credits = Msj))

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
