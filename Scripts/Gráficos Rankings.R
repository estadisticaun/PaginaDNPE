# Librerías requeridas ----
library(dplyr)
library(readxl)

# Función salvar widges ----

Salvar <- function(objeto, ruta, nombre){
  saveWidget(objeto,
             file = file.path(str_sub(getwd(), 1, str_length(getwd())),
                              ruta,
                              nombre),
             selfcontained = F, libdir = "libraryjs")
  
}

# Importar datos ----

Rankings <- read_excel("Datos/Rankings.xlsx")

# VISUALIZACIONES

# QS Mundo ----

QSMundoDF <- Rankings %>% filter(RANKING == 'QSMundo') %>% select(-c("RANKING"))

QSMundo <- Plot.SeriesRev(datos = QSMundoDF, 
             categoria = "TOTAL", 
             col = c("#00BB2D"),
             libreria = "highcharter",
             titulo = "Evolución posiciones de la UNAL en QS World University Rankings",
             labelY = "Puesto Mundo",
             estilo    = list(hc.Tema = 4, hc.Slider = FALSE,
                             hc.Credits = "Periodo: 2011-2020"))

# QS Latino ----


QSLatinoDF <- Rankings %>% filter(RANKING == 'QSLatino') %>% select(-c("RANKING"))

QSLatino <- Plot.SeriesRev(datos = QSLatinoDF, 
               categoria = "TOTAL", 
               col = c("#00BB2D"),
               libreria = "highcharter",
               titulo = "Evolución posiciones de la UNAL en QS Latin America University Rankings",
               labelY = "Puesto en Latinoamérica",
               estilo    = list(hc.Tema = 4, hc.Slider = FALSE,
                                hc.Credits = "Periodo: 2011-2020"))


# QS Áreas ----


QSAreasDF <- Rankings %>% filter(RANKING == 'QSAreas') %>% select(-c("RANKING"))

QSAreas <- Plot.Series(datos = QSAreasDF, 
               categoria = "TOTAL", 
               col = c("#00BB2D"),
               libreria = "highcharter",
               titulo = "Evolución posiciones de la UNAL en QS World University Rankings by Subject",
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
               labelY = "Puesto Mundo",
               estilo    = list(hc.Tema = 4, hc.Slider = FALSE,
                                hc.Credits = "Periodo: 2017-2020"))

# THE Latino ----

THELatinoDF <- Rankings %>% filter(RANKING == 'THELatino') %>% select(-c("RANKING"))

THELatino <- Plot.SeriesRev(datos = THELatinoDF, 
               categoria = "TOTAL", 
               col = c("#00BB2D"),
               libreria = "highcharter",
               titulo = "Evolución posiciones de la UNAL en Times Higher Education (THE)
                         Latin America University Rankings",
               labelY = "Puesto en Latinoamérica",
               estilo    = list(hc.Tema = 4, hc.Slider = FALSE,
                                hc.Credits = "Periodo: 2017-2020"))

# THE Emergentes ----

THEEmergentesDF <- Rankings %>% filter(RANKING == 'THEEmergentes') %>% select(-c("RANKING"))

THEEmergentes <- Plot.SeriesRev(datos = THEEmergentesDF, 
                   categoria = "TOTAL", 
                   col = c("#00BB2D"),
                   libreria = "highcharter",
                   titulo = "Evolución posiciones de la UNAL en Times Higher Education (THE)
                         Emerging Economies Rankings",
                   labelY = "Puesto Mundo",
                   estilo    = list(hc.Tema = 4, hc.Slider = FALSE,
                                hc.Credits = "Periodo: 2018-2021"))

# Crear HTML ----

Salvar(QSMundo, "Rankings", "QSMundo.html")
Salvar(QSLatino, "Rankings", "QSLatino.html")
Salvar(QSAreas, "Rankings", "QSAreas.html")
Salvar(THEMundo, "Rankings", "THEMundo.html")
Salvar(THELatino, "Rankings", "THELatino.html")
Salvar(THEEmergentes, "Rankings", "THEEmergentes.html")
