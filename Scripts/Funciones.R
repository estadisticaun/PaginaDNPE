# ____________________________________________________________________________________ #
#                            1). CONSTRUIR SERIES DE TIEMPO                            #
# ____________________________________________________________________________________ #

PocentRelativo <- function(x) {
  TotPercent <- function(m){ m*100/sum(m, na.rm = TRUE) }
  RowPorcent <- round( t(apply(x, MARGIN = 1, FUN = TotPercent)), 0 )
  return(as.data.frame(RowPorcent))
}

Plot.SeriesRev <- function(datos, categoria, colores, titulo = "", labelX = "Periodo", labelY = "",
                        libreria = c("highcharter", "plotly", "dygraphs"), estilo = NULL) {
  
  # COMANDOS DE VERIFICACIÓN Y VALIDACIÓN
  if(missingArg(datos) || missingArg(categoria)) {
    stop("¡Por favor introduzca un conjunto de datos y una categoría dentro de la columna 'Variable'!", call. = FALSE)
  }
  categoria <- toupper(categoria)
  if (!(categoria %in% datos$Variable)) {
    stop("¡Por favor introduzca una categoría que se encuentra dentro de la columna 'Variable'!", call. = FALSE)
  }
  if (!(is.character(titulo) && is.character(labelX) && is.character(labelY))) {
    stop("¡El argumento 'titulo', 'labelX' y 'labelY' deben ser una cadena de texto!", call. = FALSE)
  }
  if (missingArg(libreria)) {
    warning("¡Se usará la librería 'highcharter' por defecto para realizar el plot!", call. = FALSE)
    libreria <- "highcharter"
  } else {
    libreria  <- tolower(libreria)
    '%NotIN%' <- Negate('%in%')
    if (libreria %NotIN% c("highcharter", "plotly", "dygraphs")) {
      stop("¡Por favor introduzca el nombre de una librería valida (paquete usado para realizar la gráfica)!", call. = FALSE)
    }
  }
  LegendTitle <- ifelse(is.null(estilo$LegendTitle), "", estilo$LegendTitle)
  
  # CREACIÓN DEL DATAFRAME CON EL CUAL SE CREARÁ LA GRÁFICA
  DataFrame <- datos %>%
    filter(Variable == categoria) %>%
    mutate(Fecha = paste(YEAR, SEMESTRE, sep = "-")) %>%
    select(-Variable, -YEAR, -SEMESTRE) %>%
    relocate(Fecha)
  
  TablaHorizontal <- DataFrame %>% pivot_wider(names_from = Clase, values_from = Total)
  categorias <- DataFrame %>% select(Clase) %>% distinct() %>% pull()
  
  if (length(categorias)==1L) {
    Relativo <- DataFrame %>%
      mutate(Relativo = Total/Total*100) %>%
      select(-Total)
  } else {
    Relativo <- TablaHorizontal %>% select(-Fecha) %>%
      PocentRelativo() %>% as_tibble() %>%
      mutate(Fecha = TablaHorizontal$Fecha) %>%
      pivot_longer(cols = categorias, names_to = "Clase", values_to = "Relativo")
  }
  TablaFinal <- DataFrame %>% inner_join(Relativo)
  
  if (!(missingArg(colores) || length(colores)==length(categorias))) {
    stop(paste0("¡El número de colores ingresados en el vector 'colores' no corresponde con el número de categorías a colorear!",
                "\n\tNo. colores ingresados = ", length(colores), " != ", "No. de categorías = ", length(categorias)), call. = FALSE)
  }
  if (missingArg(colores)) { colores <- rainbow(length(categorias), alpha = 0.7) }
  
  # CREACIÓN DEL PLOT RETORNAR
  if(libreria == "highcharter") {
    
    if(!(missingArg(estilo) || is.null(estilo$hc.Tema))) {
      ThemeHC <- switch(estilo$hc.Tema,
                        "1" = hc_theme_538(),
                        "2" = hc_theme_alone(),
                        "3" = hc_theme_economist(),
                        "4" = hc_theme_ffx(),
                        "5" = hc_theme_flat(),
                        "6" = hc_theme_ggplot2(),
                        "7" = hc_theme_google(),
                        "8" = hc_theme_monokai(),
                        "9" = hc_theme_darkunica(),
                        "10" = hc_theme_gridlight()
      )
    } else { ThemeHC <- hc_theme_flat() }
    BoxInfo <- ifelse(!(missingArg(estilo) || is.null(estilo$hc.BoxInfo)), estilo$hc.BoxInfo, TRUE)
    
    PlotSeries <- TablaFinal %>%
      hchart(type = "line", hcaes(x = Fecha, y = Total, group = Clase), color = colores,
             zoomType = list(enabled = FALSE),  resetZoomButton = TRUE) %>%
      hc_chart(type = "datetime", zoomType = "x") %>%
      hc_plotOptions(line = list(marker = list(enabled = FALSE, symbol = "square", radius = 1))) %>%
      
      hc_title(text = titulo, style = list(fontWeight = "bold",
                                           fontSize   = "22px",
                                           color      = "#333333",
                                           useHTML    = TRUE)
      ) %>%
      hc_xAxis(title = list(text   = labelX,
                            offset = 70,
                            style  = list(fontWeight = "bold",
                                          fontSize   = "18px",
                                          color      = "black")
      ),
      align = "center", lineColor = "#787878", opposite  = FALSE,
      labels = list(style = list(fontWeight = "bold", color = "black", fontSize = "18px"))
      ) %>%
      hc_yAxis(title = list(text   = labelY,
                            offset = 70,
                            style  = list(fontWeight = "bold",
                                          fontSize   = "18px",
                                          color      = "black")
      ),
      lineColor = "#787878", opposite  = FALSE, lineWidth = 1, min = 1, reversed = TRUE,
      labels    = list(style = list(fontWeight = "bold", color = "black", fontSize = "18px"))
      ) %>%
      # https://github.com/jbkunst/highcharter/issues/331
      hc_exporting(enabled = TRUE, filename = paste0("PlotSeries_", categoria)) %>%
      hc_legend(enabled = TRUE, align = "center", verticalAlign = "bottom", layout = "horizontal",
                title = list(text = LegendTitle, style = list(textDecoration = "underline")),
                x = 42, y = 0, itemStyle = list(fontWeight = "bold",
                                                color      = "black",
                                                fontSize   = "18px")) %>%
      hc_tooltip(crosshairs = TRUE, shared = BoxInfo,
                 pointFormat = '<span style="color:{series.color}">\u25CF </span><b>{series.name}: {point.y}</b> ({point.Relativo}%)<br/>',
                 backgroundColor = hex_to_rgba("#BAAEAE", 0.7),
                 borderColor = "#6D6666", borderWidth = 5, useHTML = TRUE) %>%
      hc_add_theme(ThemeHC)
    
    if (!missingArg(estilo) && estilo$hc.Slider==TRUE) {
      PlotSeries <- PlotSeries %>%
        hc_navigator(height = 15, margin = 5, maskFill = "rgba(255,16,46,0.6)",
                     enabled = TRUE, series = list(color     = "#999999",
                                                   lineWidth = 30,
                                                   type      = "areaspline",
                                                   fillColor = "#999999")
        ) %>%
        hc_rangeSelector(enabled = TRUE, inputEnabled = FALSE, labelStyle = list(display = "none"),
                         buttonPosition = list(align = "left"), floating = FALSE,
                         buttons = list(list(type = "all", text = "Restaurar")))
    }
    if (!(missingArg(estilo) || is.null(estilo$hc.Credits))) {
      PlotSeries <- PlotSeries %>%
        hc_subtitle(text = estilo$hc.Credits, align = "left", style = list(color = "#2B908F", fontWeight = "bold"))
    }
    
  } else if (libreria == "plotly") {
    
    if (!(missingArg(estilo) || is.null(estilo$ply.LegendPosition))) {
      ParmsLegend <- estilo$ply.LegendPosition
    } else {
      ParmsLegend <- list(x = 1, y = 0.5, orientation = "v")
    }
    if (!(missingArg(estilo) || is.null(estilo$ply.Credits))) {
      ParmsCredits <- estilo$ply.Credits
    } else {
      ParmsCredits <- list(x = 0.2, y = 1, text = "")
    }
    Hovermode <- ifelse(!(missingArg(estilo) || is.null(estilo$ply.Interaction)), estilo$ply.Interaction, "x unified")
    
    FreqRelativa <- Relativo %>% pivot_wider(names_from = Clase, values_from = Relativo)
    PlotSeries <- plot_ly(data = TablaHorizontal)
    for (i in 1:length(categorias)) {
      df_Temp    <- data.frame(X = TablaHorizontal$Fecha, Y = TablaHorizontal[[categorias[i]]], Text = FreqRelativa[[categorias[i]]])
      PlotSeries <- add_trace(PlotSeries, x = ~X, y = ~Y, data = df_Temp, text = ~ Text,
                              name = categorias[i], type = "scatter", mode = "markers+lines",
                              line = list(color = colores[i], width = 3),
                              marker =  list(color = colores[i], size = 6, line = list(width = 1.2, color = "#787878")),
                              hovertemplate = paste('%{y}', '(%{text:.2s}%)'),
                              textposition = "outside")
    }
    # Arial | Open Sans | Courier New, monospace
    FamilyAxis  <- list(family = "Old Standard TT, serif", size = 16, color = "#525252")
    FamilyTitle <- list(family = "Open Sans", size = 24, color = "#333333")
    
    Title <- list(text = paste0("<b>", titulo, "</b>"), font = FamilyTitle, y = 0.96)
    Xaxis <- list(title = labelX,
                  zeroline = FALSE,
                  showline = TRUE,
                  showgrid = FALSE,
                  showticklabels = TRUE,
                  linecolor = "#787878",
                  linewidth = 2.5,
                  autotick  = FALSE,
                  ticks     = "outside",
                  tickwidth = 2.5,
                  ticklen   = 10,
                  tickcolor = "#CCCCCC",
                  tickangle = -45,
                  tickfont  = FamilyAxis)
    Yaxis <- list(title = labelY,
                  zeroline = TRUE,
                  showline = TRUE,
                  showgrid = TRUE,
                  showticklabels = TRUE,
                  linecolor = "#787878",
                  linewidth = 3,
                  separatethousands = TRUE,
                  tickfont  = FamilyAxis)
    
    PlotSeries <- PlotSeries %>%
      layout(title = Title, xaxis = Xaxis, yaxis = Yaxis,
             autosize = TRUE, showlegend = TRUE,
             legend = append(ParmsLegend, list(traceorder = "normal", title = list(text = paste0("<b>", LegendTitle, "</b>")))),
             hovermode = Hovermode,
             annotations = append(ParmsCredits, list(showarrow = FALSE, xref = "paper", yref = "paper",
                                                     xanchor = "right", yanchor = "auto", xshift = 0, yshift = 0,
                                                     font = list(size = 12, color = "#CCCCCC")))
      ) %>% config(locale = "es")
    
  } else if (libreria == "dygraphs") {
    
    LegendWidth <- ifelse(!(missingArg(estilo) || is.null(estilo$dyg.LegendWidth)), estilo$dyg.LegendWidth, 250)
    
    Periodos <- TablaHorizontal %>% select(Fecha) %>% distinct() %>% pull()
    Periodos <- gsub("-2", "-7", Periodos)
    TableHorizontal <- TablaHorizontal
    TableHorizontal$Fecha <- as.Date(as.yearmon(Periodos, "%Y-%m"))
    TableHorizontal <- xts(x = TableHorizontal[,-1], order.by = TableHorizontal$Fecha)
    
    getSemestre <- 'function(d) {
    var monthNames = ["I", "", "", "", "", "","II", "", "", "", "", ""];
    date = new Date(d);
    if (date.getMonth() == 0 || date.getMonth() == 6) {
    return date.getFullYear() + "-" + monthNames[date.getMonth()];
    } else {
    return "";
    }
  }'
    dyUnzoom <- function(dygraph) {
      dyPlugin(
        dygraph = dygraph,
        name = "Unzoom",
        path = system.file("plugins/unzoom.js", package = "dygraphs")
      )
}
    
    PlotSeries <- dygraph(TableHorizontal, main = paste0("<span style='color:", "#333333", ";'>", titulo, "</span>")) %>%
      dyOptions(drawPoints = TRUE, pointSize = 2,
                strokeWidth = 2, colors = colores, includeZero = TRUE,
                axisTickSize = 3, axisLineColor = "#787878",
                axisLabelColor = "#525252", axisLabelFontSize = 16,
                drawGrid = TRUE, gridLineColor = "lightblue") %>%
      dyLegend(show = "always", width = LegendWidth, hideOnMouseOut = TRUE) %>%
      dyAxis("x", label = labelX, axisLabelFormatter = JS(getSemestre), axisLineWidth = 4) %>%
      dyAxis("y", label = labelY, axisLineWidth = 4) %>%
      dyRangeSelector(height = 30, strokeColor = "") %>%
      dyUnzoom()
    
    if (!(missingArg(estilo) || is.null(estilo$dyg.Resaltar))) {
      if (estilo$dyg.Resaltar) {
        PlotSeries <- PlotSeries %>%
          dyHighlight(highlightCircleSize = 5,
                      highlightSeriesBackgroundAlpha = 0.5,
                      highlightSeriesOpts = list(strokeWidth = 2.5),
                      hideOnMouseOut = TRUE)
      }
    }
    }
  
  return(PlotSeries)
  }


# ____________________________________________________________________________________ #
#                            2). CONSTRUIR SERIES DE TIEMPO                            #
# ____________________________________________________________________________________ #

PocentRelativo <- function(x) {
  TotPercent <- function(m){ m*100/sum(m, na.rm = TRUE) }
  RowPorcent <- round( t(apply(x, MARGIN = 1, FUN = TotPercent)), 0 )
  return(as.data.frame(RowPorcent))
}

Plot.Series <- function(datos, categoria, colores, titulo = "", labelX = "Periodo", labelY = "",
                           libreria = c("highcharter", "plotly", "dygraphs"), estilo = NULL) {
  
  # COMANDOS DE VERIFICACIÓN Y VALIDACIÓN
  if(missingArg(datos) || missingArg(categoria)) {
    stop("¡Por favor introduzca un conjunto de datos y una categoría dentro de la columna 'Variable'!", call. = FALSE)
  }
  categoria <- toupper(categoria)
  if (!(categoria %in% datos$Variable)) {
    stop("¡Por favor introduzca una categoría que se encuentra dentro de la columna 'Variable'!", call. = FALSE)
  }
  if (!(is.character(titulo) && is.character(labelX) && is.character(labelY))) {
    stop("¡El argumento 'titulo', 'labelX' y 'labelY' deben ser una cadena de texto!", call. = FALSE)
  }
  if (missingArg(libreria)) {
    warning("¡Se usará la librería 'highcharter' por defecto para realizar el plot!", call. = FALSE)
    libreria <- "highcharter"
  } else {
    libreria  <- tolower(libreria)
    '%NotIN%' <- Negate('%in%')
    if (libreria %NotIN% c("highcharter", "plotly", "dygraphs")) {
      stop("¡Por favor introduzca el nombre de una librería valida (paquete usado para realizar la gráfica)!", call. = FALSE)
    }
  }
  LegendTitle <- ifelse(is.null(estilo$LegendTitle), "", estilo$LegendTitle)
  
  # CREACIÓN DEL DATAFRAME CON EL CUAL SE CREARÁ LA GRÁFICA
  DataFrame <- datos %>%
    filter(Variable == categoria) %>%
    mutate(Fecha = paste(YEAR, SEMESTRE, sep = "-")) %>%
    select(-Variable, -YEAR, -SEMESTRE) %>%
    relocate(Fecha)
  
  TablaHorizontal <- DataFrame %>% pivot_wider(names_from = Clase, values_from = Total)
  categorias <- DataFrame %>% select(Clase) %>% distinct() %>% pull()
  
  if (length(categorias)==1L) {
    Relativo <- DataFrame %>%
      mutate(Relativo = Total/Total*100) %>%
      select(-Total)
  } else {
    Relativo <- TablaHorizontal %>% select(-Fecha) %>%
      PocentRelativo() %>% as_tibble() %>%
      mutate(Fecha = TablaHorizontal$Fecha) %>%
      pivot_longer(cols = categorias, names_to = "Clase", values_to = "Relativo")
  }
  TablaFinal <- DataFrame %>% inner_join(Relativo)
  
  if (!(missingArg(colores) || length(colores)==length(categorias))) {
    stop(paste0("¡El número de colores ingresados en el vector 'colores' no corresponde con el número de categorías a colorear!",
                "\n\tNo. colores ingresados = ", length(colores), " != ", "No. de categorías = ", length(categorias)), call. = FALSE)
  }
  if (missingArg(colores)) { colores <- rainbow(length(categorias), alpha = 0.7) }
  
  # CREACIÓN DEL PLOT RETORNAR
  if(libreria == "highcharter") {
    
    if(!(missingArg(estilo) || is.null(estilo$hc.Tema))) {
      ThemeHC <- switch(estilo$hc.Tema,
                        "1" = hc_theme_538(),
                        "2" = hc_theme_alone(),
                        "3" = hc_theme_economist(),
                        "4" = hc_theme_ffx(),
                        "5" = hc_theme_flat(),
                        "6" = hc_theme_ggplot2(),
                        "7" = hc_theme_google(),
                        "8" = hc_theme_monokai(),
                        "9" = hc_theme_darkunica(),
                        "10" = hc_theme_gridlight()
      )
    } else { ThemeHC <- hc_theme_flat() }
    BoxInfo <- ifelse(!(missingArg(estilo) || is.null(estilo$hc.BoxInfo)), estilo$hc.BoxInfo, TRUE)
    
    PlotSeries <- TablaFinal %>%
      hchart(type = "line", hcaes(x = Fecha, y = Total, group = Clase), color = colores,
             zoomType = list(enabled = FALSE),  resetZoomButton = TRUE) %>%
      hc_chart(type = "datetime", zoomType = "x") %>%
      hc_plotOptions(line = list(marker = list(enabled = FALSE, symbol = "square", radius = 1))) %>%
      
      hc_title(text = titulo, style = list(fontWeight = "bold",
                                           fontSize   = "22px",
                                           color      = "#333333",
                                           useHTML    = TRUE)
      ) %>%
      hc_xAxis(title = list(text   = labelX,
                            offset = 70,
                            style  = list(fontWeight = "bold",
                                          fontSize   = "18px",
                                          color      = "black")
      ),
      align = "center", lineColor = "#787878", opposite  = FALSE,
      labels = list(style = list(fontWeight = "bold", color = "black", fontSize = "18px"))
      ) %>%
      hc_yAxis(title = list(text   = labelY,
                            offset = 70,
                            style  = list(fontWeight = "bold",
                                          fontSize   = "18px",
                                          color      = "black")
      ),
      lineColor = "#787878", opposite  = FALSE, lineWidth = 1, min = 1, reversed = FALSE,
      labels    = list(style = list(fontWeight = "bold", color = "black", fontSize = "18px"))
      ) %>%
      # https://github.com/jbkunst/highcharter/issues/331
      hc_exporting(enabled = TRUE, filename = paste0("PlotSeries_", categoria)) %>%
      hc_legend(enabled = TRUE, align = "center", verticalAlign = "bottom", layout = "horizontal",
                title = list(text = LegendTitle, style = list(textDecoration = "underline")),
                x = 42, y = 0, itemStyle = list(fontWeight = "bold",
                                                color      = "black",
                                                fontSize   = "18px")) %>%
      hc_tooltip(crosshairs = TRUE, shared = BoxInfo,
                 pointFormat = '<span style="color:{series.color}">\u25CF </span><b>{series.name}: {point.y}</b> ({point.Relativo}%)<br/>',
                 backgroundColor = hex_to_rgba("#BAAEAE", 0.7),
                 borderColor = "#6D6666", borderWidth = 5, useHTML = TRUE) %>%
      hc_add_theme(ThemeHC)
    
    if (!missingArg(estilo) && estilo$hc.Slider==TRUE) {
      PlotSeries <- PlotSeries %>%
        hc_navigator(height = 15, margin = 5, maskFill = "rgba(255,16,46,0.6)",
                     enabled = TRUE, series = list(color     = "#999999",
                                                   lineWidth = 30,
                                                   type      = "areaspline",
                                                   fillColor = "#999999")
        ) %>%
        hc_rangeSelector(enabled = TRUE, inputEnabled = FALSE, labelStyle = list(display = "none"),
                         buttonPosition = list(align = "left"), floating = FALSE,
                         buttons = list(list(type = "all", text = "Restaurar")))
    }
    if (!(missingArg(estilo) || is.null(estilo$hc.Credits))) {
      PlotSeries <- PlotSeries %>%
        hc_subtitle(text = estilo$hc.Credits, align = "left", style = list(color = "#2B908F", fontWeight = "bold"))
    }
    
  } else if (libreria == "plotly") {
    
    if (!(missingArg(estilo) || is.null(estilo$ply.LegendPosition))) {
      ParmsLegend <- estilo$ply.LegendPosition
    } else {
      ParmsLegend <- list(x = 1, y = 0.5, orientation = "v")
    }
    if (!(missingArg(estilo) || is.null(estilo$ply.Credits))) {
      ParmsCredits <- estilo$ply.Credits
    } else {
      ParmsCredits <- list(x = 0.2, y = 1, text = "")
    }
    Hovermode <- ifelse(!(missingArg(estilo) || is.null(estilo$ply.Interaction)), estilo$ply.Interaction, "x unified")
    
    FreqRelativa <- Relativo %>% pivot_wider(names_from = Clase, values_from = Relativo)
    PlotSeries <- plot_ly(data = TablaHorizontal)
    for (i in 1:length(categorias)) {
      df_Temp    <- data.frame(X = TablaHorizontal$Fecha, Y = TablaHorizontal[[categorias[i]]], Text = FreqRelativa[[categorias[i]]])
      PlotSeries <- add_trace(PlotSeries, x = ~X, y = ~Y, data = df_Temp, text = ~ Text,
                              name = categorias[i], type = "scatter", mode = "markers+lines",
                              line = list(color = colores[i], width = 3),
                              marker =  list(color = colores[i], size = 6, line = list(width = 1.2, color = "#787878")),
                              hovertemplate = paste('%{y}', '(%{text:.2s}%)'),
                              textposition = "outside")
    }
    # Arial | Open Sans | Courier New, monospace
    FamilyAxis  <- list(family = "Old Standard TT, serif", size = 16, color = "#525252")
    FamilyTitle <- list(family = "Open Sans", size = 24, color = "#333333")
    
    Title <- list(text = paste0("<b>", titulo, "</b>"), font = FamilyTitle, y = 0.96)
    Xaxis <- list(title = labelX,
                  zeroline = FALSE,
                  showline = TRUE,
                  showgrid = FALSE,
                  showticklabels = TRUE,
                  linecolor = "#787878",
                  linewidth = 2.5,
                  autotick  = FALSE,
                  ticks     = "outside",
                  tickwidth = 2.5,
                  ticklen   = 10,
                  tickcolor = "#CCCCCC",
                  tickangle = -45,
                  tickfont  = FamilyAxis)
    Yaxis <- list(title = labelY,
                  zeroline = TRUE,
                  showline = TRUE,
                  showgrid = TRUE,
                  showticklabels = TRUE,
                  linecolor = "#787878",
                  linewidth = 3,
                  separatethousands = TRUE,
                  tickfont  = FamilyAxis)
    
    PlotSeries <- PlotSeries %>%
      layout(title = Title, xaxis = Xaxis, yaxis = Yaxis,
             autosize = TRUE, showlegend = TRUE,
             legend = append(ParmsLegend, list(traceorder = "normal", title = list(text = paste0("<b>", LegendTitle, "</b>")))),
             hovermode = Hovermode,
             annotations = append(ParmsCredits, list(showarrow = FALSE, xref = "paper", yref = "paper",
                                                     xanchor = "right", yanchor = "auto", xshift = 0, yshift = 0,
                                                     font = list(size = 12, color = "#CCCCCC")))
      ) %>% config(locale = "es")
    
  } else if (libreria == "dygraphs") {
    
    LegendWidth <- ifelse(!(missingArg(estilo) || is.null(estilo$dyg.LegendWidth)), estilo$dyg.LegendWidth, 250)
    
    Periodos <- TablaHorizontal %>% select(Fecha) %>% distinct() %>% pull()
    Periodos <- gsub("-2", "-7", Periodos)
    TableHorizontal <- TablaHorizontal
    TableHorizontal$Fecha <- as.Date(as.yearmon(Periodos, "%Y-%m"))
    TableHorizontal <- xts(x = TableHorizontal[,-1], order.by = TableHorizontal$Fecha)
    
    getSemestre <- 'function(d) {
    var monthNames = ["I", "", "", "", "", "","II", "", "", "", "", ""];
    date = new Date(d);
    if (date.getMonth() == 0 || date.getMonth() == 6) {
    return date.getFullYear() + "-" + monthNames[date.getMonth()];
    } else {
    return "";
    }
  }'
    dyUnzoom <- function(dygraph) {
      dyPlugin(
        dygraph = dygraph,
        name = "Unzoom",
        path = system.file("plugins/unzoom.js", package = "dygraphs")
      )
}
    
    PlotSeries <- dygraph(TableHorizontal, main = paste0("<span style='color:", "#333333", ";'>", titulo, "</span>")) %>%
      dyOptions(drawPoints = TRUE, pointSize = 2,
                strokeWidth = 2, colors = colores, includeZero = TRUE,
                axisTickSize = 3, axisLineColor = "#787878",
                axisLabelColor = "#525252", axisLabelFontSize = 16,
                drawGrid = TRUE, gridLineColor = "lightblue") %>%
      dyLegend(show = "always", width = LegendWidth, hideOnMouseOut = TRUE) %>%
      dyAxis("x", label = labelX, axisLabelFormatter = JS(getSemestre), axisLineWidth = 4) %>%
      dyAxis("y", label = labelY, axisLineWidth = 4) %>%
      dyRangeSelector(height = 30, strokeColor = "") %>%
      dyUnzoom()
    
    if (!(missingArg(estilo) || is.null(estilo$dyg.Resaltar))) {
      if (estilo$dyg.Resaltar) {
        PlotSeries <- PlotSeries %>%
          dyHighlight(highlightCircleSize = 5,
                      highlightSeriesBackgroundAlpha = 0.5,
                      highlightSeriesOpts = list(strokeWidth = 2.5),
                      hideOnMouseOut = TRUE)
      }
    }
    }
  
  return(PlotSeries)
  }
