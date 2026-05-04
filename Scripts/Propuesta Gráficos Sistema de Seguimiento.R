
##%######################################################%##
#                                                          #
####                     Librerías                      ####
#                                                          #
##%######################################################%##

library(tidyverse)
library(readxl)
library(plotly)
library(gt)

# Importar Datos

QUIPU_2026 <- read_excel("Datos/Consulta Avance Financiero.xlsx") 
QUIPU_2026V2 <- read_excel("Datos/Consulta Avance Financiero Feb10.xlsx") 
BPUN_2026 <- read_excel("Datos/Fichas QUIPU PGD2527.xlsx") 


# Avance General ------------

# Gráfico Avance Feb 10-2026

Consolidado <- QUIPU_2026V2 %>%  
  inner_join(BPUN_2026, by = c("PROYECTO" = "quipu")) %>% 
  filter(VIGENCIA_MOV %in% c(2025, 2026)) %>% 
  select(c("Vigencia" = "VIGENCIA_MOV", 
           "Apropiacion" =  "VR_APROP_VIGENCIA", 
           "Disponibilidad" =  "VR_DISPONIBILIDAD", 
           "Compromisos" = "VR_COMPROMISOS", 
           "Pagos" ="VR_PAGOS_VIG_ACT")) %>% 
  summarise(Apropiacion = sum(Apropiacion), 
            Disponibilidad = sum(Disponibilidad),
            Compromisos = sum(Compromisos),
            Pagos = sum(Pagos),
            .by = c(Vigencia)) %>% 
  summarise(across(c(Apropiacion:Pagos), ~ sum(.x)), .by = c(Vigencia)) %>%
  mutate(P_Disponibilidad = Disponibilidad / Apropiacion,
         P_Compromisos = Compromisos / Apropiacion,
         P_Pagos = Pagos / Compromisos)
Consolidado

# Gráfico ----

QUIPU_2026V2 %>%  
  inner_join(BPUN_2026, by = c("PROYECTO" = "quipu")) %>% 
  filter(VIGENCIA_MOV %in% c(2025)) %>% 
  select(c("Vigencia" = "VIGENCIA_MOV", 
           "Apropiacion" =  "VR_APROP_VIGENCIA", 
           "Disponibilidad" =  "VR_DISPONIBILIDAD", 
           "Compromisos" = "VR_COMPROMISOS", 
           "Pagos" ="VR_PAGOS_VIG_ACT")) %>% 
  summarise(Apropiacion = sum(Apropiacion), 
            Disponibilidad = sum(Disponibilidad),
            Compromisos = sum(Compromisos),
            Pagos = sum(Pagos),
            .by = c(Vigencia)) %>% 
  summarise(across(c(Apropiacion:Pagos), ~ sum(.x)), .by = c(Vigencia)) %>%
  mutate(P_Disponibilidad = Disponibilidad / Apropiacion,
         P_Compromisos = Compromisos / Apropiacion,
         P_Pagos = Pagos / Compromisos) %>% 
  mutate(Vigencia = paste("Vigencia", Vigencia)) %>% 
  pivot_longer(cols = starts_with("P_"),
               names_to = "Proceso",
               values_to = "Valor") %>% 
  mutate(Proceso = fct_rev(fct_inorder(Proceso)),
         Vigencia = fct_inorder(as.character(Vigencia)),
         Barra = 1) %>% 
  ggplot() +
  geom_rect(xmin = -0.009, xmax = 0.35, ymin = 0.6, ymax = 3.4, alpha = 0.04, fill = "red")+
  geom_rect(xmin = 0.35, xmax = 0.70, ymin = 0.6, ymax = 3.4, alpha = 0.05, fill = "yellow")+
  geom_rect(xmin = 0.70, xmax = 1.01,ymin = 0.6, ymax = 3.4, alpha = 0.03, fill = "green")+
  geom_bar(aes(x = Barra, y = Proceso), stat = "identity", fill = "gray80", col= "white", width = 0.6)+
  geom_bar(aes(x = Valor, y = Proceso), stat = "identity", fill = "#41b6c4", width = 0.50)+
  geom_text(aes(x = Valor, y = Proceso, label = scales::percent(Valor, accuracy = 0.01)), hjust = -0.3, size = 3.5, fontface = "bold")+
  labs(x = "\nPorcentaje de ejecución financiera", y = "Estado de los recursos financieros\n")+
  scale_x_continuous(limits = c(0, 1), labels = scales::percent, n.breaks = 10)+
  scale_y_discrete(labels = c("Con pagos", "Con compromisos", "Con Certificado de\nDisponibilidad (CDP)"))+
  facet_wrap(vars(Vigencia), ncol = 1)+
  theme(
    axis.text.x = element_text(size = 9),
    axis.text.y = element_text(size = 9),
    plot.title = element_text(face = "italic", size = 13),
    panel.grid = element_blank(), panel.spacing.x = unit(1, "lines"),
    strip.text = element_text(size = 12, face = "bold", color = "white"),
    strip.background = element_rect(fill = "#9b89b3"))

  theme(
        aspect.ratio = 1/2.5, 
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 11),
        panel.background = element_rect(fill = "gray100", color = "darkblue"),
        panel.grid = element_blank(), panel.spacing.y = unit(2, "lines"),
        strip.text = element_text(size = 14, face = "bold", color = "white"),
        strip.background = element_rect(fill = "#9b89b3"))


# Tabla -----

QUIPU_2026V2 %>%  
  inner_join(BPUN_2026, by = c("PROYECTO" = "quipu")) %>% 
  filter(VIGENCIA_MOV %in% c(2025, 2026)) %>% 
  select(c("Vigencia" = "VIGENCIA_MOV", 
           "Apropiacion" =  "VR_APROP_VIGENCIA", 
           "Disponibilidad" =  "VR_DISPONIBILIDAD", 
           "Compromisos" = "VR_COMPROMISOS", 
           "Pagos" ="VR_PAGOS_VIG_ACT")) %>% 
  summarise(Apropiacion = sum(Apropiacion), 
            Disponibilidad = sum(Disponibilidad),
            Compromisos = sum(Compromisos),
            Pagos = sum(Pagos),
            .by = c(Vigencia)) %>% 
  summarise(across(c(Apropiacion:Pagos), ~ sum(.x)), .by = c(Vigencia)) %>%
  mutate(P_Disponibilidad = Disponibilidad / Apropiacion,
         P_Compromisos = Compromisos / Apropiacion,
         P_Pagos = Pagos / Compromisos) %>% 
  relocate(Vigencia, Apropiacion, Disponibilidad, P_Disponibilidad, Compromisos, P_Compromisos, Pagos, P_Pagos) %>% 
  gt(locale = "es_CO") %>% 
  tab_spanner(
    label = "Ejecución del Presupuesto - PGD 2025-2027", # El título superior
    columns = everything(), # Columnas que abarca
    id = "spanner_presupuesto"
    ) %>% 
  # cols_align(
  #   align = "center") %>%
  fmt_currency(
    columns = c(Apropiacion, Disponibilidad, Compromisos, Pagos),
    decimals = 0) %>% 
  fmt_percent(
    columns = c(starts_with("P_")),
    decimals = 2) %>% 
  cols_label(
    Apropiacion = "Recurso apropiado",
    Disponibilidad = "Con CDP",
    P_Disponibilidad = "Con CDP (%)",
    Compromisos = "Con compromisos",
    P_Compromisos = "Con compromisos (%)",
    Pagos = "Comprometido con pago",
    P_Pagos = "Comprometido con pago (%)") %>% 
  tab_style(
    style = list(
      cell_fill(color = "#F2F2F2"), # Gris oscuro
      cell_text(color = "black") # Texto blanco para contraste
    ),
    locations = cells_column_labels()) %>% 
  tab_style(
    style = cell_text(weight = "bold", align = "center"), # Define el estilo negrilla
    locations = cells_body(columns = Vigencia)) %>% 
   tab_footnote(
    footnote = "CDP: Certificado de Disponibilidad Presupuestal",
    locations = cells_column_labels(columns = c(Disponibilidad, P_Disponibilidad))) %>% 
  tab_source_note(
    source_note = "Fuente: Dirección Nacional de Planeación y Estadística - Sistema Financiero QUIPU "
  ) %>% 
  tab_source_note(
    source_note = paste("Corte de información:", Sys.Date()-1)) %>% 
  tab_style(
    style = list(
      cell_fill(color = "#D9EAD3"), # Color verde claro (puedes usar nombres o Hex)
      cell_text(weight = "bold", color = "darkgreen") # Texto en negrita y color
    ),
    locations = cells_column_spanners(spanners = "spanner_presupuesto")
  ) %>% 
  tab_options(
    # Cambia el sistema de números por símbolos estándar (el primero es *)
    footnotes.marks = "standard",
    source_notes.font.size = px(12)
  )


# Avance por Sedes -----

# Gráfico ----

QUIPU_2026V2 %>%  
  inner_join(BPUN_2026, by = c("PROYECTO" = "quipu")) %>% 
  filter(VIGENCIA_MOV %in% c(2026)) %>% 
  select(c("Vigencia" = "VIGENCIA_MOV", 
           "Sede" = "sede",
           "Apropiacion" =  "VR_APROP_VIGENCIA", 
           "Disponibilidad" =  "VR_DISPONIBILIDAD", 
           "Compromisos" = "VR_COMPROMISOS", 
           "Pagos" ="VR_PAGOS_VIG_ACT")) %>% 
  summarise(Apropiacion = sum(Apropiacion), 
            Disponibilidad = sum(Disponibilidad),
            Compromisos = sum(Compromisos),
            Pagos = sum(Pagos),
            .by = c(Vigencia, Sede)) %>% 
  summarise(across(c(Apropiacion:Pagos), ~ sum(.x)), .by = c(Vigencia, Sede)) %>%
  mutate(P_Disponibilidad = Disponibilidad / Apropiacion,
         P_Compromisos = Compromisos / Apropiacion,
         P_Pagos = Pagos / Compromisos) %>% 
  mutate(Vigencia = paste("Vigencia", Vigencia)) %>% 
  arrange(desc(P_Compromisos)) %>% 
  pivot_longer(cols = starts_with("P_"),
               names_to = "Proceso",
               values_to = "Valor") %>% 
  mutate(Proceso = fct_inorder(Proceso), 
         Proceso = fct_recode(Proceso, 
                              "Con CDP *" = "P_Disponibilidad", 
                              "Con compromiso" = "P_Compromisos", 
                              "Con pago" = "P_Pagos"),
         Sede =  fct_reorder(Sede, Valor),
          Barra = 1) %>% 
  ggplot() +
  geom_rect(xmin = -0.009, xmax = 0.35, ymin = 0.6, ymax = 10.4, alpha = 0.008, fill = "red")+
  geom_rect(xmin = 0.35, xmax = 0.70, ymin = 0.6, ymax = 10.4, alpha = 0.008, fill = "yellow")+
  geom_rect(xmin = 0.70, xmax = 1.01,ymin = 0.6, ymax = 10.4, alpha = 0.008, fill = "green")+
  geom_bar(aes(x = Barra, y = Sede), stat = "identity", fill = "gray80", col= "white", width = 0.6)+
  geom_bar(aes(x = Valor, y = Sede), stat = "identity", fill = "#41b6c4", width = 0.50)+
  geom_text(aes(x = Valor, y = Sede, label = scales::percent(Valor, accuracy = 0.1), hjust = ifelse(Valor < 0.8, -0.1, 1.1)), 
                size = 3, 
                fontface = "bold")+
  facet_wrap(vars(Proceso), ncol = 3)+
  scale_x_continuous(limits = c(0, 1), labels = scales::percent, n.breaks = 5)+
  labs(title = "\nEvolución porcentual (%) ejecución presupuestal por sedes - vigencia 2026",
       subtitle = paste("Corte de información:", Sys.Date()-1, "\n"),
       x = "\nPorcentaje de ejecución financiera", 
       y = "Sede\n",
       caption = "\n* CDP: Certificado de Disponibilidad Presupuestal\nFuente: Dirección Nacional de Planeación y Estadística - Sistema Financiero QUIPU\n")+
    theme(
    axis.text.x = element_text(size = 9),
    axis.text.y = element_text(size = 9),
    plot.title = element_text(face = "italic", size = 13),
    panel.grid = element_blank(), panel.spacing.x = unit(1, "lines"),
    strip.text = element_text(size = 12, face = "bold", color = "white"),
    strip.background = element_rect(fill = "#9b89b3"))
  

# Tabla Vigencia 2026-----

QUIPU_2026V2 %>%  
  inner_join(BPUN_2026, by = c("PROYECTO" = "quipu")) %>% 
  filter(VIGENCIA_MOV %in% c(2026)) %>% 
  select(c("Vigencia" = "VIGENCIA_MOV", 
           "Sede" = "sede",
           "Apropiacion" =  "VR_APROP_VIGENCIA", 
           "Disponibilidad" =  "VR_DISPONIBILIDAD", 
           "Compromisos" = "VR_COMPROMISOS", 
           "Pagos" ="VR_PAGOS_VIG_ACT")) %>% 
  summarise(Apropiacion = sum(Apropiacion), 
            Disponibilidad = sum(Disponibilidad),
            Compromisos = sum(Compromisos),
            Pagos = sum(Pagos),
            .by = c(Vigencia, Sede)) %>% 
  summarise(across(c(Apropiacion:Pagos), ~ sum(.x)), .by = c(Vigencia, Sede)) %>%
  mutate(P_Disponibilidad = Disponibilidad / Apropiacion,
         P_Compromisos = Compromisos / Apropiacion,
         P_Pagos = Pagos / Compromisos) %>% 
  relocate(Sede, Vigencia, Apropiacion, Disponibilidad, P_Disponibilidad, Compromisos, P_Compromisos, Pagos, P_Pagos) %>% 
  arrange(desc(P_Compromisos)) %>% 
  select(-c(Vigencia)) %>% 
  bind_rows(summarise(., across(c(Apropiacion, Disponibilidad, Compromisos, Pagos), \(x) sum(x, na.rm = TRUE)), Sede = "Total")) %>% 
  mutate(P_Disponibilidad = ifelse(is.na(P_Disponibilidad), Disponibilidad / Apropiacion, P_Disponibilidad)) %>% 
  mutate(P_Compromisos = ifelse(is.na(P_Compromisos), Compromisos / Apropiacion, P_Compromisos)) %>%
  mutate(P_Pagos = ifelse(is.na(P_Pagos), Pagos / Compromisos, P_Pagos)) %>%
  gt(locale = "es_CO") %>% 
  tab_spanner(
    label = "Ejecución presupuesto PGD 2025-2027 por sedes - Vigencia 2026", # El título superior
    columns = everything(), # Columnas que abarca
    id = "spanner_presupuesto"
  ) %>% 
  fmt_currency(
    columns = c(Apropiacion, Disponibilidad, Compromisos, Pagos),
    decimals = 0) %>% 
  fmt_percent(
    columns = c(starts_with("P_")),
    decimals = 2) %>% 
  cols_align(
    align = "center",
    columns = -c(Sede)
  ) %>% 
  cols_label(
    Apropiacion = "Recurso apropiado",
    Disponibilidad = "Con CDP",
    P_Disponibilidad = "Con CDP (%)",
    Compromisos = "Con compromisos",
    P_Compromisos = "Con compromisos (%)",
    Pagos = "Con pago",
    P_Pagos = "Con pago (%)") %>% 
   tab_style(
    style = list(
      cell_fill(color = "#F0F7FF"), # Gris oscuro
      cell_text(color = "black", align = "center") # Texto blanco para contraste
    ),
    locations = cells_column_labels()) %>% 
  tab_style(
    style = list(cell_text(weight = "bold")),
    locations = cells_body(columns = c(Sede))) %>% 
  tab_footnote(
    footnote = "CDP: Certificado de Disponibilidad Presupuestal",
    locations = cells_column_labels(columns = c(Disponibilidad, P_Disponibilidad))) %>% 
  tab_source_note(
    source_note = "Fuente: Dirección Nacional de Planeación y Estadística - Sistema Financiero QUIPU "
  ) %>% 
  tab_source_note(
    source_note = paste("Corte de información:", Sys.Date()-1)) %>% 
  tab_style(
    style = list(
      cell_fill(color = "#9b89b3"), # Color verde claro (puedes usar nombres o Hex)
      cell_text(weight = "bold", color = "white") # Texto en negrita y color
    ),
    locations = cells_column_spanners(spanners = "spanner_presupuesto")
  ) %>% 
  tab_options(
    # Cambia el sistema de números por símbolos estándar (el primero es *)
    footnotes.marks = "standard",
    source_notes.font.size = px(12),
    row.striping.include_table_body = TRUE
  ) 


# Avance por Ejes -----

# Gráfico ----

QUIPU_2026V2 %>%  
  inner_join(BPUN_2026, by = c("PROYECTO" = "quipu")) %>% 
  filter(VIGENCIA_MOV %in% c(2026)) %>% 
  select(c("Vigencia" = "VIGENCIA_MOV", 
           "Eje" = "eje",
           "Apropiacion" =  "VR_APROP_VIGENCIA", 
           "Disponibilidad" =  "VR_DISPONIBILIDAD", 
           "Compromisos" = "VR_COMPROMISOS", 
           "Pagos" ="VR_PAGOS_VIG_ACT")) %>% 
  summarise(Apropiacion = sum(Apropiacion), 
            Disponibilidad = sum(Disponibilidad),
            Compromisos = sum(Compromisos),
            Pagos = sum(Pagos),
            .by = c(Vigencia, Eje)) %>% 
  summarise(across(c(Apropiacion:Pagos), ~ sum(.x)), .by = c(Vigencia, Eje)) %>%
  mutate(P_Disponibilidad = Disponibilidad / Apropiacion,
         P_Compromisos = Compromisos / Apropiacion,
         P_Pagos = Pagos / Compromisos) %>% 
  mutate(Vigencia = paste("Vigencia", Vigencia)) %>% 
  arrange(desc(P_Compromisos)) %>% 
  pivot_longer(cols = starts_with("P_"),
               names_to = "Proceso",
               values_to = "Valor") %>% 
  mutate(Proceso = fct_inorder(Proceso), 
         Proceso = fct_recode(Proceso, 
                              "Con CDP *" = "P_Disponibilidad", 
                              "Con compromiso" = "P_Compromisos", 
                              "Con pago" = "P_Pagos"),
         Eje =  fct_reorder(Eje, Valor),
         Barra = 1) %>% 
  ggplot() +
  geom_rect(xmin = -0.009, xmax = 0.35, ymin = 0.6, ymax = 3.4, alpha = 0.008, fill = "red")+
  geom_rect(xmin = 0.35, xmax = 0.70, ymin = 0.6, ymax = 3.4, alpha = 0.008, fill = "yellow")+
  geom_rect(xmin = 0.70, xmax = 1.01,ymin = 0.6, ymax = 3.4, alpha = 0.008, fill = "green")+
  geom_bar(aes(x = Barra, y = Eje), stat = "identity", fill = "gray80", col= "white", width = 0.6)+
  geom_bar(aes(x = Valor, y = Eje), stat = "identity", fill = "#41b6c4", width = 0.50)+
  geom_text(aes(x = Valor, y = Eje, label = scales::percent(Valor, accuracy = 0.1), hjust = ifelse(Valor < 0.8, -0.1, 1.1)), 
            size = 3, 
            fontface = "bold")+
  facet_wrap(vars(Proceso), ncol = 3)+
  scale_x_continuous(limits = c(0, 1), labels = scales::percent, n.breaks = 5)+
  labs(title = "\nEvolución porcentual (%) ejecución presupuestal por Ejes - vigencia 2026",
       subtitle = paste("Corte de información:", Sys.Date()-1, "\n"),
       x = "\nPorcentaje de ejecución financiera", 
       y = "Eje\n",
       caption = "\n* CDP: Certificado de Disponibilidad Presupuestal\nFuente: Dirección Nacional de Planeación y Estadística - Sistema Financiero QUIPU\n")+
  theme(
    axis.text.x = element_text(size = 9),
    axis.text.y = element_text(size = 9),
    plot.title = element_text(face = "italic", size = 13),
    panel.grid = element_blank(), panel.spacing.x = unit(1, "lines"),
    strip.text = element_text(size = 12, face = "bold", color = "white"),
    strip.background = element_rect(fill = "#9b89b3"))


# Tabla Vigencia 2026-----

QUIPU_2026V2 %>%  
  inner_join(BPUN_2026, by = c("PROYECTO" = "quipu")) %>% 
  filter(VIGENCIA_MOV %in% c(2026)) %>% 
  select(c("Vigencia" = "VIGENCIA_MOV", 
           "Eje" = "eje",
           "Apropiacion" =  "VR_APROP_VIGENCIA", 
           "Disponibilidad" =  "VR_DISPONIBILIDAD", 
           "Compromisos" = "VR_COMPROMISOS", 
           "Pagos" ="VR_PAGOS_VIG_ACT")) %>% 
  summarise(Apropiacion = sum(Apropiacion), 
            Disponibilidad = sum(Disponibilidad),
            Compromisos = sum(Compromisos),
            Pagos = sum(Pagos),
            .by = c(Vigencia, Eje)) %>% 
  summarise(across(c(Apropiacion:Pagos), ~ sum(.x)), .by = c(Vigencia, Eje)) %>%
  mutate(P_Disponibilidad = Disponibilidad / Apropiacion,
         P_Compromisos = Compromisos / Apropiacion,
         P_Pagos = Pagos / Compromisos) %>% 
  relocate(Eje, Vigencia, Apropiacion, Disponibilidad, P_Disponibilidad, Compromisos, P_Compromisos, Pagos, P_Pagos) %>% 
  arrange(desc(P_Compromisos)) %>% 
  select(-c(Vigencia)) %>% 
  bind_rows(summarise(., across(c(Apropiacion, Disponibilidad, Compromisos, Pagos), \(x) sum(x, na.rm = TRUE)), Eje = "Total")) %>% 
  mutate(P_Disponibilidad = ifelse(is.na(P_Disponibilidad), Disponibilidad / Apropiacion, P_Disponibilidad)) %>% 
  mutate(P_Compromisos = ifelse(is.na(P_Compromisos), Compromisos / Apropiacion, P_Compromisos)) %>%
  mutate(P_Pagos = ifelse(is.na(P_Pagos), Pagos / Compromisos, P_Pagos)) %>%
  gt(locale = "es_CO") %>% 
  tab_spanner(
    label = "Ejecución presupuesto PGD 2025-2027 por Ejes - Vigencia 2026", # El título superior
    columns = everything(), # Columnas que abarca
    id = "spanner_presupuesto"
  ) %>% 
  fmt_currency(
    columns = c(Apropiacion, Disponibilidad, Compromisos, Pagos),
    decimals = 0) %>% 
  fmt_percent(
    columns = c(starts_with("P_")),
    decimals = 2) %>% 
  cols_label(
    Apropiacion = "Recurso apropiado",
    Disponibilidad = "Con CDP",
    P_Disponibilidad = "Con CDP (%)",
    Compromisos = "Con compromisos",
    P_Compromisos = "Con compromisos (%)",
    Pagos = "Con pago",
    P_Pagos = "Con pago (%)") %>% 
  tab_style(
    style = list(
      cell_fill(color = "#F0F7FF"), # Gris oscuro
      cell_text(color = "black") # Texto blanco para contraste
    ),
    locations = cells_column_labels()) %>% 
  tab_style(
    style = list(cell_text(weight = "bold")),
    locations = cells_body(columns = c(Eje))) %>% 
  tab_footnote(
    footnote = "CDP: Certificado de Disponibilidad Presupuestal",
    locations = cells_column_labels(columns = c(Disponibilidad, P_Disponibilidad))) %>% 
  tab_source_note(
    source_note = "Fuente: Dirección Nacional de Planeación y Estadística - Sistema Financiero QUIPU "
  ) %>% 
  tab_source_note(
    source_note = paste("Corte de información:", Sys.Date()-1)) %>% 
  tab_style(
    style = list(
      cell_fill(color = "#9b89b3"), # Color verde claro (puedes usar nombres o Hex)
      cell_text(weight = "bold", color = "white") # Texto en negrita y color
    ),
    locations = cells_column_spanners(spanners = "spanner_presupuesto")
  ) %>% 
  tab_options(
    # Cambia el sistema de números por símbolos estándar (el primero es *)
    footnotes.marks = "standard",
    source_notes.font.size = px(12),
    row.striping.include_table_body = TRUE
  ) 










# Avance Proyecto Estadísticas DNPE

QUIPU_2026V2 %>%  
  inner_join(BPUN_2026, by = c("PROYECTO" = "quipu")) %>% 
  filter(VIGENCIA_MOV %in% c(2025, 2026), NOMBRE_DIRECTOR == "RODRIGUEZ RODRIGUEZ ALBERTO") %>% 
  select(c("Vigencia" = "VIGENCIA_MOV", 
           "Apropiacion" =  "VR_APROP_VIGENCIA", 
           "Disponibilidad" =  "VR_DISPONIBILIDAD", 
           "Compromisos" = "VR_COMPROMISOS", 
           "Pagos" ="VR_PAGOS_VIG_ACT")) %>% 
  summarise(Apropiacion = sum(Apropiacion), 
            Disponibilidad = sum(Disponibilidad),
            Compromisos = sum(Compromisos),
            Pagos = sum(Pagos),
            .by = c(Vigencia)) %>% 
  summarise(across(c(Apropiacion:Pagos), ~ sum(.x)), .by = c(Vigencia)) %>%
  mutate(P_Disponibilidad = Disponibilidad / Apropiacion,
         P_Compromisos = Compromisos / Apropiacion,
         P_Pagos = Pagos / Compromisos,
         Vigencia = paste("Vigencia", Vigencia)) %>%
  mutate(across(c(P_Disponibilidad:P_Pagos), ~ round(.x, 4))) %>% 
  pivot_longer(cols = starts_with("P_"),
               names_to = "Proceso",
               values_to = "Valor") %>% 
  mutate(Proceso = fct_rev(fct_inorder(Proceso)),
         Vigencia = fct_inorder(as.character(Vigencia)),
         Barra = 1) %>% 
  ggplot() +
  geom_rect(xmin = -0.009, xmax = 0.35, ymin = 0.6, ymax = 3.4, alpha = 0.04, fill = "red")+
  geom_rect(xmin = 0.35, xmax = 0.70, ymin = 0.6, ymax = 3.4, alpha = 0.05, fill = "yellow")+
  geom_rect(xmin = 0.70, xmax = 1.01,ymin = 0.6, ymax = 3.4, alpha = 0.03, fill = "green")+
  geom_bar(aes(x = Barra, y = Proceso), stat = "identity", fill = "gray80", col= "white", width = 0.6)+
  geom_bar(aes(x = Valor, y = Proceso), stat = "identity", fill = "#41b6c4", width = 0.50)+
  geom_text(aes(x = Valor, y = Proceso, label = scales::percent(Valor, accuracy = 0.01)), hjust = -0.3, size = 3.5, fontface = "bold")+
  labs(x = "\nPorcentaje de ejecución financiera", y = "Estado de los recursos financieros\n")+
  scale_x_continuous(limits = c(0, 1), labels = scales::percent, n.breaks = 10)+
  scale_y_discrete(labels = c("Con pagos", "Con compromisos", "Con Certificado de\nDisponibilidad (CDP)"))+
  facet_wrap(vars(Vigencia), ncol = 1)+
  theme(aspect.ratio = 1/2.5, 
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 11, hjust = 0),
        panel.background = element_rect(fill = "gray100", color = "darkblue"),
        panel.grid = element_blank(), panel.spacing.y = unit(2, "lines"),
        strip.text = element_text(size = 14, face = "bold", color = "white"),
        strip.background = element_rect(fill = "#addd8e", color = "black"))


# REVISAR VISTA FINANCIERA -------


QUIPU_2026_PGD <- QUIPU_2026V2 %>% select(EMPRESA, NOMBRE_EMPRESA, PROYECTO, NOMBRE_PROYECTO) 
                  
Empresas <- BPUN_2026 %>%  
  left_join(QUIPU_2026_PGD, by = c("quipu" = "PROYECTO"))


