library(shiny)
library(shinybulma)
library(formattable)
library(cowplot)
library(ggiraph)
library(plotly)

options(shiny.sanitize.errors = FALSE)

shinyServer(function(input, output, session) {
  
  # #filtrar comunas
  # observeEvent(input$region, {
  #   req(input$region != "")
  #   
  #   comunas_filtradas <- provincias_comunas %>% 
  #     #filter(region == "Antofagasta") %>% 
  #     filter(region == input$region) %>% 
  #     select(comuna) %>% 
  #     arrange(comuna) %>% 
  #     pull()
  #   
  #   updateSelectInput(session, "comuna",
  #                     choices = comunas_filtradas,
  #                     selected = ifelse(input$region == "Metropolitana de Santiago", "Santiago", comunas_filtradas[1])
  #   )
  # }) 
  # 
  # #obtener provincia de la comuna
  # provincia <- reactive({#reactive({
  #   req(input$provincia != "")
  #   d <- provincias_comunas %>% 
  #     #filter(comuna == "Puente Alto") %>% 
  #     filter(comuna == input$provincia) %>% 
  #     mutate(provincia = as.character(provincia)) %>% 
  #     select(provincia) %>% 
  #     pull()
  #   return(d)
  # })
  
  #filtrar provincia ----
  observeEvent(input$region, {
    req(input$region != "")
    
    provincias <- movilidad %>% 
      select(region, provincia) %>% 
      distinct() %>% 
      na.omit() %>% 
      filter(region == input$region) %>% 
      select(provincia) %>% 
      pull()
    
    updateSelectInput(session, "provincia",
                      choices = provincias,
                      selected = ifelse(input$region == "Metropolitana de Santiago", "Santiago", provincias[1]))
  })
  
  provincia <- reactive({ input$provincia })
  
  output$region_seleccionada <- renderText({ input$region })
  output$provincia_seleccionada <- renderText({ input$provincia })
  output$region_seleccionada2 <- renderText({ input$region })
  output$provincia_seleccionada2 <- renderText({ input$provincia })
  
  # fecha ----
  rango_fecha <- reactive({ as.numeric(input$fecha[2] - input$fecha[1]) })
  cambio_eje_fecha <- 62
  
  # #cuarentenas
  # #importar cuarentenas desde el repositorio del ministerio de ciencias
  # cuarentenas <- reactive({
  #   readr::read_csv("https://github.com/MinCiencia/Datos-COVID19/raw/master/output/producto74/paso_a_paso.csv",
  #                   col_types = readr::cols()) %>% 
  #     tidyr::pivot_longer(starts_with("20"), names_to = "fecha", values_to = "etapa_n") %>% 
  #     mutate(etapa = case_when(etapa_n == 1 ~ "Cuarentena",
  #                              etapa_n == 2 ~ "Transición",
  #                              etapa_n == 3 ~ "Preparación",
  #                              etapa_n == 4 ~ "Apertura inicial",
  #                              etapa_n == 5 ~ "Apertura avanzada")) %>% 
  #     mutate(etapa = forcats::fct_reorder(etapa, etapa_n)) %>% 
  #     mutate(fecha = lubridate::ymd(fecha)) %>% 
  #     #anexar provincias y comunas a partir del codigo de comuna
  #     left_join(provincias_comunas %>% select(region, provincia, comuna, codigo_comuna), by = "codigo_comuna")
  # })
  
  
  #procesamiento ----
  
  #filtrar por nacional, región o provincia
  pais <- reactive({
    req(input$provincia != "", input$sector != "", length(input$sector) >= 1, movilidad)
    movilidad %>% 
      filter(is.na(region)) %>% 
      mutate(unidad = "País") %>% 
      filter(sector %in% input$sector) %>% #filtrar sectores
      mutate(valor = media_movil(valor, input$suavizar)) #media móvil
  })
  
  region <- reactive({
    req(input$provincia != "", input$sector != "", length(input$sector) >= 1, movilidad)
    movilidad %>% 
      filter(region == input$region) %>% 
      rename(unidad = region) %>% 
      filter(is.na(provincia)) %>% 
      filter(sector %in% input$sector) %>% #filtrar sectores
      mutate(valor = media_movil(valor, input$suavizar)) #media móvil
  })
  
  provincia <- reactive({
    req(input$provincia != "", input$sector != "", length(input$sector) >= 1, movilidad)
    movilidad %>% 
      filter(provincia == input$provincia) %>% 
      rename(unidad = provincia) %>% 
      filter(sector %in% input$sector) %>% #filtrar sectores
      mutate(valor = media_movil(valor, input$suavizar)) #media móvil
  })
    
  # #cambios en cuarentenas
  #   cuarentenas_cambios <- reactive({
  #     req(datos2())
  #   #minimos y maximos de fecha
  #   fecha_minima <- min(datos2()$fecha)
  #   fecha_maxima <- max(datos2()$fecha)
  #   
  #   #filtros de fecha
  #   #fecha_minima_f <- input$fecha[1] #lubridate::ymd(paste("2021", input$mes_inicio, "01"))
  #   #fecha_maxima_f <- input$fecha[2] #lubridate::ymd(paste("2021", input$mes_fin, "31"))
  #   
  #   #solo cambios
  #   cuarentenas_cambios <- cuarentenas() %>% 
  #     filter(comuna == input$provincia) %>% 
  #     select(etapa, etapa_n, fecha) %>% 
  #     filter(etapa != lag(etapa)) %>% 
  #     mutate(hasta = lead(fecha),
  #            hasta = replace(hasta, is.na(hasta), fecha_maxima))
  #   return(cuarentenas_cambios)
  #   })
    
    # #media movil
    # datos3 <- reactive({
    #   req(datos2())
    # if (input$suavizar == "No") {
    #   d3 <- datos2()
    # } else if (input$suavizar != "No") {
    #   if (input$suavizar == "1 semana") {
    #     dias <- 7
    #   } else if (input$suavizar == "2 semanas") {
    #     dias <- 14
    #   } else if (input$suavizar == "3 semanas") {
    #     dias <- 21
    #   } else {
    #     dias <- readr::parse_number(input$suavizar)
    #   }
    #   #suavizar
    #   d3 <- datos2() %>%
    #     group_by(sector) %>%
    #     mutate(valor = zoo::rollmean(valor, k = dias, 
    #                                  fill = "extend"))
    # }
    #   return(d3)
    # })
    
    
    #—----
  
    #pais ----
    output$grafico_pais <- renderPlot({
    p <- pais() %>% 
      ggplot()
    
    #fondo de cuarentenas
    if (!is.null(input$fondo)) {
      p <- p +
        #cuarentenas
        geom_col(data = cuarentenas_pais, aes(fecha, (porcentaje*100)+25, fill=etapa), 
                 width = 1, alpha = 0.4)
    }
    
    #gráfico base
    p <- p +
      g_base_2 +
      #limites horizontales
      coord_cartesian(ylim = c(0, 200), expand = 0, 
                      xlim = c(input$fecha[1], input$fecha[2]))
      
    #eje y doble o normal
    if (!is.null(input$covid)) {
      p <- p +
        #linea de covid
        geom_line(data = covid_pais(), aes(fecha, scales::rescale(casos, to=c(10, 190))), #to=c(-50, 50))), 
                  size = 0.6, alpha=0.6) +
        #g_segundo_eje
        scale_y_continuous(labels = function (x) paste0(x-100, "%"), 
                           breaks = c(-75, -50, -25, 0, 25, 50, 75)+100,
                           #eje y secundario
                           sec.axis = sec_axis(~ scales::rescale(., to=c(min(covid_pais()$casos), max(covid_pais()$casos))), #from=c(0, max(.))),
                                               breaks = scales::breaks_extended(6), #breaks covid
                                               name = "Casos activos de Covid-19"
                           ))
    } else {
      p <- p +
        g_primer_eje_2
    }
    
    #escalas y tema
    p <- p +
      g_escalas + g_temas
    
    #escala responsiva para celulares
    if (input$dimension[1] < 640) {
      p <- p + theme(legend.box = "vertical")
    }
    
    return(p)
  }, res = 90)
  
  
  
  #region ----
  output$grafico_region <- renderPlot({
    p <- region() %>% 
      ggplot()
    
    #fondo de cuarentenas
    if (!is.null(input$fondo)) {
      p <- p +
        #cuarentenas
        geom_col(data = cuarentenas_region_f(), aes(fecha, (porcentaje*100)+100, fill=etapa), 
                 width = 1, alpha = 0.4)
    }
    
    #gráfico base
    p <- p +
      g_base_2 +
      #limites horizontales
      coord_cartesian(ylim = c(0, 200), expand = 0, 
                      xlim = c(input$fecha[1], input$fecha[2]))
    
    #eje y doble o normal
    #if (input$covid == TRUE) {
    if (!is.null(input$covid)) {
      p <- p +
        #linea de covid
        geom_line(data = covid_region(), aes(fecha, scales::rescale(casos, to=c(10, 190))), 
                  size = 0.6, alpha=0.6) +
        #g_segundo_eje
        scale_y_continuous(labels = function (x) paste0(x-100, "%"), 
                           breaks = c(-75, -50, -25, 0, 25, 50, 75)+100,
                           #eje y secundario
                           sec.axis = sec_axis(~ scales::rescale(., to=c(min(covid_region()$casos), max(covid_region()$casos))), #from=c(0, max(.))),
                                               breaks = scales::breaks_extended(6), #breaks covid
                                               name = "Casos activos de Covid-19"
                           ))
    } else {
      p <- p + g_primer_eje_2
    }
    
    #escalas y tema
    p <- p + g_escalas + g_temas
    
    #escala responsiva para celulares
    if (input$dimension[1] < 640) {
      p <- p + theme(legend.box = "vertical")
    }
    
    return(p)
  }, res = 90)
  
  
  
  #provincia ----
  output$grafico_provincia <- renderPlot({
    p <- provincia() %>% 
      ggplot()
    
    #fondo de cuarentenas
    if (!is.null(input$fondo)) {
      p <- p +
        #cuarentenas
        geom_col(data = cuarentenas_provincia_f(), aes(fecha, (porcentaje*100)+100, fill=etapa), 
                 width = 1, alpha = 0.4)
    }
    
    #gráfico base
    p <- p +
      g_base_2 +
      #limites horizontales
      coord_cartesian(ylim = c(0, 200), expand = 0, 
                      xlim = c(input$fecha[1], input$fecha[2]))
    
    #eje y doble o normal
    #if (input$covid == TRUE) {
    if (!is.null(input$covid)) {
      p <- p +
        #linea de covid
        geom_line(data = covid_provincia(), aes(fecha, scales::rescale(casos, to = c(10, 190))), 
                  size = 0.6, alpha=0.6) +
        #g_segundo_eje
        scale_y_continuous(labels = function (x) paste0(x-100, "%"), 
                           breaks = c(-75, -50, -25, 0, 25, 50, 75)+100,
                           #eje y secundario
                           sec.axis = sec_axis(~scales::rescale(., to = c(min(covid_provincia()$casos), max(covid_provincia()$casos))), #from=c(0, max(.))),
                                              breaks = scales::breaks_extended(6), #breaks covid
                                               name = "Casos activos de Covid-19"
                           ))
    } else {
      p <- p + g_primer_eje_2
    }
    
    #escalas y tema
    p <- p + g_escalas + g_temas
    
    #escala responsiva para celulares
    if (input$dimension[1] < 640) {
      p <- p + theme(legend.box = "vertical")
    }
    
    return(p)
  }, res = 90)
  
  
  #—----
  
  #población en cuarentena ----
  
  load("datos/cuarentenas_diarias.rdata")
  
  #provincia ----
  cuarentenas_provincia <- cuarentenas %>% 
    select(fecha, etapa, comuna, provincia, region) %>% 
    #limpiar comunas, anexar población
    distinct(region, comuna, fecha, .keep_all = T) %>%
    left_join(comunas %>% select(comuna, poblacion)) %>% 
    #arrange(fecha, region) %>% 
    #calcular provincia desde comunas
    group_by(provincia, fecha, etapa) %>% 
    summarize(poblacion = sum(poblacion))
  
  cuarentenas_provincia_f <- reactive({
    cuarentenas_provincia %>% 
      filter(provincia == input$provincia) %>% 
      group_by(fecha) %>% 
      mutate(porcentaje = poblacion/sum(poblacion))
  })
  
  output$cuarentenas_provincia_g <- renderPlot({
  cuarentenas_provincia %>% 
    filter(provincia == input$provincia) %>% 
    group_by(fecha, provincia) %>% 
    mutate(porcentaje = poblacion/sum(poblacion)) %>% 
    ggplot(aes(fecha, porcentaje, fill = etapa)) +
      coord_cartesian(xlim = c(input$fecha[1], input$fecha[2])) +
    g_cuarentenas +
    g_cuarentenas_tema
  }, res = 90)
  
  #región ----
  cuarentenas_region <- cuarentenas %>% 
    select(fecha, etapa, comuna, provincia, region) %>% 
    #limpiar comunas, anexar población
    distinct(region, comuna, fecha, .keep_all = T) %>%
    left_join(comunas %>% select(comuna, poblacion)) %>% 
    #arrange(fecha, region) %>% 
    #calcular region desde comunas
    group_by(region, fecha, etapa) %>% 
    summarize(poblacion = sum(poblacion))
  
  
  cuarentenas_region_f <- reactive({
    cuarentenas_region %>% 
      filter(region == input$region) %>% 
      group_by(fecha) %>% 
      mutate(porcentaje = poblacion/sum(poblacion))
  })
  
  output$cuarentenas_region_g <- renderPlot({
  cuarentenas_region %>% 
    filter(region == input$region) %>% 
    group_by(fecha, region) %>% 
    mutate(porcentaje = poblacion/sum(poblacion)) %>% 
    ggplot(aes(fecha, porcentaje, fill = etapa)) +
      coord_cartesian(xlim = c(input$fecha[1], input$fecha[2])) +
      g_cuarentenas +
      g_cuarentenas_tema
  }, res = 90)
  
  #país ----
  cuarentenas_pais <- cuarentenas %>%
    select(fecha, etapa, comuna, provincia, region) %>% 
    #limpiar comunas, anexar población
    distinct(region, comuna, fecha, .keep_all = T) %>%
    left_join(comunas %>% select(comuna, poblacion)) %>% 
    #calcular país
    group_by(fecha, etapa) %>% 
    summarize(poblacion = sum(poblacion, na.rm=T)) %>% 
    #porcentaje
    group_by(fecha) %>% 
    mutate(porcentaje = poblacion/sum(poblacion))
  
  output$cuarentenas_pais_g <- renderPlot({
  cuarentenas_pais %>% 
    group_by(fecha) %>% 
    mutate(porcentaje = poblacion/sum(poblacion),
           total = sum(poblacion)) %>% 
    ggplot(aes(fecha, porcentaje, fill = etapa)) +
      coord_cartesian(xlim = c(input$fecha[1], input$fecha[2])) +
      g_cuarentenas +
      g_cuarentenas_tema
  }, res = 90)
  
  
  #—----
  
  #dobles ----
  #pais ----
  d_pais_movilidad <- reactive({
    p <- pais() %>% 
      filter(fecha >= input$fecha[1], fecha <= input$fecha[2]) %>% 
      ggplot() +
      g_base_2 +
      #tooltip
      geom_point_interactive(aes(fecha, valor+100, col = sector,
                                 tooltip = paste(format(fecha, "%d de %B, %Y"), "\n",
                                                 "Movilidad:", valor, "\n", 
                                                 sector)), size = 3, alpha = 0.01) +
      coord_cartesian(xlim = c(input$fecha[1], input$fecha[2]))
    
    #eje fechas
    if (rango_fecha() > cambio_eje_fecha) {
      p <- p + g_eje_mensual
    } else {
      p <- p + g_eje_semanal
    }
    
    #eje y doble o normal
    if (!is.null(input$covid)) {
      p <- p +
        #linea de covid
        geom_line(data = covid_pais(), aes(fecha, scales::rescale(casos, to=c(10, 190))), size = 0.6, alpha=0.6) +
        scale_y_continuous(labels = function (x) paste0(x-100, "%"), breaks = c(-75, -50, -25, 0, 25, 50, 75)+100,
                           #eje y secundario
                           sec.axis = sec_axis(~ scales::rescale(., to=c(min(covid_pais()$casos), max(covid_pais()$casos))), breaks = scales::breaks_extended(6), name = "Casos activos de Covid-19"
                           ))
    } else {
      p <- p + g_primer_eje_2
    }
    
    #escalas y tema
    p <- p + g_escalas + g_temas + g_leyenda
  })
  
  #cuarentenas
  d_pais_cuarentena <- reactive({
    p <- cuarentenas_pais %>% 
      filter(fecha >= input$fecha[1], fecha <= input$fecha[2]) %>% 
      ggplot() +
      #tooltip
      geom_col_interactive(aes(fecha, (porcentaje*100), fill=etapa,
                   tooltip = paste(format(fecha, "%d de %B, %Y"), "\n",
                                   "Etapa:", etapa, "\n", 
                                   paste0(round(porcentaje*100, 1), "% de la población")
                                   )), width = 1, alpha = 1) +
      coord_cartesian(ylim = c(0, 100), expand = 0,
                      xlim = c(input$fecha[1], input$fecha[2])) +
      #escalas y tema
      g_escalas +
      g_temas +
      g_ejes_cuarentenas +
      g_ajustes_cuarentenas +
      guides(fill = guide_legend(override.aes = list(size = 3, alpha = 0.9), nrow = 1))

      
      #escala responsiva para celulares
      if (input$dimension[1] < 640) {
        p <- p + guides(fill = guide_legend(override.aes = list(size = 3, alpha = 0.9), nrow = 2))
      } else {
        p <- p
      }
  })
  
  #graficos combinados
  output$d_pais <- renderPlot({
    plot_grid(d_pais_movilidad(), d_pais_cuarentena(),
              ncol = 1, align = "v", 
              rel_heights = opciones_cowplot)
  }, res = 90)
  
  #ggiraph interactivo
  output$d_pais2 <- renderGirafe({
    p <- plot_grid(d_pais_movilidad(), d_pais_cuarentena(),
                   ncol = 1, align = "v", 
                   rel_heights = opciones_cowplot)
    
    girafe(ggobj = p,
           pointsize = 18,
           width_svg = 7.5, height_svg = 5,
           fonts = list(sans = "Open Sans"),
           options = list(
             #opts_hover(css = "opacity:0.1"), 
             opts_tooltip(#use_fill = TRUE,
                          css = "opacity: 0.7; padding:5px; 
                          border-radius:2px; border: white 1px solid; 
                          background-color: #0176DE; color: white;
                          font-family: Open Sans;"),
             opts_toolbar(position = "topright", saveaspng = FALSE)))
  })
  

  
  #region ----
  d_region_movilidad <- reactive({
    p <- region() %>% 
      filter(fecha >= input$fecha[1], fecha <= input$fecha[2]) %>% 
      ggplot() +
      g_base_2 +
      #tooltip
      geom_point_interactive(aes(fecha, valor+100, col = sector,
                                 tooltip = paste(format(fecha, "%d de %B, %Y"), "\n",
                                                 "Movilidad:", valor, "\n", 
                                                 sector)), size = 3, alpha = 0.01) +
      coord_cartesian(xlim = c(input$fecha[1], input$fecha[2]))
    
    #eje fechas
    if (rango_fecha() > cambio_eje_fecha) {
      p <- p + g_eje_mensual
    } else {
      p <- p + g_eje_semanal
    }
    
    #eje y doble o normal
    if (!is.null(input$covid)) {
      p <- p +
        #linea de covid
        geom_line(data = covid_region(), aes(fecha, scales::rescale(casos, to=c(10, 190))), size = 0.6, alpha=0.6) +
        scale_y_continuous(labels = function (x) paste0(x-100, "%"), breaks = c(-75, -50, -25, 0, 25, 50, 75)+100,
                           #eje y secundario
                           sec.axis = sec_axis(~ scales::rescale(., to=c(min(covid_region()$casos), max(covid_region()$casos))), breaks = scales::breaks_extended(6), name = "Casos activos de Covid-19"
                           ))
    } else {
      p <- p + g_primer_eje_2
    }
    
    #escalas y tema
    p <- p + g_escalas + g_temas + g_leyenda
  })
  
  #cuarentenas
  d_region_cuarentena <- reactive({
    p <- cuarentenas_region_f() %>% 
      filter(fecha >= input$fecha[1], fecha <= input$fecha[2]) %>% 
      ggplot() +
      #geom_col(aes(fecha, (porcentaje*100), fill=etapa), width = 1, alpha = 0.4) +
      #tooltip
      geom_col_interactive(aes(fecha, (porcentaje*100), fill=etapa,
                               tooltip = paste(format(fecha, "%d de %B, %Y"), "\n",
                                               "Etapa:", etapa, "\n", 
                                               paste0(round(porcentaje*100, 1), "% de la población")
                               )), width = 1, alpha = 1) +
      coord_cartesian(ylim = c(0, 100), expand = 0,
                      xlim = c(input$fecha[1], input$fecha[2])) +
      #escalas y tema
      g_escalas +
      g_temas +
      g_ejes_cuarentenas +
      g_ajustes_cuarentenas +
      guides(fill = guide_legend(override.aes = list(size = 3, alpha = 0.9), nrow = 1))
    
    #escala responsiva para celulares
    if (input$dimension[1] < 640) {
      p <- p + guides(fill = guide_legend(override.aes = list(size = 3, alpha = 0.9), nrow = 2))
    } else {
      p <- p
    }
  })
  
  #graficos combinados
  output$d_region <- renderPlot({
    plot_grid(d_region_movilidad(), d_region_cuarentena(),
              ncol = 1, align = "v", 
              rel_heights = opciones_cowplot)
  }, res = 90)
  
  #ggiraph
  output$d_region2 <- renderGirafe({
    p <- plot_grid(d_region_movilidad(), d_region_cuarentena(),
              ncol = 1, align = "v", 
              rel_heights = opciones_cowplot)
  
  girafe(ggobj = p,
         pointsize = 18,
         width_svg = 7.5, height_svg = 5,
         fonts = list(sans = "Open Sans"),
         options = list(
           #opts_hover(css = "opacity:0.1"), 
           opts_tooltip(#use_fill = TRUE,
             css = "opacity: 0.7; padding:5px; 
                          border-radius:2px; border: white 1px solid; 
                          background-color: #0176DE; color: white;
                          font-family: Open Sans;"),
           opts_toolbar(position = "topright", saveaspng = FALSE)))
  })
  
  
  #provincia ----
  d_provincia_movilidad <- reactive({
    p <- provincia() %>% 
      filter(fecha >= input$fecha[1], fecha <= input$fecha[2]) %>% 
      ggplot() +
      g_base_2 +
      #tooltip
      geom_point_interactive(aes(fecha, valor+100, col = sector,
                                 tooltip = paste(format(fecha, "%d de %B, %Y"), "\n",
                                                 "Movilidad:", valor, "\n", 
                                                 sector)), size = 3, alpha = 0.01) +
      coord_cartesian(xlim = c(input$fecha[1], input$fecha[2]))
    
    #eje fechas
    if (rango_fecha() > cambio_eje_fecha) {
      p <- p + g_eje_mensual
    } else {
      p <- p + g_eje_semanal
    }
    
    #eje y doble o normal
    if (!is.null(input$covid)) {
      p <- p +
        #linea de covid
        geom_line(data = covid_provincia(), aes(fecha, scales::rescale(casos, to=c(10, 190))), size = 0.6, alpha=0.6) +
        scale_y_continuous(labels = function (x) paste0(x-100, "%"), breaks = c(-75, -50, -25, 0, 25, 50, 75)+100,
                           #eje y secundario
                           sec.axis = sec_axis(~ scales::rescale(., to=c(min(covid_provincia()$casos), max(covid_provincia()$casos))), breaks = scales::breaks_extended(6), name = "Casos activos de Covid-19"
                           ))
    } else {
      p <- p + g_primer_eje_2
    }
    
    #escalas y tema
    p <- p + g_escalas + g_temas + g_leyenda
  })
  
  #cuarentenas
  d_provincia_cuarentena <- reactive({
    p <- cuarentenas_provincia_f() %>% 
      filter(fecha >= input$fecha[1], fecha <= input$fecha[2]) %>% 
      ggplot() +
      #geom_col(aes(fecha, (porcentaje*100), fill=etapa), width = 1, alpha = 0.4) +
      #tooltip
      geom_col_interactive(aes(fecha, (porcentaje*100), fill=etapa,
                               tooltip = paste(format(fecha, "%d de %B, %Y"), "\n",
                                               "Etapa:", etapa, "\n", 
                                               paste0(round(porcentaje*100, 1), "% de la población")
                               )), width = 1, alpha = 1) +
      coord_cartesian(ylim = c(0, 100), expand = 0,
                      xlim = c(input$fecha[1], input$fecha[2])) +
      #escalas y tema
      g_escalas +
      g_temas +
      g_ejes_cuarentenas +
      g_ajustes_cuarentenas +
      guides(fill = guide_legend(override.aes = list(size = 3, alpha = 0.9), nrow = 1))
    
    #escala responsiva para celulares
    if (input$dimension[1] < 640) {
      p <- p + guides(fill = guide_legend(override.aes = list(size = 3, alpha = 0.9), nrow = 2))
    } else {
      p <- p
    }
  })
  
  #graficos combinados
  output$d_provincia <- renderPlot({
    plot_grid(d_provincia_movilidad(), d_provincia_cuarentena(),
              ncol = 1, align = "v", 
              rel_heights = opciones_cowplot)
  }, res = 90)
  
  #ggiraph
  output$d_provincia2 <- renderGirafe({
    p <- plot_grid(d_provincia_movilidad(), d_provincia_cuarentena(),
              ncol = 1, align = "v", 
              rel_heights = opciones_cowplot)
  
  girafe(ggobj = p,
         pointsize = 18,
         width_svg = 7.5, height_svg = 5,
         fonts = list(sans = "Open Sans"),
         options = list(
           #opts_hover(css = "opacity:0.1"), 
           opts_tooltip(#use_fill = TRUE,
             css = "opacity: 0.7; padding:5px; 
                          border-radius:2px; border: white 1px solid; 
                          background-color: #0176DE; color: white;
                          font-family: Open Sans;"),
           opts_toolbar(position = "topright", saveaspng = FALSE)))
  })
  #—----
  
  # #datitos 
  # output$dato_mayor_aumento <- renderUI({
  #   dato <- movilidad %>% 
  #     movilidad_cambios() %>% 
  #     filter(cambio > 0) %>% 
  #     filter(fecha == max(fecha)) %>%  #ultima fecha
  #     arrange(desc(cambio)) %>% 
  #     slice(1)
  #   
  #   bloque_datos(titulo = "Mayor aumento diario",
  #                cambio = dato$cambio,
  #                hoy = dato$valor,
  #                ayer = dato$ayer,
  #                provincia = dato$provincia,
  #                sector = dato$sector,
  #                region = dato$region)
  # })
  # 
  # output$dato_mayor_reduccion <- renderUI({
  #   dato <- movilidad %>% 
  #     movilidad_cambios() %>% 
  #     filter(cambio < 0) %>% 
  #     filter(fecha == max(fecha)) %>%  #ultima fecha
  #     arrange(cambio) %>% 
  #     slice(1)
  #   
  #   bloque_datos(titulo = "Mayor reducción diaria",
  #                cambio = dato$cambio,
  #                hoy = dato$valor,
  #                ayer = dato$ayer,
  #                provincia = dato$provincia,
  #                sector = dato$sector,
  #                region = dato$region)
  # })
  # 
  # output$dato_mayor_movilidad <- renderUI({
  #   dato <- movilidad %>% 
  #     ungroup() %>% 
  #     filter(!is.na(provincia)) %>%  #filtrar regiones
  #     filter(fecha == max(fecha)) %>%  #ultima fecha
  #     arrange(desc(valor)) %>% 
  #     slice(1)
  #   
  #   bloque_datos(titulo = "Mayor movilidad",
  #                cambio = dato$valor,
  #                hoy = dato$valor,
  #                ayer = 0,
  #                provincia = dato$provincia,
  #                sector = dato$sector,
  #                region = dato$region)
  # })
  # 
  # #nivel mas bajo de movilidad
  # output$dato_menor_movilidad <- renderUI({
  #   dato <- movilidad %>% 
  #     ungroup() %>% 
  #     filter(!is.na(provincia)) %>%  #filtrar regiones
  #     filter(fecha == max(fecha)) %>%  #ultima fecha
  #     arrange(valor) %>% 
  #     slice(1)
  #   
  #   bloque_datos(titulo = "Menor movilidad",
  #                cambio = dato$valor,
  #                hoy = dato$valor,
  #                ayer = 0,
  #                provincia = dato$provincia,
  #                sector = dato$sector,
  #                region = dato$region)
  # })
  
  #casos covid activos
  # output$dato_covid_activos <- renderUI({
  #   dato <- covid_activos_f() %>% 
  #     arrange(fecha) %>% 
  #     mutate(ayer = lag(casos)) %>% 
  #     filter(fecha == max(fecha)) %>% 
  #     slice(1)
  #   
  #   bloque_datos(titulo = "Casos Covid-19 activos",
  #                cambio = dato$casos,
  #                hoy = dato$casos,
  #                ayer = dato$ayer,
  #                provincia = paste("Casos activos al", format(dato$fecha, "%d de %B")),
  #                sector = dato$comuna,
  #                region = dato$region,
  #                texto = "casos")
  # })
  # 
  # output$dato_covid_peak <- renderUI({
  #   casos_hoy <- covid_activos_f() %>% 
  #     filter(fecha == max(fecha)) %>% 
  #     slice(1)
  #   
  #   dato <- covid_activos_f() %>% 
  #     arrange(desc(casos)) %>% 
  #     slice(1)
  #   
  #   bloque_datos(titulo = "Peak de casos activos",
  #                cambio = dato$casos,
  #                hoy = dato$casos,
  #                ayer = casos_hoy$casos,
  #                provincia = paste("Peak de casos:", format(dato$fecha, "%d de %B, %Y")),
  #                sector = dato$comuna,
  #                region = dato$region,
  #                texto = "casos")
  # })
  # 
  # output$dato_covid_anti_peak <- renderUI({
  #   casos_hoy <- covid_activos_f() %>% 
  #     filter(fecha == max(fecha)) %>% 
  #     slice(1)
  #   
  #   dato <- covid_activos_f() %>%
  #     filter(lubridate::year(fecha) == lubridate::year(Sys.Date())) %>% 
  #     arrange(casos) %>% 
  #     slice(1)
  #   
  #   bloque_datos(titulo = "Día con menos casos activos este año",
  #                cambio = dato$casos,
  #                hoy = dato$casos,
  #                ayer = casos_hoy$casos,
  #                provincia = paste("Día con menores casos:", format(dato$fecha, "%d de %B, %Y")),
  #                sector = dato$comuna,
  #                region = dato$region,
  #                texto = "casos")
  # })
  # 
  # #fase cuarentena
  # output$dato_fase_cuarentena <- renderUI({
  #   req(input$provincia != "")
  #   
  #   dato <- cuarentenas() %>%
  #     #select(-c(codigo_region, codigo_comuna, comuna_residencia)) %>%
  #     filter(fecha == max(fecha),
  #            comuna == input$provincia)
  #   
  # 
  #   bloque_datos(titulo = "Plan Paso a Paso",
  #                cambio = dato$etapa_n,
  #                hoy = 0,
  #                ayer = 0,
  #                provincia = paste("Fase:", dato$etapa),
  #                sector = dato$comuna,
  #                region = dato$region,
  #                texto = "",
  #                prefijo = "fase")
  # })
  
  

  
  #casos covid ----
  #descargar datos covid 
  load("datos/covid_diarios.rdata") #descarga cada hora
  # covid_activos <- reactive({
  #   #req(input$covid == TRUE)
  #   covid_diarios
  #   #readr::read_csv("https://coronavirus-api.mat.uc.cl/casos_activos_sintomas_comuna",
  #   
  #   #readr::read_csv("http://localhost:8080/casos_activos_sintomas_comuna",
  #   #                col_types = readr::cols())
  # })
  
  #covid pais
  covid_pais <- reactive({
    covid_diarios %>% 
      filter(fecha >= input$fecha[1], 
             fecha <= input$fecha[2]) %>% 
    group_by(fecha) %>% 
    summarize(casos = sum(casos, na.rm = T))
  })
  
  #covid región
  covid_region <- reactive({
    covid_diarios %>% 
      filter(fecha >= input$fecha[1], 
             fecha <= input$fecha[2]) %>% 
    group_by(fecha, region) %>% 
    summarize(casos = sum(casos, na.rm = T)) %>% 
      mutate(region = recode(region,
                             "Antofagasta"                        = "Antofagasta",
                             "Arica y Parinacota"                 = "Arica y Parinacota",
                             "Atacama"                            = "Atacama",
                             "Aysen"                              = "Aysén del Gral. C. Ibáñez del Campo",
                             "Biobio"                             = "Biobío",
                             "Coquimbo"                           = "Coquimbo",
                             "La Araucania"                      = "La Araucanía",
                             "Del Libertador General Bernardo O’Higgins" = "Lib. Gral. Bernardo O'Higgins",
                             "Los Lagos"                          = "Los Lagos",
                             "Los Rios"                           = "Los Ríos",
                             "Magallanes y la Antartica"          = "Magallanes y Antártica Chilena",
                             "Maule"                              = "Maule",
                             "Metropolitana"                      = "Metropolitana de Santiago",
                             "Nuble"                              = "Ñuble",
                             "Tarapaca"                           = "Tarapacá",
                             "Valparaiso"                         = "Valparaíso")) %>% 
    filter(region == input$region) #filtrar
  })

  
  #covid provincia
  covid_provincia <- reactive({
    covid_diarios %>% 
    left_join(provincias_comunas %>% select(comuna, provincia)) %>% 
    filter(fecha >= input$fecha[1], 
           fecha <= input$fecha[2]) %>% 
    group_by(fecha, provincia) %>% 
    summarize(casos = sum(casos, na.rm = T)) %>% 
    filter(provincia == input$provincia) #filtrar
  })
  
  
  
  # #filtrar casos covid por comuna seleccionada
  # covid_activos_f <- reactive({
  #   req(input$provincia != "")
  #   #req(input$covid == TRUE)
  #   
  #   #filtrar y dar formato
  #   d <- covid_diarios %>% 
  #     select(-comuna) %>% 
  #     #asegurarse que calce la comuna
  #     mutate(codigo_comuna = as.numeric(codigo_comuna)) %>% 
  #     left_join(cuarentenas() %>% 
  #                 select(comuna, codigo_comuna), 
  #               by = "codigo_comuna") %>% 
  #     filter(comuna == input$provincia) %>% 
  #     select(-codigo_region, -poblacion)
  #   return(d)
  # })
  
  #descarga ----
  
  lista_descargas <- reactive({
    list(
    "Movilidad país" = pais() %>% select(-region, -provincia, -provincia_cod),
    "Movilidad región" = region() %>% select(-provincia_cod, -provincia, region = unidad),
    "Movilidad provincia" = provincia() %>% select(-provincia_cod, provincia = unidad),
    "Cuarentenas país" = cuarentenas_pais,
    "Cuarentenas región" = cuarentenas_region_f(),
    "Cuarentenas provincia" = cuarentenas_provincia_f()
    )
  })
    
  
  output$descarga <- downloadHandler(
    filename = paste0("Movilidad Google ", lubridate::today(), ".xlsx"),
    content = function(filename) {
      writexl::write_xlsx(
      #openxlsx::write.xlsx(
        x = lista_descargas(), path = filename
      )
        #colWidths = "auto", borders = "surrounding"
        #headerStyle = openxlsx::createStyle(textDecoration = "BOLD", fontName = "Arial")) 
      }, 
    contentType = "application/xlsx")
  
  
  
  #tabla
  
#   #provincias mayor movilidad
#   output$tabla_mayor_movilidad <- formattable::renderFormattable({
#     movilidad %>% 
#       ungroup() %>% 
#       filter(!is.na(provincia)) %>%  #filtrar regiones
#       #filter(sector == "Mercadería y farmacia") %>% 
#       filter(fecha == max(fecha)) %>%  #ultima fecha
#       group_by(sector) %>% 
#       arrange(desc(valor)) %>% 
#       mutate(id = 1:n()) %>% 
#       filter(id == 1) %>% 
#       select(Provincia = provincia, Región = region, 
#              Sector = sector, Movilidad=valor) %>% 
#       formattable(
#         align = c("l", "l", "l", "c"),
#         list(
#           Provincia = formatter("span", style = ~ style(font.style = "bold")),
#           area(col = "Movilidad") ~ custom_color_tile(gris_claro, gris_oscuro)
#           #area(col = "Movilidad") ~ color_text("white")
#           #area(col = "Tasa") ~ color_tile("#f4e4f4", "#c8b1de")
#         )
#       )
#   })
#   
#   
#   #movilidad promedio sectores
#   output$tabla_sectores_mayor <- formattable::renderFormattable({
#   movilidad %>% 
#     ungroup() %>% 
#     filter(!is.na(provincia)) %>%  #filtrar regiones
#     filter(fecha == max(fecha)) %>% #ultima fecha
#     group_by(sector) %>% 
#     summarize(valor = round(mean(valor, na.rm = T), 1)) %>% 
#     arrange(desc(valor)) %>% 
#     select(Sector = sector,
#            Movilidad = valor) %>% 
#     formattable(
#       align = c("l", "c"),
#       list(
#         Sector = formatter("span", style = ~ style(font.weight = "bold")),
#         area(col = "Movilidad") ~ custom_color_tile(gris_claro, gris_oscuro)
#         #area(col = "Tasa") ~ color_tile("#f4e4f4", "#c8b1de")
#       )
#     )
#   })
#   
#   #movilidad promedio por región
#   output$tabla_regiones_mayor <- formattable::renderFormattable({
#   movilidad %>% 
#     ungroup() %>% 
#     filter(!is.na(region)) %>%  #filtrar regiones
#     filter(is.na(provincia)) %>%  #filtrar regiones
#     filter(fecha == max(fecha)) %>% #ultima fecha
#     group_by(region) %>% 
#     summarize(valor = round(mean(valor, na.rm = T), 1)) %>% 
#     arrange(desc(valor)) %>% 
#       select(Región = region, 
#              Movilidad = valor) %>% 
#       formattable(
#         align = c("l", "c"),
#         list(
#           #Región = formatter("span", style = ~ style(font.weight = "bold")),
#           area(col = "Movilidad") ~ custom_color_tile(gris_claro, gris_oscuro)
#           #area(col = "Tasa") ~ color_tile("#f4e4f4", "#c8b1de")
#         )
#       )
#       
# })
  
  # output$Tabla <- DT::renderDataTable({
  #   table <- covid_activos_f()
  #   datatable(table,
  #             extensions = c('Buttons', 'ColReorder', 'Responsive'), 
  #             options = list( 
  #               dom = "Blfrtip", 
  #               pageLength = 20, 
  #               colReorder = TRUE,
  #               searching = TRUE,
  #               lengthChange = FALSE, 
  #               rownames = FALSE, 
  #               scrollX = TRUE, 
  #               searchHighlight = TRUE,
  #               initComplete = JS(
  #                 "function(settings, json) {",
  #                 "$(this.api().table().header()).css({'background-color': '#FFFFFF', 'color': '#FFFFFF'});",
  #                 "}"
  #               ),
  #               language = list(
  #                 info = "Observaciones _START_ a _END_ de un total de _TOTAL_ observaciones",
  #                 paginate = list(previous = 'Anterior', `next` = 'Siguiente'),
  #                 lengthMenu = "Tabla de _MENU_ observaciones.",
  #                 search = "Buscar"),
  #               buttons = list(list(extend = "collection",
  #                                   buttons = c('pdf', 'csv', 'excel'),
  #                                   text = "Descargar tabla"
  #               ))),
  #             class = 'cell-border compact hover order-column', 
  #             filter = list(position = 'top'))
  # })
  
  #ventana ----
  
  observeEvent(input$ayuda, {
    shinyWidgets::sendSweetAlert(
      session = session,
      title = NULL,
      btn_colors = celeste,
      btn_labels = "Volver",
      closeOnClickOutside = TRUE,
      showCloseButton = TRUE,
      text = list(
        h2("¿Desde dónde se extraen los datos?"),
        p("Los datos del visualizador son extraídos directamente de los archivos CSV que están en el sitio web de Google COVID-19 Community Mobility Report. Estos muestran las tendencias de cómo va variando los números de visitas a determinados lugares y el tiempo que dura en comparación a un valor referencial. En este caso, los datos fueron filtrados exclusivamente para Chile."),
        hr(),
        h2("¿Qué lugares considera el informe?"),
        p("Los lugares o categorías que toma en consideración son: supermercados y farmacias (movilidad en lugares como supermercado, almacenes, tiendas de comida, etc), parques (parques locales, nacionales, playas, plazas, jardínes públicos, etc) estaciones de transporte (por ejemplo, movilidad en metro, tren o bus) tiendas y ocio (movilidad en cafeterías, centros comerciales, restaurantes, cines, bibliotecas, parque de diversiones, etc), zonas residenciales y lugares de trabajo (tendencias de movilidad en los lugares de trabajo)."),
        hr(),
        h2("¿Cómo interpretar el indicador?"),
        ("El informe permite identificar en qué medida las personas de Chile han aumentado o reducido sus visitas o movilidad en las categorías (lugares) que se mencionan anteriormente. Al momento del despliegue, si el porcentaje de variación es negativo, indica que la movilidad ha disminuido. Por su parte, si el porcentaje es positivo, implica que la movilidad de las personas ha aumentado. Para mayor información sobre cómo interpretar patrones comunes de los datos, visite el sitio web: https://support.google.com/covid19-mobility/answer/9825414?hl=es-419"),
        hr(),
        h2("¿Cómo se obtienen los datos?"),
        p("El conjunto de datos es obtenido a partir de los propios usuarios que tienen habilitado el historial de ubicaciones de su cuenta registrada en Google con GPS y el uso de sus teléfonos inteligentes (smartphones). Por ende, no es posible garantizar que se represente el comportamiento exacto de toda la población. Por otra parte, en cuanto a los valores de referencia, Google calcula el valor medio de cada día de la semana durante un periodo de 5 semanas, abarcando desde el 3 de enero al 6 de febrero del 2020."),
        hr(),
        h2("¿Pueden haber datos incompletos en alguna provincia?"),
        p("De acuerdo a la documentación de Google, si los datos no alcanzan los umbrales de calidad y privacidad, es posible que pueda visualizar los campos de algunos lugares o fechas vacíos. Por añadidura, basado en el principio de privacidad diferencial, no se comparte información personal (movimientos, contactos o ubicación) que pueda identificar particularmente a una persona.")
      ),
      html = T
    )
  })
  
})
    