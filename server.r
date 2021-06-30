library(shiny)
library(shinybulma)
library(formattable)

shinyServer(function(input, output, session) {
  
  #filtrar comunas ----
  observeEvent(input$region, {
    req(input$region != "")
    
    comunas_filtradas <- provincias_comunas %>% 
      #filter(region == "Antofagasta") %>% 
      filter(region == input$region) %>% 
      select(comuna) %>% 
      arrange(comuna) %>% 
      pull()
    
    updateSelectInput(session, "comuna",
                      choices = comunas_filtradas)
  }) 
  
  #obtener provincia de la comuna ----
  provincia <- reactive({#reactive({
    req(input$comuna != "")
    d <- provincias_comunas %>% 
      #filter(comuna == "Puente Alto") %>% 
      filter(comuna == input$comuna) %>% 
      mutate(provincia = as.character(provincia)) %>% 
      select(provincia) %>% 
      pull()
    return(d)
  })
  
  #cuarentenas ----
  #importar cuarentenas desde el repositorio del ministerio de ciencias
  cuarentenas <- reactive({
    readr::read_csv("https://github.com/MinCiencia/Datos-COVID19/raw/master/output/producto74/paso_a_paso.csv",
                    col_types = readr::cols()) %>% 
      tidyr::pivot_longer(starts_with("20"), names_to = "fecha", values_to = "etapa_n") %>% 
      mutate(etapa = case_when(etapa_n == 1 ~ "Cuarentena",
                               etapa_n == 2 ~ "Transición",
                               etapa_n == 3 ~ "Preparación",
                               etapa_n == 4 ~ "Apertura inicial",
                               etapa_n == 5 ~ "Apertura avanzada")) %>% 
      mutate(etapa = forcats::fct_reorder(etapa, etapa_n)) %>% 
      mutate(fecha = lubridate::ymd(fecha)) %>% 
      #anexar provincias y comunas a partir del codigo de comuna
      left_join(provincias_comunas %>% select(provincia, comuna, codigo_comuna), by = "codigo_comuna")
  })
  
  #casos covid ----
  covid_activos_f <- reactive({
    req(input$covid == TRUE)
    
    #filtrar y dar formato
    d <- covid_activos() %>% 
      select(-comuna) %>% 
      #asegurarse que calce la comuna
      mutate(codigo_comuna = as.numeric(codigo_comuna)) %>% 
      left_join(cuarentenas() %>% 
                  select(comuna, codigo_comuna), 
                by = "codigo_comuna") %>% 
      filter(comuna == input$comuna) %>% 
      select(-region, -codigo_region, -poblacion)
    
    return(d)
  })
  
  
  #grafico nuevo ----
  output$grafico_cuarentenas <- renderPlot({
    req(input$comuna != "",
        input$sector != "",
        length(input$sector) >= 1,
        movilidad,
        provincia())
    
    #filtrar por región o provincia
    if (input$selector_unidad_geo == "Región") {
      d1 <- movilidad %>% 
        filter(region == input$region) %>% 
        rename(unidad = region) %>% 
        filter(is.na(provincia))
      
    } else if (input$selector_unidad_geo == "Provincia") {
      d1 <- movilidad %>% 
        filter(provincia == provincia()) %>% 
        rename(unidad = provincia)
    }
    
    #filtrar sectores
    d2 <- d1 %>% 
      filter(sector %in% input$sector)
    
    #minimos y maximos de fecha
    fecha_minima <- min(d2$fecha)
    fecha_maxima <- max(d2$fecha)
    
    #filtros de fecha
    fecha_minima_f <- lubridate::ymd(paste("2021", input$mes_inicio, "01"))
    fecha_maxima_f <- lubridate::ymd(paste("2021", input$mes_fin, "31"))
    
    #solo cambios
    cuarentenas_cambios <- cuarentenas() %>% 
      filter(comuna == input$comuna) %>% 
      select(etapa, etapa_n, fecha) %>% 
      filter(etapa != lag(etapa)) %>% 
      mutate(hasta = lead(fecha),
             hasta = replace(hasta, is.na(hasta), fecha_maxima))
    
    
    #suavizar
    if (input$suavizar == "No") {
      d3 <- d2
    } else if (input$suavizar != "No") {
      if (input$suavizar == "1 semana") {
        dias <- 7
      } else if (input$suavizar == "2 semanas") {
        dias <- 14
      } else {
        dias <- readr::parse_number(input$suavizar)
      }
      #suavizar
      d3 <- d2 %>%
        group_by(sector) %>%
        mutate(valor = zoo::rollmean(valor, k = dias, 
                                     fill = "extend"))
    }
    
    #graficar
    p <- d3 %>% 
      ggplot()
    
    #fondo de cuarentenas
    if (input$fondo == TRUE) {
      p <- p +
        #fondo
        geom_rect(data = cuarentenas_cambios, aes(fill = etapa,
                                                  xmin = fecha, xmax = hasta,
                                                  ymin = -Inf, ymax = Inf),
                  alpha = 0.2)
    }
    
    #gráfico base
    p <- p +
      geom_hline(yintercept = 0, size = 0.4, alpha = 0.7) +
      geom_hline(yintercept = 50, size = 0.3, alpha = 0.4, linetype = "dashed") +
      geom_hline(yintercept = -50, size = 0.3, alpha = 0.4, linetype = "dashed") +
      #lineas
      geom_line(aes(fecha, valor, col = sector), show.legend = F) +
      geom_point(aes(fecha, valor, col = sector), size = 0, alpha = 0) +
      #limites horizontales
      coord_cartesian(xlim = c(fecha_minima_f, fecha_maxima_f)) + 
      scale_x_date(date_breaks = "months", date_labels = "%b", 
                   expand = expansion(mult = c(0,0)))
    
    #eje y doble o normal
    if (input$covid == TRUE) {
      p <- p +
        #linea de covid
        geom_line(data = covid_activos_f(), aes(fecha, casos/50), 
                  size = 1, alpha=0.8, linetype = "dashed", lineend="round") +
        scale_y_continuous(labels = function (x) paste0(x, "%"), 
                           breaks = c(-75, -50, -25, 0, 25, 50, 75),
                           #eje y secundario
                           sec.axis = sec_axis(~.*50, #breaks = 0, 
                                               breaks = scales::breaks_extended(6), #breaks covid
                                               labels = function (x) ifelse(x<0, "", x), #eliminar negativos
                                               name = paste("Casos activos de Covid-19 en", 
                                                            as.character(input$comuna))
                           ))
    } else {
      p <- p +
        scale_y_continuous(labels = function (x) paste0(x, "%"), 
                           breaks = c(-75, -50, -25, 0, 25, 50, 75))
    }
    
    p <- p +
      scale_fill_manual(values = rev(c("lightgreen", "yellow1", "orange", "red"))) +
      labs(y = "Cambio porcentual respecto a línea de base") +
      theme_minimal() +
      theme(axis.title.x = element_blank(),
            legend.title = element_blank(),
            panel.grid.minor.x = element_blank(),
            #legend.position = c(.8, .15))
            legend.position = "right") +
      guides(fill = guide_legend(override.aes = list(size = 3, alpha=0.5), ncol = 1)) +
      guides(col = guide_legend(override.aes = list(size = 5, alpha=1, fill=NA, text=NA), ncol = 1))
    
    #poner subtítulo de región o provincia
    if (input$selector_unidad_geo == "Región") {
      p <- p +
        labs(subtitle = paste("Región:", input$region))
    } else if (input$selector_unidad_geo == "Provincia") {
      p <- p +
        labs(subtitle = paste("Provincia:", provincia()))
      
    }
    return(p)
    
    
  }, res = 90)
  
  #box ----
  # output$box_cuarentena <- renderValueBox({
  #   data <- cuarentenas() %>%
  #     select(-c(codigo_region, codigo_comuna, comuna_residencia)) %>%
  #     filter(fecha == max(fecha),
  #                   comuna == input$comuna) %>%
  #     distinct(etapa)
  #   # 
  #   infoBox(title = "Fase Paso a Paso", value = data , icon = icon("virus-slash"),
  #           color = "purple", fill = TRUE)
  #   
  # })
  # 
  # output$casos_activos <- renderValueBox({
  #   data_activos <- covid_activos() %>%
  #     filter(fecha == max(fecha),
  #                   comuna == input$comuna) %>%
  #     select(casos)
  #   
  #   infoBox(title = "Casos activos", value = data_activos, icon = icon("head-side-virus"),
  #           #color = "light-blue", 
  #           fill = TRUE)
  #   
  # })
  # 
  # output$poblacion <- renderValueBox({
  #   poblacion <- covid_activos() %>%
  #     select(!-c(poblacion, comuna)) %>% 
  #     filter(comuna == input$comuna) %>% 
  #     slice_head()
  #   
  #   infoBox(title = "Población", value = poblacion, icon = icon("user-check"),
  #           color = "olive", fill = TRUE)
  #   
  # })
  
  #datitos ----
  
  output$dato_mayor_aumento <- renderUI({
    dato <- movilidad %>% 
      movilidad_cambios() %>% 
      filter(cambio > 0) %>% 
      filter(fecha == max(fecha)) %>%  #ultima fecha
      arrange(desc(cambio)) %>% 
      slice(1)
    
    bloque_datos(titulo = "Mayor aumento diario",
                 cambio = dato$cambio,
                 hoy = dato$valor,
                 ayer = dato$ayer,
                 provincia = dato$provincia,
                 sector = dato$sector,
                 region = dato$region)
  })
  
  output$dato_mayor_reduccion <- renderUI({
    dato <- movilidad %>% 
      movilidad_cambios() %>% 
      filter(cambio < 0) %>% 
      filter(fecha == max(fecha)) %>%  #ultima fecha
      arrange(cambio) %>% 
      slice(1)
    
    bloque_datos(titulo = "Mayor reducción diaria",
                 cambio = dato$cambio,
                 hoy = dato$valor,
                 ayer = dato$ayer,
                 provincia = dato$provincia,
                 sector = dato$sector,
                 region = dato$region)
  })
  
  output$dato_mayor_movilidad <- renderUI({
    dato <- movilidad %>% 
      ungroup() %>% 
      filter(!is.na(provincia)) %>%  #filtrar regiones
      filter(fecha == max(fecha)) %>%  #ultima fecha
      arrange(desc(valor)) %>% 
      slice(1)
    
    bloque_datos(titulo = "Mayor movilidad",
                 cambio = dato$valor,
                 hoy = dato$valor,
                 ayer = 0,
                 provincia = dato$provincia,
                 sector = dato$sector,
                 region = dato$region)
  })
  
  #nivel mas bajo de movilidad
  output$dato_menor_movilidad <- renderUI({
    dato <- movilidad %>% 
      ungroup() %>% 
      filter(!is.na(provincia)) %>%  #filtrar regiones
      filter(fecha == max(fecha)) %>%  #ultima fecha
      arrange(valor) %>% 
      slice(1)
    
    bloque_datos(titulo = "Menor movilidad",
                 cambio = dato$valor,
                 hoy = dato$valor,
                 ayer = 0,
                 provincia = dato$provincia,
                 sector = dato$sector,
                 region = dato$region)
  })
  
  #mayor movilidad en el sector
  
  
  

  
  #descargar datos covid ----
  covid_activos <- reactive({
    
    readr::read_csv("https://coronavirus-api.mat.uc.cl/casos_activos_sintomas_comuna")
    
  })
  
  #tabla ----
  
  #provincias mayor movilidad
  output$tabla_mayor_movilidad <- formattable::renderFormattable({
    movilidad %>% 
      ungroup() %>% 
      filter(!is.na(provincia)) %>%  #filtrar regiones
      #filter(sector == "Mercadería y farmacia") %>% 
      filter(fecha == max(fecha)) %>%  #ultima fecha
      group_by(sector) %>% 
      arrange(desc(valor)) %>% 
      mutate(id = 1:n()) %>% 
      filter(id == 1) %>% 
      select(Provincia = provincia, Región = region, 
             Sector = sector, Movilidad=valor) %>% 
      formattable(
        align = c("l", "l", "l", "c"),
        list(
          Provincia = formatter("span", style = ~ style(font.style = "bold")),
          area(col = "Movilidad") ~ custom_color_tile(gris_claro, gris_oscuro)
          #area(col = "Movilidad") ~ color_text("white")
          #area(col = "Tasa") ~ color_tile("#f4e4f4", "#c8b1de")
        )
      )
  })
  
  
  #movilidad promedio sectores
  output$tabla_sectores_mayor <- formattable::renderFormattable({
  movilidad %>% 
    ungroup() %>% 
    filter(!is.na(provincia)) %>%  #filtrar regiones
    filter(fecha == max(fecha)) %>% #ultima fecha
    group_by(sector) %>% 
    summarize(valor = round(mean(valor, na.rm = T), 1)) %>% 
    arrange(desc(valor)) %>% 
    select(Sector = sector,
           Movilidad = valor) %>% 
    formattable(
      align = c("l", "c"),
      list(
        Sector = formatter("span", style = ~ style(font.weight = "bold")),
        area(col = "Movilidad") ~ custom_color_tile(gris_claro, gris_oscuro)
        #area(col = "Tasa") ~ color_tile("#f4e4f4", "#c8b1de")
      )
    )
  })
  
  #movilidad promedio por región
  output$tabla_regiones_mayor <- formattable::renderFormattable({
  movilidad %>% 
    ungroup() %>% 
    filter(!is.na(region)) %>%  #filtrar regiones
    filter(is.na(provincia)) %>%  #filtrar regiones
    filter(fecha == max(fecha)) %>% #ultima fecha
    group_by(region) %>% 
    summarize(valor = round(mean(valor, na.rm = T), 1)) %>% 
    arrange(desc(valor)) %>% 
      select(Región = region, 
             Movilidad = valor) %>% 
      formattable(
        align = c("l", "c"),
        list(
          #Región = formatter("span", style = ~ style(font.weight = "bold")),
          area(col = "Movilidad") ~ custom_color_tile(gris_claro, gris_oscuro)
          #area(col = "Tasa") ~ color_tile("#f4e4f4", "#c8b1de")
        )
      )
      
})
  
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
      btn_colors = "#1574D6",
      btn_labels = "Volver",
      closeOnClickOutside = TRUE,
      showCloseButton = TRUE,
      text = tags$span(
        tags$h3("Selector de años"),
        tags$br(),
        tags$h5("El selector de años le permite elegir el periodo temporal en que se grafique la pregunta seleccionada. El rango de años posible depende del marco temporal en que se aplicó la pregunta seleccionada en las encuestas. Por defecto, se grafican los datos a partir de la encuesta más reciente donde se haya aplicado la pregunta seleccionada, con una antiguedad de seis años.", ),
        tags$br(),
        img(src = "años.gif", width = "300", align = "center"),
        #div(id="video_anios", img(width = 300,
        #                          src = "Años_2.mp4")),
        tags$br(),
        tags$br(),
        tags$h5("Por ejemplo, si elige una pregunta que empezó a aplicarse el año 2000 y dejó de preguntarse el año 2015, se le ofrecerá automáticamente el rango mínimo de 2000 y máximo de 2015, y la selección por defecto irá desde 2015 hacia 8 años atrás (2007). El selector le permite modificar esta selección para extenderla y percibir la totalidad de los datos, o bien, acercarse para apreciar el detalle de la evolución temporal de los datos."),
        tags$br(),
      ),
      html = TRUE
    )
  })
  
})