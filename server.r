library(shiny)
library(shinybulma)
library(formattable)

options(shiny.sanitize.errors = FALSE)

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
                      choices = comunas_filtradas,
                      selected = ifelse(input$region == "Metropolitana de Santiago", "Santiago", comunas_filtradas[1])
    )
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
      left_join(provincias_comunas %>% select(provincia, region, comuna, codigo_comuna), by = "codigo_comuna")
  })
  
  #—----
  
  #procesamiento ----
  
  #filtrar por región o provincia
  datos1 <- reactive({
    req(input$comuna != "",
        input$sector != "",
        length(input$sector) >= 1,
        movilidad,
        provincia())

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
    return(d1)
  })
    
    #filtrar sectores
    datos2 <- reactive({
      req(datos1())
    #filtrar sectores
    d2 <- datos1() %>% 
      filter(sector %in% input$sector)
    return(d2)
    })
    
    cuarentenas_cambios <- reactive({
      req(datos2())
    #minimos y maximos de fecha
    fecha_minima <- min(datos2()$fecha)
    fecha_maxima <- max(datos2()$fecha)
    
    #filtros de fecha
    #fecha_minima_f <- input$fecha[1] #lubridate::ymd(paste("2021", input$mes_inicio, "01"))
    #fecha_maxima_f <- input$fecha[2] #lubridate::ymd(paste("2021", input$mes_fin, "31"))
    
    #solo cambios
    cuarentenas_cambios <- cuarentenas() %>% 
      filter(comuna == input$comuna) %>% 
      select(etapa, etapa_n, fecha) %>% 
      filter(etapa != lag(etapa)) %>% 
      mutate(hasta = lead(fecha),
             hasta = replace(hasta, is.na(hasta), fecha_maxima))
    return(cuarentenas_cambios)
    })
    
    #media movil ----
    datos3 <- reactive({
      req(datos2())
    if (input$suavizar == "No") {
      d3 <- datos2()
    } else if (input$suavizar != "No") {
      if (input$suavizar == "1 semana") {
        dias <- 7
      } else if (input$suavizar == "2 semanas") {
        dias <- 14
      } else if (input$suavizar == "3 semanas") {
        dias <- 21
      } else {
        dias <- readr::parse_number(input$suavizar)
      }
      #suavizar
      d3 <- datos2() %>%
        group_by(sector) %>%
        mutate(valor = zoo::rollmean(valor, k = dias, 
                                     fill = "extend"))
    }
      return(d3)
    })
    
    
    #—----
    
    #graficar ----
    output$grafico_cuarentenas <- renderPlot({
    p <- datos3() %>% 
      ggplot()
    
    #fondo de cuarentenas ----
    if (input$fondo == TRUE) {
      p <- p +
        #fondo
        geom_rect(data = cuarentenas_cambios(), aes(fill = etapa,
                                                  xmin = fecha, xmax = hasta,
                                                  ymin = -Inf, ymax = Inf),
                  alpha = 0.1)
    }
    
    #gráfico base
    p <- p +
      geom_hline(yintercept = 0, size = 0.4, alpha = 0.7) +
      geom_hline(yintercept = 50, size = 0.3, alpha = 0.4, linetype = "dashed") +
      geom_hline(yintercept = -50, size = 0.3, alpha = 0.4, linetype = "dashed") +
      #lineas
      geom_line(aes(fecha, valor, col = sector), size = 1, show.legend = F) +
      geom_point(aes(fecha, valor, col = sector), size = 0, alpha = 0) +
      #limites horizontales
      coord_cartesian(xlim = c(input$fecha[1], input$fecha[2])) + 
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
      scale_color_manual(values = c("#dc0073", "#008bf8", "#ff4e00",
                                    "#6a4c93", "#04e762", "#f5b700")) +
      labs(y = "Cambio porcentual respecto a línea de base") +
      theme_minimal() +
      theme(axis.title.x = element_blank(),
            legend.title = element_blank(),
            panel.grid.minor.x = element_blank(),
            #legend.position = c(.8, .15))
            legend.position = "bottom") +
      theme(text = element_text(family = "Open Sans", color = gris_oscuro),
            plot.subtitle = element_text(family = "Oswald", size = 14),
            plot.title = element_text(family = "Oswald"),
            axis.title.y = element_text(family = "Open Sans", size = 10),
            axis.text.y = element_text(family = "Oswald")) +
      guides(fill = guide_legend(override.aes = list(size = 3, alpha=0.2), nrow = 2)) +
      guides(col = guide_legend(override.aes = list(size = 5, alpha=1, fill=NA, text=NA), nrow = 3))
    
    #poner subtítulo de región o provincia ----
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
  
  #casos covid activos
  output$dato_covid_activos <- renderUI({
    dato <- covid_activos_f() %>% 
      arrange(fecha) %>% 
      mutate(ayer = lag(casos)) %>% 
      filter(fecha == max(fecha)) %>% 
      slice(1)
    
    bloque_datos(titulo = "Casos Covid-19 activos",
                 cambio = dato$casos,
                 hoy = dato$casos,
                 ayer = dato$ayer,
                 provincia = paste("Casos activos al", format(dato$fecha, "%d de %B")),
                 sector = dato$comuna,
                 region = dato$region,
                 texto = "casos")
  })
  
  output$dato_covid_peak <- renderUI({
    casos_hoy <- covid_activos_f() %>% 
      filter(fecha == max(fecha)) %>% 
      slice(1)
    
    dato <- covid_activos_f() %>% 
      arrange(desc(casos)) %>% 
      slice(1)
    
    bloque_datos(titulo = "Peak de casos activos",
                 cambio = dato$casos,
                 hoy = dato$casos,
                 ayer = casos_hoy$casos,
                 provincia = paste("Peak de casos:", format(dato$fecha, "%d de %B, %Y")),
                 sector = dato$comuna,
                 region = dato$region,
                 texto = "casos")
  })
  
  output$dato_covid_anti_peak <- renderUI({
    casos_hoy <- covid_activos_f() %>% 
      filter(fecha == max(fecha)) %>% 
      slice(1)
    
    dato <- covid_activos_f() %>%
      filter(lubridate::year(fecha) == lubridate::year(Sys.Date())) %>% 
      arrange(casos) %>% 
      slice(1)
    
    bloque_datos(titulo = "Día con menos casos activos este año",
                 cambio = dato$casos,
                 hoy = dato$casos,
                 ayer = casos_hoy$casos,
                 provincia = paste("Día con menores casos:", format(dato$fecha, "%d de %B, %Y")),
                 sector = dato$comuna,
                 region = dato$region,
                 texto = "casos")
  })
  
  #fase cuarentena
  output$dato_fase_cuarentena <- renderUI({
    req(input$comuna != "")
    
    dato <- cuarentenas() %>%
      #select(-c(codigo_region, codigo_comuna, comuna_residencia)) %>%
      filter(fecha == max(fecha),
             comuna == input$comuna)
    

    bloque_datos(titulo = "Plan Paso a Paso",
                 cambio = dato$etapa_n,
                 hoy = 0,
                 ayer = 0,
                 provincia = paste("Fase:", dato$etapa),
                 sector = dato$comuna,
                 region = dato$region,
                 texto = "",
                 prefijo = "fase")
  })
  
  

  #casos covid ----
  #descargar datos covid 
  covid_activos <- reactive({
    #req(input$covid == TRUE)
    readr::read_csv("https://coronavirus-api.mat.uc.cl/casos_activos_sintomas_comuna",
                    col_types = readr::cols())
  })
  
  #filtrar casos covid por comuna seleccionada
  covid_activos_f <- reactive({
    req(input$comuna != "")
    #req(input$covid == TRUE)
    
    #filtrar y dar formato
    d <- covid_activos() %>% 
      select(-comuna) %>% 
      #asegurarse que calce la comuna
      mutate(codigo_comuna = as.numeric(codigo_comuna)) %>% 
      left_join(cuarentenas() %>% 
                  select(comuna, codigo_comuna), 
                by = "codigo_comuna") %>% 
      filter(comuna == input$comuna) %>% 
      select(-codigo_region, -poblacion)
    return(d)
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
      btn_colors = gris_oscuro,
      btn_labels = "Volver",
      closeOnClickOutside = TRUE,
      showCloseButton = TRUE,
      text = list(
        h2("De donde se extraen los datos?"),
        p("Los datos del visualizador son extraídos directamente de los archivos CSV que están en el sitio web de Google COVID-19 Community Mobility Report. Estos muestran las tendencias de cómo va variando los números de visitas a determinados lugares y el tiempo que dura en comparación a un valor referencial. En este caso, los datos fueron filtrados exclusivamente para Chile."),
        hr(),
        h2("¿Qué lugares considera el informe?"),
        p("Básicamente, los lugares o categorías que toma en consideración son: supermercados y farmacias (movilidad en lugares como supermercado, almacenes, tiendas de comida, etc), parques (parques locales, nacionales, playas, plazas, jardínes públicos, etc) estaciones de transporte (por ejemplo, movilidad en metro, tren o bus) tiendas y ocio (movilidad en cafeterías, centros comerciales, restaurantes, cines, bibliotecas, parque de diversiones, etc), zonas residenciales y lugares de trabajo (tendencias de movilidad en los lugares de trabajo)."),
        hr(),
        h2("¿Cómo interpretar el indicador?"),
        ("El informe permite identificar en qué medida, las personas de Chile han aumentado o reducido sus visitas o movilidad en las categorías (lugares) que se mencionan anteriormente. Al momento del despliegue, si el porcentaje de variación es negativo, indica que la movilidad ha disminuido. Por su parte, si el porcentaje es positivo, implica que la movilidad de las personas ha aumentado. Para mayor información sobre cómo interpretar patrones comunes de los datos, visite el sitio web: https://support.google.com/covid19-mobility/answer/9825414?hl=es-419"),
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