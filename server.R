library(shiny)

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
    provincia <- reactive({
        req(input$comuna != "")
        d <- provincias_comunas %>% 
            #filter(comuna == "Puente Alto") %>% 
            filter(comuna == input$comuna) %>% 
            mutate(provincia = as.character(provincia)) %>% 
            select(provincia) %>% 
            pull()
        return(d)
    })
    
    
    # #grafico ----
    # output$grafico_basico <- renderPlot({
    #     req(input$comuna != "",
    #         input$sector != "",
    #         length(input$sector) >= 1,
    #         movilidad,
    #         provincia())
    #         
    #     #filtrar por región o provincia
    #     if (input$selector_unidad_geo == "Región") {
    #      data <- movilidad %>% 
    #          filter(region == input$region) %>% 
    #          rename(unidad = region)
    #      
    #     } else if (input$selector_unidad_geo == "Provincia") {
    #         data <- movilidad %>% 
    #             filter(provincia == provincia()) %>% 
    #             rename(unidad = provincia)
    #     }
    #     
    #     
    #     #gráfico base
    #     p <- data %>% 
    #         #filtrar sectores
    #         filter(sector %in% input$sector) %>% 
    #         ggplot() +
    #         geom_line(aes(fecha, valor, col = sector)) +
    #         scale_x_date(date_breaks = "months", date_labels = "%b") +
    #         labs(y = "Cambio porcentual respecto a línea de base") +
    #         theme(axis.title.x = element_blank(),
    #               legend.title = element_blank(),
    #               panel.grid.minor.x = element_blank(),
    #               legend.position = "bottom")
    #     
    #     #poner subtítulo de región o provincia
    #     if (input$selector_unidad_geo == "Región") {
    #         p <- p +
    #             labs(subtitle = paste("Región:", input$region))
    #     } else if (input$selector_unidad_geo == "Provincia") {
    #         p <- p +
    #             labs(subtitle = paste("Provincia:", provincia()))
    #         
    #     }
    #     return(p)
    # })
    
    #cuarentenas ----
    #importar cuarentenas desde el repositorio del ministerio de ciencias
    cuarentenas <- readr::read_csv("https://github.com/MinCiencia/Datos-COVID19/raw/master/output/producto74/paso_a_paso.csv",
                                   col_types = readr::cols()) %>% 
        tidyr::pivot_longer(starts_with("20"), names_to = "fecha", values_to = "etapa_n") %>% 
        mutate(etapa = case_when(etapa_n == 1 ~ "Cuarentena",
                                 etapa_n == 2 ~ "Transición",
                                 etapa_n == 3 ~ "Preparación",
                                 etapa_n == 4 ~ "Apertura inicial",
                                 etapa_n == 5 ~ "Apurtura avanzada")) %>% 
        mutate(etapa = forcats::fct_reorder(etapa, etapa_n)) %>% 
        mutate(fecha = lubridate::ymd(fecha)) %>% 
        #anexar provincias y comunas a partir del codigo de comuna
        left_join(provincias_comunas %>% select(provincia, comuna, codigo_comuna), by = "codigo_comuna")
    
    
    
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
        
        # #limitar el mes máximo
        # fecha_maxima_f <- ifelse(input$mes_fin > lubridate::month(Sys.Date()),
        #                          lubridate::month(Sys.Date()),
        #                          input$mes_fin)
        
        
        #solo cambios
        cuarentenas_cambios <- cuarentenas %>% 
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
        
        p <- p +
            geom_hline(yintercept = 0, size = 0.4, alpha=0.7) +
            geom_hline(yintercept = 50, size = 0.3, alpha=0.4, linetype = "dashed") +
            geom_hline(yintercept = -50, size = 0.3, alpha=0.4, linetype = "dashed") +
            #lineas
            geom_line(aes(fecha, valor, col = sector), show.legend = F) +
            geom_point(aes(fecha, valor, col = sector), size = 0, alpha = 0) +
            #limites horizontales
            coord_cartesian(xlim = c(fecha_minima_f, fecha_maxima_f)) + 
            scale_x_date(date_breaks = "months", date_labels = "%b", 
                         expand = expansion(mult = c(0,0))) +
            scale_y_continuous(labels = function (x) paste0(x, "%"), 
                               breaks = c(-75, -50, -25, 0, 25, 50, 75)) +
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

})
