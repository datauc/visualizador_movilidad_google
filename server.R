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
    
    output$grafico_basico <- renderPlot({
        req(input$comuna != "",
            input$sector != "",
            length(input$sector) >= 1,
            movilidad,
            provincia())
            
        #filtrar por región o provincia
        if (input$selector_unidad_geo == "Región") {
         data <- movilidad %>% 
             filter(region == input$region) %>% 
             rename(unidad = region)
         
        } else if (input$selector_unidad_geo == "Provincia") {
            data <- movilidad %>% 
                filter(provincia == provincia()) %>% 
                rename(unidad = provincia)
        }
        
        
        #gráfico base
        p <- data %>% 
            #filtrar sectores
            filter(sector %in% input$sector) %>% 
            ggplot() +
            geom_line(aes(fecha, valor, col = sector)) +
            scale_x_date(date_breaks = "months", date_labels = "%b") +
            labs(y = "Cambio porcentual respecto a línea de base") +
            theme(axis.title.x = element_blank(),
                  legend.title = element_blank(),
                  panel.grid.minor.x = element_blank(),
                  legend.position = "bottom")
        
        #poner subtítulo de región o provincia
        if (input$selector_unidad_geo == "Región") {
            p <- p +
                labs(subtitle = paste("Región:", input$region))
        } else if (input$selector_unidad_geo == "Provincia") {
            p <- p +
                labs(subtitle = paste("Provincia:", provincia()))
            
        }
        return(p)
    })

})
