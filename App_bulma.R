library(shiny) 
library(shinybulma) 
library(shinydashboardPlus)
library(shinycustomloader) 
library(shinyWidgets)
library(shinymanager)
library(tidyverse)
library(DT)
library(bs4Dash)

# UI ----------------------------------------------------------------------


ui <- bulmaPage(
  aos::use_aos(), 
  
  tags$head(tags$style(".shiny-output-error{visibility: hidden}")),
  tags$head(tags$style(".shiny-output-error:after{content: 'Error'; visibility: hidden}")),
  tags$head(tags$script('
                                var dimension = [0, 0];
                                $(document).on("shiny:connected", function(e) {
                                    dimension[0] = window.innerWidth;
                                    Shiny.onInputChange("dimension", dimension);
                                });
                                $(window).resize(function(e) {
                                    dimension[0] = window.innerWidth;
                                    Shiny.onInputChange("dimension", dimension);
                                });
                            ')),
    theme = "flatly",
  tags$head(
    tags$style(HTML("@import url('//fonts.googleapis.com/css?family=Open Sans:400,600');")),
    tags$style(HTML("@import url('//fonts.googleapis.com/css?family=Oswald:400,800');")),
    tags$style(".hero{background-color:#293c55!important;}"),
    tags$h1(tags$style("h1{font-family: Oswald; font-size: 17pt;  font-weight:500;}")),
    tags$h2(tags$style("h2{font-family: Oswald; color: #fad1de; font-size:10; font-weight:600; !important}")),
    tags$h3(tags$style("h3{font-family: Oswald; font-size: 15pt; font-weight:600;}")),
    tags$h4(tags$style("h4{font-family: Oswald; font-size: 12pt; font-weight:500;}")),
    tags$a(tags$style("a{font-family: Open Sans; font-weight:400, color: #df1a57; !important}")),
    tags$head(tags$style("* {font-family: Open Sans; font-weight:400}")),
   
  ),
  
  tags$style(type = "text/css", "
      .selectize-input {font-size: 10pt; color: white; !important}
      .selectize-input.input-active {font-size: 10pt; color: black; !important}
      .selectize-input.items {background-color: #0176de; border-color: #0176de; !important}
      .selectize-input.items:hover {background-color: #293c55; border-color: #0176de; !important}
      .selectize-control.single .selectize-input:after {border-color: white transparent transparent transparent; !important}
      "), 
  tags$style(type = "text/css", "
      .selectize-dropdown {font-size: 10pt; color: black; background-color: white; !important}
      .selectize-dropdown .active {color: white; background: #0176de !important;}
      "),
  
  bulmaNav(
    "Pruebas",
    bulmaSection(
      bulmaContainer(
        br(),
        tags$style(".topimg {
                            margin-left:-30px;
                            margin-right:-30px;
                            margin-top:-15px;
                          }"),
        div(class="topimg",img(src="https://midas.mat.uc.cl/bigdata/img/logofmuc.png",height= "40%", width="40%")),
        br(),
        bulmaTitle("Visualizador Google Mobility Report"),
        br(),
        bulmaTitle("Menú de selección", icon("table")) %>% 
          aos::aos(animation = "flip-left", duration = "2000"),
        br(),
        bulmaColumns(
          bulmaColumn(multiline = TRUE, 
                      sidebarPanel(selectInput("region",
                                                label = h4("Seleccione su región"),
                                                choices = regiones,
                                                width = "100%"
                      ),
                      
                      br(),
                      
                      selectInput("comuna",
                                  label = h4("Seleccione su comuna"),
                                  choices = NULL,
                                  width = "100%"
                      ),
                                   
                      br(),
                      
                      shinyWidgets::radioGroupButtons("selector_unidad_geo",
                                                      label = h4("Elegir nivel a graficar"),
                                                      choices = c("Región", "Provincia"),
                                                      selected = "Región",
                                                      justified = TRUE,
                                                      width = "100%"),
                      
                      br(),
                      
                      selectInput("sector",
                                  label = h4("Seleccione un sector"),
                                  multiple = TRUE,
                                  choices = sectores,
                                  selected = c("Viviendas", 
                                               "Lugares de trabajo",
                                               "Retail y recreación"),
                                                width = "100%"
                      ),
                      
                      br(),
                      
                      shinyWidgets::sliderTextInput(
                        inputId = "suavizar",
                        label = h4("Suavizar datos con la media móvil"), 
                        grid = F,
                        force_edges = TRUE,
                        width= "100%",
                        choices = c("No", "2 días", "3 días", "4 días", "5 días", 
                                    "6 días", "1 semana", "2 semanas")
                      ),
                      
                      br(),
                      
                      bulmaSwitchInput(
                        inputId = "fondo",
                        label = "Agregar cuarentenas", 
                        value = TRUE),

                      
                      br(),
                      
                      bulmaSwitchInput(
                        inputId = "covid",
                        label = "Agregar casos Covid-19"), 

                      br(),
                      
                      selectInput(
                        inputId = "mes_inicio",
                        label = h4("Mes inicial"), 
                        selected = lubridate::month(Sys.Date()) - 3,
                        width = "100%",
                        choice = meses[1:(lubridate::month(Sys.Date())-1)]),
                      
                      br(),
                      
                      selectInput(
                        inputId = "mes_fin",
                        label = h4("Mes final"), 
                        selected = lubridate::month(Sys.Date()),
                        width = "100%",
                        choices = meses[2:lubridate::month(Sys.Date())]),
                      
                      br(),                              
                                   
                  bulmaActionButton("go", label = h4("Generar gráfico"), 
                                    wrap = TRUE, "button is-link") %>% 
                                     aos::aos(animation = "zoom-in", duration = "2000"),
                                   
                                   width = 12
                      )),
          
          
          mainPanel(
            bulmaColumn(width = 12, h1(icon("chart-line"), "Tablas de resumen y gráficos") %>% 
                          aos::aos(animation = "zoom-in", duration = "2000"), 
                        
                        br(),
                        
                        textOutput("texto") %>% 
                          aos::aos(animation = "zoom-in", duration = "2000"), 
                        
                        fluidRow(
                          bulmaColumn(
                            h1("Instrucciones"),
                            br(),
                            p("Esta herramienta permite llevar a cabo el procesamiento de los datos de COVID-19 Community Mobility Report ."),
                            p("Utilice los botones presentados a continuación para seleccionar las variables que le interesa graficar."),
                            
                            HTML("<p>Los datos son obtenidos y procesados desde 
                    el sitio Google Mobility Report. 
               Para mayor información haga click aquí
                    <a href='https://www.google.com/covid19/mobility/' 
                       style='color: #999999'>
                    sitio web de Google Mobility Report</a></p>"),
                            hr(),
                          )
                        ) %>% 
                          aos::aos(animation = "flip-left", duration = "2000"),
                      
                        
                        bulmaContainer(
                          bulmaSteps(
                            size = NULL,
                            bulmaStepItem(
                              color = "success", completed = TRUE, active = FALSE,
                              bulmaStepDetail(
                                color = NULL, completed = FALSE, title = "Seleccione región y provincia", marker = 1,
                                "Seleccione la región y comuna a visualizar."
                              )
                            ),
                            bulmaStepItem(
                              color = "success", completed = TRUE, active = FALSE,
                              bulmaStepDetail(
                                color = NULL, completed = FALSE, title = "Seleccione sector", marker = 2,
                                "Seleccione variable de interés."
                              )
                            ),
                            bulmaStepItem(
                              color = "success", completed = TRUE, active = FALSE,
                              bulmaStepDetail(
                                color = NULL, completed = FALSE, title = "Generación de gráfico", marker = 3,
                                "Haga click en generar gráfico para procesar los datos."
                              )
                            )
                          )
                        ) %>% 
                          aos::aos(animation = "flip-left", duration = "2000"),
                        
                        br(),
                        
                        fluidRow(
                                 withLoader(infoBoxOutput("box_cuarentena"), 
                                 type = "html", loader = "loader1"),
                                 withLoader(infoBoxOutput("casos_activos"), 
                                            type = "html", loader = "loader1"),
                                 withLoader(infoBoxOutput("poblacion"), 
                                            type = "html", loader = "loader1")),
                      
                        withLoader(
                          plotOutput("grafico_cuarentenas"),
                                   type = "html",
                                   loader = "loader3"),
                        br(),
                        withLoader(
                          dataTableOutput("Tabla"),
                          type = "html",
                          loader = "loader3")
                        
            )
          )),
        
        fluidRow(
          column(12, align = "center",
                 hr(),
                 a("Plataforma desarrollada por DATA UC"), br(),
                 a(href = "https://www.mat.uc.cl", target = "blank",
                   "Facultad de Matemáticas - Pontificia Universidad Católica de Chile"),
                 HTML("<p>Diseño y metodología: 
                    <a href='https://www.mat.uc.cl' 
                       style='color: #999999'>
                    Alejandro Jara Vallejos, Alexis Alvear Leyton y Mauricio Castro Cepero</a></p>"),
                 HTML("<p>Desarrollo interfaz gráfica: 
                    <a href='https://www.mat.uc.cl' 
                       style='color: #999999'>
                     Sebastián Massa Slimming y Bastián Olea Herrera</a></p>"),
                 
                 tags$a(img(
                   src = "logo_data.png" ,
                   width = 200, style = "padding: 8px"
                 ),
                 href = "https://www.mat.uc.cl"
                 )
                 
                 ))
      )
    )
  )
)

# Server ------------------------------------------------------------------

server <- function(input, output, session) {
  
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
  provincia <- eventReactive(input$go, {
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
  cuarentenas <- eventReactive(input$go, {
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
  covid_activos_f <- eventReactive(input$go, {
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
  
  output$box_cuarentena <- renderValueBox({
    data <- cuarentenas() %>%
      dplyr::select(-c(codigo_region, codigo_comuna, comuna_residencia)) %>%
      dplyr::filter(fecha == max(fecha),
                    comuna == input$comuna) %>%
      dplyr::distinct(etapa)
    # 
    infoBox(title = "Fase Paso a Paso", value = data , icon = icon("virus-slash"),
      color = "purple", fill = TRUE)
    
  })
  
  output$casos_activos <- renderValueBox({
    data_activos <- covid_activos() %>%
      dplyr::filter(fecha == max(fecha),
                    comuna == input$comuna) %>%
      dplyr::select(casos)
    
    infoBox(title = "Casos activos", value = data_activos, icon = icon("head-side-virus"),
            color = "light-blue", fill = TRUE)
    
  })
  
  output$poblacion <- renderValueBox({
    poblacion <- covid_activos() %>%
      dplyr::select(!-c(poblacion, comuna)) %>% 
      dplyr::filter(comuna == input$comuna) %>% 
      dplyr::slice_head()
    
    infoBox(title = "Población", value = poblacion, icon = icon("user-check"),
            color = "olive", fill = TRUE)
    
  })
  
  #descargar datos covid ----
  covid_activos <- eventReactive(input$go, {
    
    readr::read_csv("https://coronavirus-api.mat.uc.cl/casos_activos_sintomas_comuna")
    
  })

  output$Tabla <- DT::renderDataTable({
    table <- covid_activos_f()
    datatable(table,
              extensions = c('Buttons', 'ColReorder', 'Responsive'), 
              options = list( 
                dom = "Blfrtip", 
                pageLength = 20, 
                colReorder = TRUE,
                searching = TRUE,
                lengthChange = FALSE, 
                rownames = FALSE, 
                scrollX = TRUE, 
                searchHighlight = TRUE,
                initComplete = JS(
                  "function(settings, json) {",
                  "$(this.api().table().header()).css({'background-color': '#FFFFFF', 'color': '#FFFFFF'});",
                  "}"
                ),
                language = list(
                  info = "Observaciones _START_ a _END_ de un total de _TOTAL_ observaciones",
                  paginate = list(previous = 'Anterior', `next` = 'Siguiente'),
                  lengthMenu = "Tabla de _MENU_ observaciones.",
                  search = "Buscar"),
                buttons = list(list(extend = "collection",
                                    buttons = c('pdf', 'csv', 'excel'),
                                    text = "Descargar tabla"
                ))),
              class = 'cell-border compact hover order-column', 
              filter = list(position = 'top'))
  })
  
}

# shinyApp ----------------------------------------------------------------

shinyApp(ui, server)
