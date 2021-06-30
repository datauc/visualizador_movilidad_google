#http://dataintelligence.cl/shiny/visualizador_movilidad_google

library(shiny)

shinyUI(fluidPage(

    includeCSS("estilos.css"), #estilos css
    titlePanel("Visualizador de movilidad"),
    
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
    

    #selectores ----
    fluidRow(
        column(6,
               selectInput("region",
                           label = "Seleccione su región",
                           choices = regiones,
                           width = "100%"
               )
        ),
        column(6,     
               selectInput("comuna",
                           label = "Seleccione su comuna",
                           choices = NULL,
                           width = "100%"
               )
        )
    ),
    
    #botonera ----
    fluidRow(
        column(12,
               shinyWidgets::radioGroupButtons("selector_unidad_geo",
                                               label = "Elegir nivel a graficar",
                                               choices = c("Región", "Provincia"),
                                               selected = "Región",
                                               justified = TRUE,
                                               width = "100%")
        ),
        
        column(12,
               shinyWidgets::pickerInput("sector",
                           label = "Seleccione un sector",
                           multiple = TRUE,
                           choices = sectores,
                           selected = c("Viviendas", 
                                        "Lugares de trabajo",
                                        "Retail y recreación"),
                           width = "100%"
               )
        ),
        
        column(6,
               shinyWidgets::sliderTextInput(
                   inputId = "suavizar",
                   label = "Suavizar datos con la media móvil", 
                   grid = F,
                   force_edges = TRUE,
                   width= "100%",
                   choices = c("No", "2 días", "3 días", "4 días", "5 días", "6 días", "1 semana", "2 semanas")
               )
        ),
        column(3,
               shinyWidgets::switchInput(
                   inputId = "fondo",
                   label = "Cuarentenas", 
                   value = TRUE,
                   #labelWidth = "120px", 
                   onLabel = "sí",
                   offLabel = "no",
                   size = "small",
                   #onStatus = "danger",
                   #offStatus = "info"
               )
        ),
        column(3,
               shinyWidgets::switchInput(
                   inputId = "covid",
                   label = "Casos Covid-19", 
                   value = FALSE,
                   #labelWidth = "120px", 
                   onLabel = "sí",
                   offLabel = "no",
                   size = "small",
                   #onStatus = "danger",
                   #offStatus = "info"
               )
        ),
    ),
    fluidRow(
        column(6,
               selectInput(
                   inputId = "mes_inicio",
                   label = "Mes incial", 
                   selected = lubridate::month(Sys.Date()) - 3,
                   width = "100%",
                   choice = meses[1:(lubridate::month(Sys.Date())-1)]
               )
        ),
        column(6,
               selectInput(
                   inputId = "mes_fin",
                   label = "Mes final", 
                   selected = lubridate::month(Sys.Date()),
                   width = "100%",
                   choices = meses[2:lubridate::month(Sys.Date())]
               )
        )
    ),
    
    #gráfico ----
    # fluidRow(
    #     column(12,
    #      plotOutput("grafico_basico")      
    #     )
    # ),
    fluidRow(
        column(12,
               plotOutput("grafico_cuarentenas")      
        )
    ),
    
    #datitos ----
    fluidRow(
        column(3,
               uiOutput("dato_mayor_aumento")
    ),
    column(3,
           uiOutput("dato_prueba")
    )
    )
    
))
