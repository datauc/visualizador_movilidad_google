library(shiny)

shinyUI(fluidPage(

    # Application title
    titlePanel("Visualizador de movilidad"),

    #selectores ----
    fluidRow(
        column(12,
               selectInput("region",
                           label = "Seleccione su región",
                           choices = regiones
               ),
               
               selectInput("comuna",
                           label = "Seleccione su comuna",
                           choices = NULL
               )
        )
    ),
    
    #obtonera ----
    fluidRow(
        column(12,
               shinyWidgets::radioGroupButtons("selector_unidad_geo",
                                               label = "Elegir nivel a graficar",
                                               choices = c("Región", "Provincia"),
                                               selected = "Región",
                                               justified = TRUE,
                                               width = "90%")
        ),
        
        column(12,
               shinyWidgets::pickerInput("sector",
                           label = "Seleccione un sector",
                           multiple = TRUE,
                           choices = sectores,
                           width = "90%"
               )
        ),
    ),
    
    #gráfico ----
    fluidRow(
        column(12,
         plotOutput("grafico_basico")      
        )
        
    )
))
