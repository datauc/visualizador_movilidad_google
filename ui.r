#http://dataintelligence.cl/shiny/visualizador_movilidad_google

library(shiny)
library(shinybulma)
library(lubridate)

shinyUI(fluidPage(title = "Visualizador de movilidad de Google", lang = "es",
  
  #ocultar errores
  #tags$head(tags$style(".shiny-output-error{visibility: hidden}")),
  #tags$head(tags$style(".shiny-output-error:after{content: 'Error'; visibility: hidden}")),
  theme = "flatly",
  aos::use_aos(), 
  includeCSS("estilos.css"), #estilos css
  
  fluidRow(
    column(12,
           h1("Visualizador de movilidad de Google") %>% 
             aos::aos(animation = "zoom-in", duration = "1000"),
           br(),
           p("Desarrollado por Data UC, Universidad Católica de Chile",
             style = "color: #5f7181 !important; font-family: Oswald; font-style: italic;") %>% 
             aos::aos(animation = "zoom-in", duration = "1000", delay = "100"),
           
           #br()
    )
  ),
  
  #sidebar ----
  fluidRow(
    column(4,
           h2("Menú de selección", icon("table")) %>% 
             aos::aos(animation = "zoom-in", duration = "600"),
           #bulmaColumns(
           # bulmaColumn(multiline = TRUE, 
           #            sidebarPanel(
           #selectores ----
           selectInput("region",
                       label = h4("Seleccione su región"),
                       choices = regiones,
                       width = "100%"
           ),
           
           #br(),
           
           selectInput("comuna",
                       label = h4("Seleccione su comuna"),
                       choices = NULL,
                       width = "100%"
           ),
           
           #br(),
           
           shinyWidgets::radioGroupButtons("selector_unidad_geo",
                                           label = h4("Elegir nivel a graficar"),
                                           choices = c("Región", "Provincia"),
                                           selected = "Región",
                                           justified = TRUE,
                                           width = "100%"),
           
           #br(),
           
           selectInput("sector",
                       label = h4("Seleccione un sector"),
                       multiple = TRUE,
                       choices = sectores,
                       selected = c("Viviendas", 
                                    "Lugares de trabajo",
                                    "Retail y recreación"),
                       width = "100%"
           ),
           
           #br(),
           
           # shinyWidgets::sliderTextInput(
           #   inputId = "suavizar",
           #   label = h4("Suavizar datos con la media móvil"), 
           #   grid = F,
           #   force_edges = TRUE,
           #   width= "100%",
           #   choices = c("No", "2 días", "3 días", "4 días", "5 días", 
           #               "6 días", "1 semana", "2 semanas")
           # ),
           
           selectInput(
             inputId = "suavizar",
             label = h4("Suavizar datos con la media móvil"),
             #grid = F,
             #force_edges = TRUE,
             width= "100%",
             choices = c("No", "2 días", "3 días", "4 días", "5 días",
                         "6 días", "1 semana", "2 semanas")
           ),
           
           #br(),
           
           fluidRow(
             br(),
             column(6,
                    
                    shinyWidgets::prettySwitch(
                      inputId = "fondo",
                      label = "Cuarentenas", 
                      value = TRUE),
             ),
             column(6,
                    shinyWidgets::prettySwitch(
                      inputId = "covid",
                      label = "Casos Covid-19"), 
             )
           ),
           #br(),
           
           # fluidRow(
           #   column(6,
           #          selectInput(
           #            inputId = "mes_inicio",
           #            label = h4("Mes inicial"),
           #            selected = lubridate::month(Sys.Date()) - 3,
           #            width = "100%",
           #            choice = meses[1:(lubridate::month(Sys.Date())-1)]),
           #   ),
           #   column(6,
           #          selectInput(
           #            inputId = "mes_fin",
           #            label = h4("Mes final"),
           #            selected = lubridate::month(Sys.Date()),
           #            width = "100%",
           #            choices = meses[2:lubridate::month(Sys.Date())]),
           #   )
           # ),
           
           #column(12,
                  
           #),
           
           column(12, align="right",
             actionLink(
               inputId = "ayuda",
               style = "align: right !important; font-size: 130%; color: #5f7181;",
               #class = "botonayuda",
               label = NULL, icon = icon("question-circle")
             )
           )
           
    ), #%>% 
      #animación sidebar
      #aos::aos(animation = "zoom-in", duration = "1000", delay = "100"),
    
    #body ----
    column(8,
           
           fluidRow(
             column(12,
                    h2(icon("chart-line"), "Tablas de resumen y gráficos") %>% 
                      aos::aos(animation = "zoom-in", duration = "600"), 
                    
                    br(),
                    
                    # textOutput("texto") %>% 
                    #   aos::aos(animation = "zoom-in", duration = "600"), 
                    
                    #fluidRow(
                    #bulmaColumn(
                    # h1("Instrucciones") %>% 
                    #   aos::aos(animation = "zoom-in", duration = "600"),
                    # br(),
                    
                    div(
                      HTML("Esta herramienta permite visualizar los datos del <em>COVID-19 Community Mobility Report</em> desarrollado por Google. Utilice los botones presentados a continuación para seleccionar las variables que le interesa graficar."),
                      
                      HTML("<p>Los datos son obtenidos y procesados desde 
                    el sitio <em>Google Mobility Report</em>. 
               Para mayor información y datos sobre metodología, acceda al
                    <a href='https://www.google.com/covid19/mobility/' 
                       style='color: #999999'>
                    sitio web del Google Mobility Report</a></p>"),
                    ) %>% 
                 aos::aos(animation = "zoom-in", duration = "600")
             )
           ),
           
           # bulmaContainer(
           #   bulmaSteps(
           #     size = NULL,
           #     bulmaStepItem(
           #       color = "success", completed = TRUE, active = FALSE,
           #       bulmaStepDetail(
           #         color = NULL, completed = FALSE, title = "Seleccione región y provincia", marker = 1,
           #         "Seleccione la región y comuna a visualizar."
           #       )
           #     ),
           #     bulmaStepItem(
           #       color = "success", completed = TRUE, active = FALSE,
           #       bulmaStepDetail(
           #         color = NULL, completed = FALSE, title = "Seleccione sector", marker = 2,
           #         "Seleccione variable de interés."
           #       )
           #     ),
           #     bulmaStepItem(
           #       color = "success", completed = TRUE, active = FALSE,
           #       bulmaStepDetail(
           #         color = NULL, completed = FALSE, title = "Generación de gráfico", marker = 3,
           #         "Haga click en generar gráfico para procesar los datos."
           #       )
           #     )
           #   )
           #) %>% 
           #   aos::aos(animation = "zoom-in", duration = "600"),
           
           
           #datos 1 ----
           
           fluidRow(
             hr(),
             column(3,
                    uiOutput("dato_mayor_aumento")
             ),
             column(3,
                    uiOutput("dato_mayor_reduccion")
             ),
             column(3,
                    uiOutput("dato_menor_movilidad")
             ),
             column(3,
                    uiOutput("dato_mayor_movilidad")
             ),
             column(12,
                    br(),
             hr(),
             )
           ),
           
           #grafico ----
           fluidRow(
             column(12,
                    plotOutput("grafico_cuarentenas") %>% 
                      shinycssloaders::withSpinner(),
                      #shinycustomloader::withLoader(type = "html", loader = "loader3"),
                    br(),
                    dateRangeInput(
                      inputId = "fecha",
                      label = "Seleccionar rango de fechas",
                      min = "2020-02-15", #ymd
                      max = Sys.Date(),
                      start = Sys.Date() - months(3),
                      end = Sys.Date(),
                      format = "dd-mm-yyyy",
                      startview = "month",
                      weekstart = 0,
                      language = "es",
                      separator = " hasta ",
                      width = "100%",
                      autoclose = TRUE
                    ),
                    
                    # shinycustomloader::withLoader(
                    #   dataTableOutput("Tabla"),
                    #   type = "html",
                    #   loader = "loader3")
             )
           ),
           
           
           
           #datitos 2 ----
           
           #fila 2
           fluidRow(
             hr(),
             column(3,
                    uiOutput("dato_covid_activos")
             ),
             column(3,
                    uiOutput("dato_covid_peak")
             ),
             column(3,
                    uiOutput("dato_covid_anti_peak")
             ),
             column(3,
                    uiOutput("dato_fase_cuarentena")
             ),
             
             column(12,
                    br(),
                    hr(),
             )
           ),
           # fluidRow(
           #   column(3,
           #          uiOutput("dato_mayor_aumento_sector")
           #   ),
           #   column(3,
           #          uiOutput("dato_mayor_reduccion_sector")
           #   ),
           #   hr(),
           # ),
           
           
           #tablas ----
           
           fluidRow(
             column(12,
                    h3("Provincias con mayor movilidad por sector"),
                    formattable::formattableOutput("tabla_mayor_movilidad")
             ),
             column(6,
                    h3("Sectores con mayor movilidad promedio"),
                    formattable::formattableOutput("tabla_sectores_mayor")
             ),
             column(6,
                    h3("Movilidad promedio de las regiones"),
                    formattable::formattableOutput("tabla_regiones_mayor")
             ),
           )
           
    ) %>% 
      #animación body
      aos::aos(animation = "zoom-in", duration = "1000", delay = "200"),
    
    
    #footer ----
    
    fluidRow(
      column(12, align = "center",
             hr(),
             em("Plataforma desarrollada por Data UC usando R y Shiny"), 
             br(),
             a(href = "https://www.mat.uc.cl", target = "blank", 
               style = "color: #5f7181",
               "Facultad de Matemáticas - Pontificia Universidad Católica de Chile"),
             HTML("<p>Desarrollo: 
                    <a href='https://www.mat.uc.cl' 
                       style='color: #5f7181'>
                     Sebastián Massa Slimming y Bastián Olea Herrera</a></p>"),
             HTML("<p>Diseño y metodología: 
                    <a href='https://www.mat.uc.cl' 
                       style='color: #5f7181'>
                    Alejandro Jara Vallejos, Alexis Alvear Leyton y Mauricio Castro Cepero</a></p>"),
             
             tags$a(img(
               src = "logodatauc.png" ,
               width = 200, style = "padding: 8px"
             ),
             href = "https://www.mat.uc.cl"
             )
             
      )
    ) %>% 
      #animación footer
      aos::aos(animation = "zoom-in", duration = "1000", delay = "300"),
  )
)
)