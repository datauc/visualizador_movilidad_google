#http://dataintelligence.cl/shiny/visualizador_movilidad_google

library(shiny)
library(shinybulma)

shinyUI(fluidPage(
  
  
  #tags$head(tags$style(".shiny-output-error{visibility: hidden}")),
  #tags$head(tags$style(".shiny-output-error:after{content: 'Error'; visibility: hidden}")),
  theme = "flatly",
  
  aos::use_aos(), 
  includeCSS("estilos.css"), #estilos css
  
  
  # tags$head(
  #   # tags$style(HTML("@import url('//fonts.googleapis.com/css?family=Open Sans:400,600');")),
  #   # tags$style(HTML("@import url('//fonts.googleapis.com/css?family=Oswald:300, 400,800');")),
  #   tags$style(".hero{background-color:#293c55!important;}"),
  #   tags$h1(tags$style("h1{font-family: Oswald; font-size: 26pt; font-weight:300;}")),
  #   tags$h2(tags$style("h2{font-family: Oswald; font-size: 17pt; font-weight:600; !important}")),
  #   tags$h3(tags$style("h3{font-family: Oswald; font-size: 15pt; font-weight:600;}")),
  #   tags$h4(tags$style("h4{font-family: Oswald; font-size: 12pt; font-weight:500;}")),
  #   #links
  #   tags$a(tags$style("a{font-family: Open Sans; font-weight:400, color: #df1a57; !important}")),
  #   tags$head(tags$style("* {font-family: Open Sans; font-weight:400}")),
  #   
  # ),
  
  # tags$style(type = "text/css", "
  #     .selectize-input {font-size: 10pt; color: white; !important}
  #     .selectize-input.input-active {font-size: 10pt; color: black; !important}
  #     .selectize-input.items {background-color: #0176de; border-color: #0176de; !important}
  #     .selectize-input.items:hover {background-color: #293c55; border-color: #293c55; !important}
  #     .selectize-control.single .selectize-input:after {border-color: white transparent transparent transparent; !important}
  #     "), 
  # tags$style(type = "text/css", "
  #     .selectize-dropdown {font-size: 10pt; color: black; background-color: white; !important}
  #     .selectize-dropdown .active {color: white; background: #0176de !important;}
  #     "),
  
  #bulmaNav(
  #  "Pruebas",
  #  bulmaSection(
  #    bulmaContainer(
  # br(),
  # tags$style(".topimg {
  #                     margin-left:-30px;
  #                     margin-right:-30px;
  #                     margin-top:-15px;
  #                   }"),
  # div(class="topimg",img(src="https://midas.mat.uc.cl/bigdata/img/logofmuc.png",height= "40%", width="40%")),
  #      br(),
  
  
  fluidRow(
    column(12,
           h1("Visualizador Google Mobility Report") %>% 
             aos::aos(animation = "zoom-in", duration = "2000"),
           br(),
           h2("Menú de selección", icon("table")) %>% 
             aos::aos(animation = "zoom-in", duration = "600"),
           br()
    )
  ),
  
  #sidebar ----
  fluidRow(
    column(4,
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
           
           fluidRow(
             column(6,
                    selectInput(
                      inputId = "mes_inicio",
                      label = h4("Mes inicial"), 
                      selected = lubridate::month(Sys.Date()) - 3,
                      width = "100%",
                      choice = meses[1:(lubridate::month(Sys.Date())-1)]),
             ),
             column(6,
                    selectInput(
                      inputId = "mes_fin",
                      label = h4("Mes final"), 
                      selected = lubridate::month(Sys.Date()),
                      width = "100%",
                      choices = meses[2:lubridate::month(Sys.Date())]),
             )
           ),
           
           column(12, align="right",
             actionLink(
               inputId = "ayuda",
               style = "align: right !important; font-size: 120%; color: #5f7181;",
               #class = "botonayuda",
               label = NULL, icon = icon("question-circle")
             )
           )
           
    ),
    
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
                    h1("Instrucciones") %>% 
                      aos::aos(animation = "zoom-in", duration = "600"),
                    br(),
                    
                    div(
                      p("Esta herramienta permite llevar a cabo el procesamiento de los datos de COVID-19 Community Mobility Report ."),
                      p("Utilice los botones presentados a continuación para seleccionar las variables que le interesa graficar."),
                      
                      HTML("<p>Los datos son obtenidos y procesados desde 
                    el sitio Google Mobility Report. 
               Para mayor información haga click aquí
                    <a href='https://www.google.com/covid19/mobility/' 
                       style='color: #999999'>
                    sitio web de Google Mobility Report</a></p>"),
               hr(),
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
           
           
           #fluidRow(
           # shinycustomloader::withLoader(
           #   infoBoxOutput("box_cuarentena"), 
           #   type = "html", loader = "loader1"),
           # shinycustomloader::withLoader(
           #   infoBoxOutput("casos_activos"), 
           #   type = "html", loader = "loader1"),
           # shinycustomloader::withLoader(
           #   infoBoxOutput("poblacion"), 
           #   type = "html", loader = "loader1")),
           
           #grafico ----
           fluidRow(
             column(12,
                    plotOutput("grafico_cuarentenas") %>% 
                      shinycustomloader::withLoader(type = "html", loader = "loader3"),
                    br(),
                    
                    # shinycustomloader::withLoader(
                    #   dataTableOutput("Tabla"),
                    #   type = "html",
                    #   loader = "loader3")
             )
           ),
           
           
           
           #datitos ----
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
             )
           ),
           fluidRow(
             column(3,
                    uiOutput("dato_mayor_aumento_sector")
             ),
             column(3,
                    uiOutput("dato_mayor_reduccion_sector")
             ),
             hr(),
           ),
           
           
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
           
    ),
    
    
    #footer ----
    
    fluidRow(
      column(12, align = "center",
             hr(),
             em("Plataforma desarrollada por DATA UC"), 
             br(),
             a(href = "https://www.mat.uc.cl", target = "blank", 
               style = "color: #5f7181",
               "Facultad de Matemáticas - Pontificia Universidad Católica de Chile"),
             HTML("<p>Diseño y metodología: 
                    <a href='https://www.mat.uc.cl' 
                       style='color: #5f7181'>
                    Alejandro Jara Vallejos, Alexis Alvear Leyton y Mauricio Castro Cepero</a></p>"),
             HTML("<p>Desarrollo interfaz gráfica: 
                    <a href='https://www.mat.uc.cl' 
                       style='color: #5f7181'>
                     Sebastián Massa Slimming y Bastián Olea Herrera</a></p>"),
             
             tags$a(img(
               src = "logodatauc.png" ,
               width = 200, style = "padding: 8px"
             ),
             href = "https://www.mat.uc.cl"
             )
             
      )
    )
  )
)
)