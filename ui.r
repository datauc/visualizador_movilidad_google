#http://dataintelligence.cl/shiny/visualizador_movilidad_google

library(shiny)
library(lubridate)
library(ggiraph)

shinyUI(fluidPage(title = "Visualizador de movilidad de Google", lang = "es",
                  
                  #ocultar errores
                  #tags$head(tags$style(".shiny-output-error{visibility: hidden}")),
                  #tags$head(tags$style(".shiny-output-error:after{content: 'Error'; visibility: hidden}")),
                  theme = "flatly",
                  aos::use_aos(), 
                  includeCSS("estilos.css"), #estilos css
                  
                  #detectar tamaño de la ventana
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
                            ')), #input$dimension[1]
                  
                  #header ----
                  fluidRow(
                    column(12,
                           style = paste0("background-color:", celeste, ";"), 
                           
                           #div(style = "margin-top: 10px;
                           #              margin-bottom: -40px;",
                           column(2, align="center",
                                  img(src="Logo-MIDAS blanco.png", width=200,
                                      style = "padding: 30px; 
                                             padding-left: 10px;
                                             padding-right: 10px;
                                             margin-top: 10px")
                           ),
                           column(2, align="center",
                                  img(src="logo-datauc-blanco.svg", width=150,
                                      style = "padding: 30px;
                                                padding-left: 10px;
                                                padding-right: 10px;
                                                margin-top: 0px")
                                  #)
                           ),
                           column(8,
                                  h1("Visualizador de movilidad",
                                     style = "padding:10px; margin-left: 10px;  color: white;"),
                                  p("Datos de Google y situación de cuarentenas",
                                    br(),
                                    style = "padding:10px; margin-left: 10px; margin-top: -20px; color: white; font-family: Roboto; font-style: italic;")
                           )
                    ),
                    fluidRow(
                      br(), p(" "), br()
                    )
                  ),
                  
                  #sidebar ----
                  fluidRow(
                    br(),
                    column(4,
                           #h2("Menú de selección", icon("table")), #%>% 
                           #aos::aos(animation = "zoom-in", duration = "600"),
                           
                           #selectores ----
                           shinyWidgets::pickerInput("sector",
                                                     label = h4("Categoría de lugar"),
                                                     multiple = TRUE,
                                                     #selectize = F,
                                                     choices = sectores,
                                                     selected = c("Lugares de trabajo"),
                                                     width = "100%"
                           ),
                           
                           selectInput("region",
                                       label = h4("Región"),
                                       choices = regiones,
                                       width = "100%"
                           ),
                           
                           selectInput("provincia",
                                       label = h4("Provincia"),
                                       choices = NULL,
                                       width = "100%"
                           ),
                           
                           dateRangeInput(
                             inputId = "fecha",
                             label = h4("Rango de fechas"),
                             min = "2020-02-15", #ymd
                             max = max(movilidad$fecha, na.rm = T),#Sys.Date(),
                             start = Sys.Date() - months(3),
                             end = max(movilidad$fecha, na.rm = T),##Sys.Date(),
                             format = "dd-mm-yyyy",
                             startview = "month",
                             weekstart = 0,
                             language = "es",
                             separator = " hasta ",
                             width = "100%",
                             autoclose = TRUE
                           ),
                           
                           
                           selectInput(
                             inputId = "suavizar",
                             label = h4("Suavizar datos usando media móvil"),
                             #grid = F,
                             #force_edges = TRUE,
                             width= "100%",
                             choices = c("No", "2 días", "3 días", "4 días", "5 días",
                                         "6 días", "1 semana", "2 semanas", "3 semanas")
                           ),
                           
                           #br(),
                           
                           # shinyWidgets::prettySwitch(
                           #   inputId = "fondo",
                           #   label = "Cuarentenas", 
                           #   value = TRUE),
                           # shinyWidgets::prettySwitch(
                           #   inputId = "covid",
                           #   label = "Casos Covid-19"), 
                           br(),
                           
                           shinyWidgets::checkboxGroupButtons(
                             inputId = "covid",
                             label= NULL,
                             choices = c("Mostrar casos activos Covid-19"),
                             justified = TRUE
                           ),
                           
                           #br(),
                           
                           # shinyWidgets::checkboxGroupButtons(
                           #   inputId = "fondo",
                           #   label= NULL,
                           #   choices = c("Mostrar etapas de cuarentena"),
                           #   justified = TRUE
                           # ),
                           
                           br(),
                           hr(),
                           br(),
                           
                           actionButton("notas_t", "Notas técnicas", class = "btn-primary btn-xs"),
                           br(),br(),
                           actionButton("notas_m", "Notas metodológicas", class = "btn-primary btn-xs"),
                           br(),br(),
                           actionButton("ayuda", "Ayuda", class = "btn-primary btn-xs"),
                           br(),br()
                           
                           # column(12, align="right",
                           #        actionLink(
                           #          inputId = "ayuda",
                           #          style = "align: right !important; font-size: 130%; color: #5f7181;",
                           #          #class = "botonayuda",
                           #          label = NULL, icon = icon("question-circle")
                           #        )
                           # )
                           #fin columna sidebar       
                    ), #%>% 
                    #animación sidebar
                    #aos::aos(animation = "zoom-in", duration = "1000", delay = "100"),
                    
                    #body ----
                    column(8,
                           #tabsetPanel(id = "tabs", type="pills",
                           #MOVILIDAD ----
                           #tabPanel(title=HTML("&nbsp;&nbsp;&nbsp;&nbsp;Movilidad&nbsp;&nbsp;&nbsp;&nbsp;"), 
                           
                           fluidRow(
                             column(12,
                                    #h2(icon("chart-line"), "Resultados principales"), #%>% 
                                    #aos::aos(animation = "zoom-in", duration = "600"), 
                                    
                                    br(),
                                    
                                    div(
                                      # HTML("<p>Esta herramienta permite visualizar los datos del <em>COVID-19 Community Mobility Report</em> desarrollado por Google, además de la información de etapas de cuarentenas proporcionada por el Ministerio de Salud y el Ministerio de Ciencia, Tecnología, Conocimiento e Innovación. 
                                      #                           Utilice los botones para seleccionar las variables que le interesa graficar.</p>"),
                                      p("Esta herramienta permite visualizar los datos del", em("COVID-19 Community Mobility Report"), "desarrollado por Google, además de la información de etapas del plan paso a paso proporcionado por el Ministerio de Salud y el Ministerio de Ciencia, Tecnología, Conocimiento e Innovación."),
                                      p("Utilice los botones en el menú lateral para seleccionar las variables que le interesa graficar. La información puede ser desplegada a nivel regional y provincial. Puede seleccionar en las categorías de lugar los sitios en los que le interese observar los registros de movilidad."),
                                      p("Puede graficar las series en diferentes rangos de fechas para identificar el impacto de las medidas restrictivas del plan paso a paso en la movilidad. En el gráfico inferior puede observar la proporción de la población de la provincia elegida, que se encontraba en las diferentes etapas del plan paso a paso."),
                                      p("Los datos de movilidad se comparan con un valor de referencia, en época pre-pandemia, que corresponde a la mediana de un periodo de 5 semanas comprendido entre el 3 de enero y el 6 de febrero del 2020."),
                                      #br(),
                                      # HTML("<p>Los datos son obtenidos y procesados desde el 
                                      #                           <a href='https://www.google.com/covid19/mobility/'>
                                      #                           <em>Google Mobility Report</em>.
                                      #                             </a>
                                      #                           y el 
                                      #                           <a href='https://github.com/MinCiencia/Datos-COVID19'>
                                      #                           repositorio de datos del Ministerio de Ciencias.
                                      #                             </a>
                                      #                           </p>"),
                                    ) #%>% 
                                    #aos::aos(animation = "zoom-in", duration = "600")
                             )
                           ),
                           
                           
                           
                           #datos 1 
                           
                           # fluidRow(
                           #   column(12,
                           #          #hr(),
                           #          column(3,
                           #                 uiOutput("dato_mayor_aumento")
                           #          ),
                           #          column(3,
                           #                 uiOutput("dato_mayor_reduccion")
                           #          ),
                           #          column(3,
                           #                 uiOutput("dato_menor_movilidad")
                           #          ),
                           #          column(3,
                           #                 uiOutput("dato_mayor_movilidad")
                           #          ),
                           #          column(12,
                           #                 br(),
                           #                 #hr(),
                           #          )
                           #   )
                           # ),
                           
                           #fecha
                           # fluidRow(
                           #   column(12,
                           #          dateRangeInput(
                           #            inputId = "fecha",
                           #            label = "Seleccionar rango de fechas",
                           #            min = "2020-02-15", #ymd
                           #            max = Sys.Date(),
                           #            start = Sys.Date() - months(3),
                           #            end = Sys.Date(),
                           #            format = "dd-mm-yyyy",
                           #            startview = "month",
                           #            weekstart = 0,
                           #            language = "es",
                           #            separator = " hasta ",
                           #            width = "100%",
                           #            autoclose = TRUE
                           #          ) %>% 
                           #            aos::aos(animation = "zoom-in", duration = "600"),
                           #   )
                           # ),
                           
                           #grafico ----
                           fluidRow(
                             column(12,
                                    h3("Movilidad a nivel nacional"), #%>% 
                                    #aos::aos(animation = "zoom-in", duration = "600", once = TRUE),
                                    # plotOutput("d_pais", height = alturas_graficos) %>% 
                                    #   shinycssloaders::withSpinner(hide.ui = F),
                                    girafeOutput("d_pais2", height = alturas_graficos) %>% 
                                      shinycssloaders::withSpinner(hide.ui = F),
                                    # plotlyOutput("d_pais3", height = alturas_graficos) %>% 
                                    #   shinycssloaders::withSpinner(hide.ui = F),
                                    br(),
                                    
                                    h3("Movilidad a nivel regional"), #%>% 
                                    #aos::aos(animation = "zoom-in", duration = "600", once = TRUE),
                                    h5(textOutput("region_seleccionada")),
                                    # plotOutput("d_region", height = alturas_graficos) %>% 
                                    #   shinycssloaders::withSpinner(hide.ui = F),
                                    girafeOutput("d_region2", height = alturas_graficos) %>% 
                                      shinycssloaders::withSpinner(hide.ui = F),
                                    br(),
                                    
                                    h3("Movilidad a nivel provincial"), #%>% 
                                    #aos::aos(animation = "zoom-in", duration = "600", once = TRUE),
                                    h5(textOutput("provincia_seleccionada")),
                                    # plotOutput("d_provincia", height = alturas_graficos) %>% 
                                    #   shinycssloaders::withSpinner(hide.ui = F),
                                    girafeOutput("d_provincia2", height = alturas_graficos) %>% 
                                      shinycssloaders::withSpinner(hide.ui = F)
                             )
                           ),
                           
                           
                           
                           #datitos 2 ----
                           
                           #fila 2
                           fluidRow(
                             #hr(),
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
                                    #hr(),
                             )
                           )
                           #                         ), #fin tab movilidad
                           # 
                           # #CUARENTENAS ----
                           # tabPanel(title=HTML("&nbsp;&nbsp;&nbsp;&nbsp;Cuarentenas&nbsp;&nbsp;&nbsp;&nbsp;"),
                           #          
                           #          fluidRow(
                           #            column(12,
                           #                   h2(icon("chart-area"), "Población en cuarentenas") %>% 
                           #                     aos::aos(animation = "zoom-in", duration = "600"), 
                           #                   
                           #                   br()
                           #            )
                           #          ),
                           #          
                           #          #grafico ----
                           #          fluidRow(
                           #            column(12,
                           #                   h3("Población en cuarentena a nivel nacional"), #%>% 
                           #                     #aos::aos(animation = "zoom-in", duration = "600", once = TRUE),
                           #                   plotOutput("cuarentenas_pais_g") %>% 
                           #                     shinycssloaders::withSpinner(hide.ui = F),
                           #                   br(),
                           #                   
                           #                   h3("Población en cuarentena a nivel regional"), #%>% 
                           #                     #aos::aos(animation = "zoom-in", duration = "600", once = TRUE),
                           #                   h5(textOutput("region_seleccionada2")),
                           #                   plotOutput("cuarentenas_region_g") %>% 
                           #                     shinycssloaders::withSpinner(hide.ui = F),
                           #                   br(),
                           #                   
                           #                   h3("Población en cuarentena a nivel provincial"), #%>% 
                           #                     #aos::aos(animation = "zoom-in", duration = "600", once = TRUE),
                           #                   h5(textOutput("provincia_seleccionada2")),
                           #                   plotOutput("cuarentenas_provincia_g") %>% 
                           #                     shinycssloaders::withSpinner(hide.ui = F)
                           #            )
                           #          ),
                           #          
                           # ) #fin tab cuarentenas
                           #)#fin tabset
                    )
                  ), #fin fluidrow sidebar+body
                  
                  #—----
                  #tablas ----
                  # 
                  # fluidRow(
                  #   column(12,
                  #          hr(),
                  #          h2(icon("table"), "Tablas de resumen") %>% 
                  #            aos::aos(animation = "zoom-in", duration = "600")
                  #   ),
                  #   column(12,
                  #          h3("Provincias con mayor movilidad por sector"),
                  #          formattable::formattableOutput("tabla_mayor_movilidad")
                  #   ),
                  #   column(6,
                  #          h3("Sectores con mayor movilidad promedio"),
                  #          formattable::formattableOutput("tabla_sectores_mayor")
                  #   ),
                  #   column(6,
                  #          h3("Movilidad promedio de las regiones"),
                  #          formattable::formattableOutput("tabla_regiones_mayor")
                  #   ),
                  # ),
                  # 
                  
                  #descarga ----
                  
                  fluidRow(
                    column(12,
                           div(
                             downloadButton(outputId = "descarga", 
                                            label = HTML("&nbsp;&nbsp;descargar datos"), 
                                            icon = icon("file-download"),
                                            style = "text-decoration: none !important;"
                             ), 
                             align = "center"),      
                           br()
                    )
                  ),
                  
                  
                  #footer ----
                  
                  fluidRow(
                    column(12, align = "center",
                           hr(),
                           a(href = "https://www.mat.uc.cl", target = "blank", 
                             "Facultad de Matemáticas - Pontificia Universidad Católica de Chile"),
                           br(),
                           p("Sebastián Massa Slimming, Bastián Olea Herrera, Alejandro Jara Vallejos, Alexis Alvear Leyton y Mauricio Castro Cepero"),
                    )
                  ),
                  fluidRow(
                    column(6, align = "center",
                           img(src = "logo-uc-04.svg", width = 150, style = "padding: 8px"),
                           br(), br(), p("")
                    ),
                    column(6, align = "center",
                           img(src = "Logo ANID_MILENIO.png", width = 200, style = "padding: 8px"),
                           br(), br(),  p("")
                    ),
                  )
)
)