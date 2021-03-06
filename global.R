library(dplyr)
library(ggplot2)

Sys.setlocale(category = "LC_TIME", locale="es_ES.UTF-8") #meses en español

#datos ----
load("datos/provincias_comunas.rdata")
load("datos/comunas.rdata")
load("movilidad.rdata")

#colores ----
gris_claro <- "#5f7181"
gris_oscuro <- "#414a58"


celeste <- "#0176DE"
azul <- "#173F8A"
azul_oscuro <- "#03122E"

#colores originales
colores1 <- c("#dc0073", "#008bf8", "#ff4e00",
              "#6a4c93", "#04e762", "#f5b700")

#colores material 600
colores2 <- c(#"#e53935",
              "#d81b60",
              "#8e24aa",
              "#3949ab",
              #"#1e88e5",
              "#039be5",
              #"#43a047",
              "#00897b",
              "#7cb342")
              #"#fb8c00")
#scales::show_col(colores2)

semaforo <- c("#ef5350",
              #"#ff7043",
              "#ffa726",
              #"#ff8a65",
              #"#ffca28",
              #"#ffee58",
              "#ffeb3b",
              "#9ccc65")
#scales::show_col(semaforo)


#selectores----
#provincias_comunas$region %>% unique()  %>% sort() %>% dput()
regiones <- c("Metropolitana de Santiago", "Antofagasta", "Arica y Parinacota", "Atacama", "Aysén del Gral. C. Ibáñez del Campo", 
              "Biobío", "Coquimbo", "La Araucanía", "Lib. Gral. Bernardo O'Higgins", 
              "Los Lagos", "Los Ríos", "Magallanes y Antártica Chilena", 
              "Maule", "Ñuble", "Tarapacá", 
              "Valparaíso")

#movilidad$sector %>% unique() %>% dput()
sectores <- c("Lugares de trabajo", 
              "Mercadería y farmacia", "Parques", "Retail y recreación", 
              "Transporte público", "Viviendas")

meses <- c("Enero" = 1,
           "Febrero" = 2,
           "Marzo" = 3,
           "Abril" = 4,
           "Mayo" = 5,
           "Junio" = 6,
           "Julio" = 7,
           "Agosto" = 8,
           "Septiembre" = 9,
           "Octubre" = 10,
           "Noviembre" = 11,
           "Diciembre" = 12)


#Configuración de animación al cargar gráficos
options(spinner.color = azul,
        spinner.type= 4,
        spinner.size = 1)


#función que calcula media móvil a partir del input
media_movil <- function(x, input_dias) {
  
  #determinar días
  if (input_dias != "No") {
    if (input_dias == "1 semana") {
      dias <- 7
    } else if (input_dias == "2 semanas") {
      dias <- 14
    } else if (input_dias == "3 semanas") {
      dias <- 21
    } else {
      dias <- readr::parse_number(input_dias)
    }
  }
  
  #si no se eligen días: nada
  if (input_dias == "No") {
    x <- x
  } else {
    #si se eligen días: suavizar
    x <- zoo::rollmean(x, k = dias, fill = "extend")
  }
}



#función que calcula diferencia de movilidad entre hoy y ayer
movilidad_cambios <- function(data) {
  data %>% 
    ungroup() %>% 
    #filtrar regiones
    filter(!is.na(provincia)) %>% 
    #ordenar
    arrange(provincia, sector, fecha) %>% 
    #cambio respecto al día anterior
    mutate(ayer = lag(valor),
           cambio = valor - ayer)
}


bloque_datos <- function(titulo = "Título",
                         cambio = 20,
                         hoy = 50,
                         ayer = 30,
                         provincia = "Provincia",
                         sector = "Sector",
                         region = "Región",
                         prefijo ="",
                         texto = "%") {
  
  flecha_simbolo <- ifelse(hoy > ayer, "▲", "▼")
  flecha_color <- ifelse(hoy > ayer, 
                           "#ff8080", #rojo
                           "#99e699") #verde
  
  #borrar simbolo si cifras son iguales
  flecha_simbolo <- ifelse(hoy == ayer, "", flecha_simbolo)
  
  list(
    p(titulo, style = paste0("margin-bottom: -7px; font-family: Oswald; font-size: 130%; color:", gris_oscuro, "; margin-top: 20px;")),
    div(style = "margin-bottom: -12px;",
        #prefijo a cifra
        p(prefijo, style = paste0("color:", gris_oscuro, "; font-size: 200%; font-family: Oswald; display:inline-block;")),
        #cifra
        p(cambio, class = "cifra_datos", style = "display:inline-block;"),
        #texto despues de cifra (%)
        p(texto, style = paste0("color:", gris_oscuro, "; font-size: 200%; font-family: Oswald; display:inline-block;")),
        #flecha
        p(flecha_simbolo, style = paste0("color: ", flecha_color, "; display:inline-block; vertical-align: 60%; line-height: normal;")),
    ),
    #textos grises
    p(provincia, class = "texto_datos"),
    p(sector, class = "texto_datos"),
    p(region %>% paste("Región:", .), class = "texto_datos")
  ) 
}


custom_color_tile <- function (...) 
{
  formatter("span",
            style = function(x) style(display = "block", 
                                      padding = "0 4px", 
                                      `color` = "white", 
                                      `border-radius` = "4px", 
                                      `background-color` = csscolor(gradient(as.numeric(x), 
                                                                             ...))))
}


#piezas gráficos ----

#gráfico base
g_base <- list(
  geom_hline(yintercept = 0, size = 0.4, alpha = 0.7),
  #geom_hline(yintercept = 50, size = 0.3, alpha = 0.4, linetype = "dashed"),
  #geom_hline(yintercept = -50, size = 0.3, alpha = 0.4, linetype = "dashed"),
  #lineas
  geom_line(aes(fecha, valor, col = sector), size = 1, show.legend = F),
  geom_point(aes(fecha, valor, col = sector), size = 0, alpha = 0),
  scale_x_date(date_breaks = "months", date_labels = "%b", 
               expand = expansion(mult = c(0,0)))
)

#eje normal del índice de movilidad
g_primer_eje <- list(
  scale_y_continuous(labels = function (x) paste0(x, "%"), 
                     breaks = c(-75, -50, -25, 0, 25, 50, 75))
)

#gráfico base con transformaciones para poner fondo de cuarentenas
g_base_2 <- list(
  geom_hline(yintercept = 0+100, size = 0.4, alpha = 0.7),
  #geom_hline(yintercept = 50+100, size = 0.3, alpha = 0.4, linetype = "dashed"),
  #geom_hline(yintercept = -50+100, size = 0.3, alpha = 0.4, linetype = "dashed"),
  #lineas
  geom_line(aes(fecha, valor+100, col = sector), size = 1, show.legend = F),
  geom_point(aes(fecha, valor+100, col = sector), size = 0, alpha = 0)
)

g_eje_mensual <- list(
  scale_x_date(date_breaks = "months", date_labels = "%b", 
               expand = expansion(mult = c(0, 0)))
)

g_eje_semanal <- list(
  scale_x_date(date_breaks = "weeks", date_labels = "%d-%b", 
               expand = expansion(mult = c(0, 0)))
)

#eje normal con transformaciones
g_primer_eje_2 <- list(
  scale_y_continuous(labels = function (x) paste0(x-100, "%"), 
                     breaks = c(-75, -50, -25, 0, 25, 50, 75)+100)
)

g_escalas <- list(
  scale_fill_manual(values = semaforo),
  scale_color_manual(values = colores2),
                     labs(y = "% movilidad")
)

g_ejes_cuarentenas <- list(
  scale_y_continuous(labels = function (x) paste0(x, "%"),
                     breaks = c(0, 50, 100)),
    labs(y = "% pob. en cuarentena")
)

g_leyenda <- list(
  theme(legend.position = "top",
        legend.box.margin = margin(b=-20))
)

g_temas <- list(
  theme_minimal(),
  theme(axis.title.x = element_blank(),
        legend.title = element_blank(),
        panel.grid.minor.x = element_blank(),
        #legend.position = c(.8, .15))
        legend.position = "bottom"),
  theme(text = element_text(family = "Open Sans", color = gris_oscuro),
        plot.subtitle = element_text(family = "Oswald", size = 14),
        plot.title = element_text(family = "Oswald"),
        axis.title.y = element_text(family = "Open Sans", face = "italic", size = 10),
        axis.text.y = element_text(family = "Oswald")),
  guides(fill = guide_legend(override.aes = list(size = 3, alpha=0.4), nrow = 2)),
  guides(col = guide_legend(override.aes = list(size = 5, alpha=1, fill=NA, text=NA), nrow = 2))
)


#piezas para gráficos de cuarentenas
g_cuarentenas <- list(
  geom_col(width = 1, alpha = 0.6, show.legend = F),
    scale_y_continuous(labels = function (x) paste0(x*100, "%")),
    labs(y = "Porcentaje de la población\nen cada etapa de cuarentena"),
    geom_point(aes(fecha, porcentaje, col = etapa), size = 0, alpha = 0),
    scale_x_date(date_breaks = "months", date_labels = "%b", 
                 expand = expansion(mult = c(0,0))),
    scale_fill_manual(values = semaforo,
                      aesthetics = c("color", "fill"))
)

g_cuarentenas_tema <- list(
  theme_minimal(),
    theme(text = element_text(family = "Open Sans", color = gris_oscuro),
          panel.grid = element_blank(),
          axis.title.y = element_text(family = "Open Sans", size = 10),
          axis.text.y = element_text(margin = margin(r=0), family = "Oswald"),
          axis.title.x = element_blank(),
          legend.title = element_blank(),
          legend.position = "bottom"),
    guides(fill = guide_legend(override.aes = list(size = 3, alpha=0.6), nrow = 2)),
    guides(col = guide_legend(override.aes = list(size = 5, alpha=0.6, fill=NA, text=NA), nrow = 2))
)


g_ajustes_cuarentenas <- list(
theme(legend.text = element_text(margin = margin(r = 10))), #separar textos leyenda
theme(axis.text.x = element_blank()), #sin fecha
theme(panel.grid = element_blank())
)


opciones_cowplot <- c(2, 1)
alturas_graficos <- "450px"

