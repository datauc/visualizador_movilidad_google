library(dplyr)
library(ggplot2)
#library(formattable)
#library(shinybulma)

Sys.setlocale(category = "LC_TIME", locale="es_ES.UTF-8") #meses en español

#cargar lista de provincias/regiones/comunas
load("datos/provincias_comunas.rdata")
load("movilidad.rdata")

#colores
gris_claro <- "#5f7181"
gris_oscuro <- "#414a58"

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