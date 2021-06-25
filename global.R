library(dplyr)
library(ggplot2)

Sys.setlocale(category = "LC_TIME", locale="es_ES.UTF-8") #meses en español

#cargar lista de provincias/regiones/comunas
load("datos/provincias_comunas.rdata")
load("movilidad.rdata")

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
