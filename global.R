library(dplyr)
library(ggplot2)

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