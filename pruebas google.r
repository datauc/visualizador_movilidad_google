library(dplyr)
library(ggplot2)

Sys.setlocale(category = "LC_TIME", locale="es_ES.UTF-8") #meses en español

#Sys.getlocale()
#Sys.setlocale(locale="C.UTF-8") #meses en español

#importar y ordenar
movilidad <- readr::read_csv("datos/Region_Mobility_Report_CSVs/2020_CL_Region_Mobility_Report.csv") %>% 
  rename(region = sub_region_1,
         provincia = sub_region_2,
         provincia_cod = iso_3166_2_code,
         fecha = date) %>% 
  select(region,
         provincia,
         provincia_cod,
         fecha,
         ends_with("baseline")) %>% 
  tidyr::pivot_longer(cols = ends_with("baseline"), 
                      names_to = "sector", values_to = "valor") %>% 
  mutate(sector = stringr::str_remove(sector, "_percent_change_from_baseline"),
         sector = stringr::str_replace_all(sector, "_", " ")) %>% 
  mutate(sector = recode(sector,
                         "grocery and pharmacy" = "Mercadería y farmacia",
                         "parks" = "Parques",
                         "residential" = "Viviendas",
                         "retail and recreation" = "Retail y recreación",
                         "transit stations" = "Transporte público",
                         "workplaces" = "Lugares de trabajo")) %>% 
  mutate(provincia = stringr::str_remove(provincia, " Province")) %>% 
  mutate(region = recode(region,
                         "Santiago Metropolitan Region" = "Metropolitana de Santiago",
                         "Aysén" =  "Aysén del Gral. C. Ibáñez del Campo",
                         "O'Higgins" = "Lib. Gral. Bernardo O'Higgins",
                         "Araucania" = "La Araucanía",
                         "Bio Bio" = "Biobío"))
                         

movilidad %>% count(region)
movilidad %>% count(sector) %>% select(1)


save(movilidad, file = "movilidad.rdata")

#graficar
movilidad %>% 
  filter(region == "Santiago Metropolitan Region") %>% 
  ggplot() +
  geom_line(aes(fecha, valor, col = sector)) +
  facet_wrap(~provincia, scales = "free") +
  scale_x_date(date_breaks = "months", date_labels = "%b") +
  labs(y = "Cambio porcentual respecto a línea de base") +
  theme(axis.title.x = element_blank(),
        legend.title = element_blank(),
        panel.grid.minor.x = element_blank(),
        legend.position = c(.8, .15))

ggsave(dpi = "retina", width = 10, height = 8,
       filename = "Movilidad RM.jpg")



#selector de comunas que entregue la provincia ----
library(rvest)

#comunas de la RM
#url_tablas <- "https://es.wikipedia.org/wiki/Región_Metropolitana_de_Santiago#División_pol%C3%ADtico-administrativa_de_la_Región_Metropolitana_de_Santiago"
# #scrapping de tablas
# tablas <- read_html(url_tablas) %>% 
#   html_table()
# 
# #elegir tabla de la RM y formatearla
# comunas <- tablas[[3]] %>%
#   janitor::clean_names() %>%
#   mutate(provincia2 = tolower(provincia),
#          provincia2 = stringr::str_remove(provincia2, "\\d+ "),
#          provincia2 = textclean::replace_non_ascii(provincia2))
# comunas


comunas_s <- read_html("https://es.wikipedia.org/wiki/Anexo:Comunas_de_Chile") %>% 
  html_table()

comunas <- comunas_s %>% 
  bind_rows() %>% 
  janitor::clean_names() %>% 
  select(-x3) %>% 
    mutate(provincia2 = tolower(provincia),
           provincia2 = stringr::str_remove(provincia2, "\\d+ "),
           provincia2 = textclean::replace_non_ascii(provincia2))

#anexar comunas a base de movilidad
provincias_comunas <- movilidad %>% select(provincia) %>% distinct() %>% na.omit() %>% 
  mutate(provincia2 = tolower(provincia),
         provincia2 = textclean::replace_non_ascii(provincia2)) %>% 
  left_join(comunas %>% rename(comuna=nombre) %>% select(region, comuna, provincia, provincia2), 
            by = c("provincia2" = "provincia2")) %>% 
  select(provincia.x, region, comuna) %>% 
  rename(provincia = provincia.x)

provincias_comunas

save(provincias_comunas, file = "datos/provincias_comunas.rdata")




movilidad$sector %>% unique() %>% dput()

provincias_comunas$region %>% unique()  %>% sort() %>% dput()

regiones <- c("Metropolitana de Santiago", 
   "Antofagasta", "La Araucanía", "Arica y Parinacota", "Atacama", 
   "Aysén del Gral. C. Ibáñez del Campo", "Biobío", "Coquimbo", 
   "Los Lagos", "Los Ríos", "Magallanes y Antártica Chilena", 
   "Maule", "Ñuble", "Lib. Gral. Bernardo O'Higgins",
   "Tarapacá", "Valparaíso")

regiones %>% sort() %>% dput()

c("Metropolitana de Santiago", "Antofagasta", "Arica y Parinacota", "Atacama", "Aysén del Gral. C. Ibáñez del Campo", 
  "Biobío", "Coquimbo", "La Araucanía", "Lib. Gral. Bernardo O'Higgins", 
  "Los Lagos", "Los Ríos", "Magallanes y Antártica Chilena", 
  "Maule", "Ñuble", "Tarapacá", 
  "Valparaíso")


provincias_comunas %>% 
  filter(comuna == "Puente Alto") %>% 
  mutate(provincia = as.character(provincia)) %>% 
  select(provincia) %>% 
  pull()

movilidad %>% 
  filter(sector %in% c("Parques", "Viviendas"))



unique(movilidad$region)

       

regiones




#—----
library(echarts4r)

library(dplyr)
unique(movilidad$provincia)
movilidad %>% 
  count(provincia)
  filter(provincia == "Cordillera")
  e_charts(x = fecha) %>% 
  e_line(valor)
