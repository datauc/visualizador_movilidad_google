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



#importar comunas y provincias de Chile ----
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

#comunas de chile
comunas_s <- read_html("https://es.wikipedia.org/wiki/Anexo:Comunas_de_Chile") %>% 
  html_table()

comunas <- comunas_s %>% 
  bind_rows() %>% 
  janitor::clean_names() %>% 
  select(-x3) %>% 
  rename(codigo_comuna = cut_codigo_unico_territorial) %>% 
    mutate(provincia2 = tolower(provincia),
           provincia2 = stringr::str_remove(provincia2, "\\d+ "),
           provincia2 = textclean::replace_non_ascii(provincia2))

comunas

#anexar comunas a base de movilidad
provincias_comunas <- movilidad %>% 
  select(provincia) %>% 
  distinct() %>% 
  na.omit() %>% 
  mutate(provincia2 = tolower(provincia),
         provincia2 = textclean::replace_non_ascii(provincia2)) %>% 
  left_join(comunas %>% rename(comuna=nombre) %>% select(region, comuna, codigo_comuna, provincia, provincia2), 
            by = c("provincia2" = "provincia2")) %>% 
  select(provincia.x, region, comuna, codigo_comuna) %>% 
  rename(provincia = provincia.x)

provincias_comunas

save(provincias_comunas, file = "datos/provincias_comunas.rdata")


#generar vectores ----


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


#echarts ----
library(echarts4r)

library(dplyr)
unique(movilidad$provincia)
movilidad %>% 
  filter(provincia == "Cordillera") %>% 
  group_by(sector) %>% 
  e_charts(x = fecha) %>% 
  e_line(valor)



avg <- list(
  type = "average",
  name = "AVG"
)

movilidad %>% 
  dplyr::group_by(sector) %>% 
  dplyr::filter(provincia == "Santiago") %>% 
  echarts4r::e_chart(x = fecha) %>% 
  echarts4r::e_line(serie = valor, smooth = TRUE, symbol_size = 1,
                    coord_system = "cartesian2d") %>% 
  echarts4r::e_tooltip(trigger = "axis", axisPointer = list(type = "cross"),
                       textStyle = list(fontFamily="sans-serif",
                                        fontSize=10)) %>% 
  echarts4r::e_toolbox_feature(feature = "saveAsImage", title = "Guardar gráfico") %>% 
  echarts4r::e_toolbox_feature("dataZoom", title = list(zoom = "Ampliar gráfico", 
                                                        back = "Deshacer")) %>% 
  echarts4r::e_toolbox_feature("restore", title = 'Restaurar') %>% 
  # echarts4r::e_title("Índice de movilidad Google", 
  #                    left = "center", top = 2, 
  #                    textStyle = list(fontSize = 20)) %>% 
  echarts4r::e_theme("royal") %>% 
  echarts4r::e_mark_line(data = list(type = "average", name = "AVG"), title = "Promedio")  %>% 
  echarts4r::e_datazoom()  %>% 
  #echarts4r::e_timeline_opts(autoPlay = TRUE, top = 40) %>% 
  echarts4r::e_grid(right = "15%") %>%  
  echarts4r::e_legend(orient = "vertical", right = "5", top = "15%") #%>% 
  # echarts4r::e_mark_point(data = list(
  #   xAxis = as.Date("2020-05-27"),
  #   yAxis = 7,
  #   value = "Cuarentena 1"
  # )) 


#remover puntos
#acercar linea de tiempo
#agregar cuarentenas
#remover puntos



#cuarentenas ----
#importar cuarentenas desde el repositorio del ministerio de ciencias
cuarentenas <- readr::read_csv("https://github.com/MinCiencia/Datos-COVID19/raw/master/output/producto74/paso_a_paso.csv",
                               col_types = readr::cols()) %>% 
  tidyr::pivot_longer(starts_with("20"), names_to = "fecha", values_to = "etapa_n") %>% 
  mutate(etapa = case_when(etapa_n == 1 ~ "Cuarentena",
                           etapa_n == 2 ~ "Transición",
                           etapa_n == 3 ~ "Preparación",
                           etapa_n == 4 ~ "Apertura inicial",
                           etapa_n == 5 ~ "Apurtura avanzada")) %>% 
  mutate(etapa = forcats::fct_reorder(etapa, etapa_n)) %>% 
  mutate(fecha = lubridate::ymd(fecha)) %>% 
  #anexar provincias y comunas a partir del codigo de comuna
  left_join(provincias_comunas %>% select(provincia, comuna, codigo_comuna), by = "codigo_comuna")


comuna = "La Florida"

#solo cambios
cuarentenas_cambios <- cuarentenas %>% 
  filter(comuna == "La Florida") %>% 
  select(etapa, etapa_n, fecha) %>% 
  filter(etapa != lag(etapa)) %>% 
  mutate(fecha_cambios = fecha)

#Transición        2 2020-08-31

movilidad %>% 
  filter(fecha =="2020-08-31") %>% 
  filter(provincia == "Cordillera")
  #anexar cuarentenas
  #left_join(cuarentenas %>% filter(comuna == "La Florida") %>% select(etapa, etapa_n, fecha), by = "fecha") %>% 
  filter(provincia == "Cordillera") %>% 
  left_join(cuarentenas_cambios, by = "fecha") %>% 
  arrange(desc(fecha)) %>% 
  glimpse()
  

#graficar cuarentenas
movilidad2 <- movilidad %>% 
  filter(provincia == "Cordillera") %>%
  left_join(cuarentenas_cambios, by = "fecha")

unique(movilidad2$fecha_cambios)

max(movilidad$fecha)
max(movilidad2$fecha)
movilidad2 %>% 
  filter(fecha == "2021-03-13")

movilidad2 %>%   
group_by(sector) %>% 
  e_charts(x = fecha) %>% 
  e_line(valor, symbol='none') %>% 
  e_datazoom(
    type = "slider", 
    toolbox = FALSE,
    bottom = -5,
  ) %>% 
  e_x_axis(fecha, axisPointer = list(show = TRUE)) %>% 
  e_mark_line(data = list(xAxis = c(lubridate::ymd("2021-02-09")),
                          symbol = "circle"
                          ),
              title = "cuarentena"
              ) %>% 
  e_mark_line(data = list(
    yAxis = "average")
    )

#—---
#filtrar provincia
d1 <- movilidad %>% 
  filter(provincia == "Cordillera")

d1 %>% arrange(fecha)

fecha_maxima <- max(d1$fecha)
fecha_minima <- min(d1$fecha)

#solo cambios
cuarentenas_cambios <- cuarentenas %>% 
  filter(comuna == "Puente Alto") %>% 
  select(etapa, etapa_n, fecha) %>% 
  filter(etapa != lag(etapa)) %>% 
  mutate(hasta = lead(fecha),
         hasta = replace(hasta, is.na(hasta), fecha_maxima))

#suavizar
d2 <- d1 %>% 
  group_by(sector) %>% 
  mutate(valor = zoo::rollmean(valor, k = 3, fill = "extend"))

d2 %>% 
  ggplot() +
  #fondo
  geom_rect(data = cuarentenas_cambios, aes(fill=etapa,
      xmin = fecha, xmax = hasta,
      ymin = -Inf, ymax = Inf),
      alpha = 0.2) +
  #lineas
  geom_line(aes(fecha, valor, col = sector), show.legend = F) +
  geom_point(aes(fecha, valor, col = sector), size = 0, alpha = 0) +
  #limites horizontales
  coord_cartesian(xlim = c(fecha_minima, fecha_maxima)) + 
  scale_x_date(date_breaks = "months", date_labels = "%b", 
               expand = expansion(mult = c(0,0))) +
  scale_fill_manual(values = rev(c("yellow1", "orange", "red"))) +
  labs(y = "Cambio porcentual respecto a línea de base") +
  theme_minimal() +
  theme(axis.title.x = element_blank(),
        legend.title = element_blank(),
        panel.grid.minor.x = element_blank(),
        #legend.position = c(.8, .15))
        legend.position = "right") +
  guides(fill = guide_legend(override.aes = list(size = 3, alpha=0.5), ncol = 1)) +
  guides(col = guide_legend(override.aes = list(size = 5, alpha=1, fill=NA, text=NA), ncol = 1))
  


