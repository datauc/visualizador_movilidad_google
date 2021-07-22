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
  left_join(provincias_comunas %>% select(region, provincia, comuna, codigo_comuna), by = "codigo_comuna")


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
#graficar con cuarentenas ----
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
  geom_hline(yintercept = 0, size = 0.3, alpha=0.8) +
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
  




#—----

movilidad %>% 
  filter(region == "Metropolitana de Santiago") %>% count(provincia)



#—----

covid_activos <- readr::read_csv("https://coronavirus-api.mat.uc.cl/casos_activos_sintomas_comuna")

covid_activos_f <- covid_activos %>% 
  select(-comuna) %>% 
  mutate(codigo_comuna = as.numeric(codigo_comuna)) %>% 
  left_join(cuarentenas %>% 
              select(comuna, codigo_comuna), 
            by = "codigo_comuna") %>% 
  filter(comuna == "Puente Alto") %>% 
  select(-region, -codigo_region, -poblacion) %>% 
  #reescalar datos
  mutate(casos_r = scales::rescale(casos, to = c(-50, 50)))


d2 %>% 
  ggplot() +
  #fondo
  geom_rect(data = cuarentenas_cambios, aes(fill=etapa,
                                            xmin = fecha, xmax = hasta,
                                            ymin = -Inf, ymax = Inf),
            alpha = 0.2) +
  geom_hline(yintercept = 0, size = 0.3, alpha=0.8) +
  #lineas
  geom_line(aes(fecha, valor, col = sector), show.legend = F) +
  ####
  #covid
  geom_line(data = covid_activos_f, aes(fecha, casos/50), 
            size = 1, alpha=0.8, linetype = "dashed", lineend="round") +
  scale_y_continuous(sec.axis = sec_axis(~.*50, 
                                         breaks = scales::breaks_extended(6), #breaks covid
                                         labels = function (x) ifelse(x<0, "", x), #eliminar negativos
                                         #breaks = 0, 
                                         name = paste("Casos activos de Covid-19 en", "Puente Alto"))) +
  ####
  ####
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




#—----

#datos ----
load("movilidad.rdata")

movilidad %>% 
  filter(provincia == "Cordillera")

movilidad_cambios <- movilidad %>% 
  ungroup() %>% 
  #filtrar regiones
  filter(!is.na(provincia)) %>% 
  #ordenar
  arrange(provincia, sector, fecha) %>% 
  #cambio respecto al día anterior
  mutate(cambio = valor - lag(valor))

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


movilidad_cambios %>% 
  print(n=200)

#mayor aumento
movilidad %>% 
  movilidad_cambios() %>% 
  filter(cambio > 0) %>% 
  filter(fecha == max(fecha)) %>%  #ultima fecha
  arrange(desc(cambio)) %>% 
  slice(1)

#mayor disminución
movilidad %>% 
  movilidad_cambios() %>% 
  filter(cambio < 0) %>% 
  filter(fecha == max(fecha)) %>%  #ultima fecha
  arrange(cambio) %>% 
  slice(1)

#nivel mas bajo de movilidad
movilidad %>% 
  ungroup() %>% 
  filter(!is.na(provincia)) %>%  #filtrar regiones
  filter(fecha == max(fecha)) %>%  #ultima fecha
  arrange(valor)

#nivel mas alto de movilidad
movilidad %>% 
  ungroup() %>% 
  filter(!is.na(provincia)) %>%  #filtrar regiones
  filter(fecha == max(fecha)) %>%  #ultima fecha
  arrange(desc(valor))

#mayor aumento en sector x, y, ...
movilidad %>% 
  movilidad_cambios() %>% 
  filter(cambio > 0) %>% 
  #filtrar sector
  filter(sector == "Mercadería y farmacia") %>% 
  filter(fecha == max(fecha)) %>%  #ultima fecha
  arrange(desc(cambio))

#mayor disminución en sector x, y, ...
movilidad %>% 
  movilidad_cambios() %>% 
  filter(cambio < 0) %>% 
  #filtrar sector
  filter(sector == "Mercadería y farmacia") %>% 
  filter(fecha == max(fecha)) %>%  #ultima fecha
  arrange(cambio)

#mayor movilidad en sector
movilidad %>% 
  ungroup() %>% 
  filter(!is.na(provincia)) %>%  #filtrar regiones
  filter(sector == "Parques") %>% 
  filter(fecha == max(fecha)) %>%  #ultima fecha
  arrange(desc(valor))

#tablas ----
library(formattable)
#provincias con mayor movilidad por sector
movilidad %>% 
  ungroup() %>% 
  filter(!is.na(provincia)) %>%  #filtrar regiones
  #filter(sector == "Mercadería y farmacia") %>% 
  filter(fecha == max(fecha)) %>%  #ultima fecha
  group_by(sector) %>% 
  arrange(desc(valor)) %>% 
  mutate(id = 1:n()) %>% 
  filter(id == 1) %>% 
  select(Provincia = provincia, Región = region, 
         Sector = sector, Movilidad=valor) %>% 
  formattable(
    align = c("l", "l", "c", "c"),
    list(
      Provincia = formatter("span", style = ~ style(font.style = "bold")),
      area(col = "Movilidad") ~ color_tile("#fce8ef", "#f3a5c0")#,
      #area(col = "Tasa") ~ color_tile("#f4e4f4", "#c8b1de")
    )
  )


#movilidad promedio sectores
movilidad %>% 
  ungroup() %>% 
  filter(!is.na(provincia)) %>%  #filtrar regiones
  filter(fecha == max(fecha)) %>% #ultima fecha
  group_by(sector) %>% 
  summarize(valor = round(mean(valor, na.rm = T), 1)) %>% 
  arrange(valor) %>% 
  select(Sector = sector,
         Movilidad = valor) %>% 
  formattable(
    align = c("l", "l", "c", "c"),
    list(
      Sector = formatter("span", style = ~ style(font.style = "bold")),
      area(col = "Movilidad") ~ color_tile("#fce8ef", "#f3a5c0")#,
      #area(col = "Tasa") ~ color_tile("#f4e4f4", "#c8b1de")
    )
  )

#movilidad promedio por región
movilidad %>% 
  ungroup() %>% 
  filter(!is.na(region)) %>%  #filtrar regiones
  filter(is.na(provincia)) %>%  #filtrar regiones
  filter(fecha == max(fecha)) %>% #ultima fecha
  group_by(region) %>% 
  summarize(valor = mean(valor, na.rm = T)) %>% 
  arrange(valor)


#—----

movilidad %>% 
  group_by(fecha, sector) %>% 
  summarize(valor = mean(valor, na.rm = T))




movilidad %>% filter(provincia == "Cordillera") %>% tally()

movilidad %>% filter(is.na(region))






movilidad %>% 
  filter(is.na(region)) %>%
  arrange(fecha) %>%
  mutate(valor = media_movil(valor, "3 días"))

covid_activos <-   readr::read_csv("https://coronavirus-api.mat.uc.cl/casos_activos_sintomas_comuna",
                                   col_types = readr::cols())

covid_activos %>% 
  left_join(provincias_comunas %>% select(comuna, provincia)) %>% 
  group_by(fecha, provincia) %>% 
  summarize(casos = sum(casos, na.rm = T)) %>% 
  filter(provincia == "Chacabuco") %>% 
  filter(fecha >= lubridate::dmy("20-04-2021")) %>% 
  ggplot(aes(fecha, casos)) +
  geom_line()



covid_diarios <- readr::read_csv("https://coronavirus-api.mat.uc.cl/casos_activos_sintomas_comuna",
                                 col_types = readr::cols()) %>% 
  select(fecha, region, comuna, casos)


save(covid_diarios, file = "datos/covid_diarios.rdata")


#—----

cuarentenas_pais %>% 
  #group_by(fecha) %>% 
  mutate(porcentaje_e = scales::rescale(porcentaje, 
                                        #from = c(0, 1), 
                                        to = c(0, 150))) %>% 
  ggplot(aes(fecha, porcentaje_e, fill = etapa)) +
  geom_area()

movilidad %>% 
  filter(is.na(region)) %>% 
  mutate(unidad = "País") %>% 
  #filter(sector %in% input$sector) %>% #filtrar sectores
  mutate(valor = media_movil(valor, "No")) %>% #media móvil
  ggplot() +
  geom_col(data = cuarentenas_pais, aes(fecha, (porcentaje_escalado), fill = etapa),
           alpha = 0.3, position = position_stack()) +
  g_base


#—----

#cuarentenas (Jara) ----

load("~/Movilidad/Google/visualizador_movilidad_google/datos/provincias_comunas.rdata")
load("datos/cuarentenas_diarias.rdata")
load("datos/comunas.rdata")




library(ggplot2)

#por provincia ----
cuarentenas_provincia <- cuarentenas %>% 
  select(fecha, etapa, comuna, provincia, region) %>% 
  #limpiar comunas, anexar población
  distinct(region, comuna, fecha, .keep_all = T) %>%
  left_join(comunas %>% select(comuna, poblacion)) %>% 
  #arrange(fecha, region) %>% 
  #calcular provincia desde comunas
  group_by(provincia, fecha, etapa) %>% 
  summarize(poblacion = sum(poblacion))

cuarentenas_provincia %>% 
  filter(provincia == "Santiago") %>% 
  group_by(fecha, provincia) %>% 
  mutate(porcentaje = poblacion/sum(poblacion)) %>% 
  ggplot(aes(fecha, porcentaje, fill = etapa)) +
  geom_col()

#por región ----
tictoc::tic()
cuarentenas_region <- cuarentenas %>% 
  select(fecha, etapa, comuna, provincia, region) %>% 
  #limpiar comunas, anexar población
  distinct(region, comuna, fecha, .keep_all = T) %>%
  left_join(comunas %>% select(comuna, poblacion)) %>% 
  #arrange(fecha, region) %>% 
  #calcular region desde comunas
  group_by(region, fecha, etapa) %>% 
  summarize(poblacion = sum(poblacion))
tictoc::toc()
  
cuarentenas_region %>% 
  filter(region == "Maule") %>% 
  group_by(fecha, region) %>% 
  mutate(porcentaje = poblacion/sum(poblacion)) %>% 
  ggplot(aes(fecha, porcentaje, fill = etapa)) +
  geom_col()

#país ----
tictoc::tic()
cuarentenas_pais <- cuarentenas %>%
  select(fecha, etapa, comuna, provincia, region) %>% 
  #limpiar comunas, anexar población
  distinct(region, comuna, fecha, .keep_all = T) %>%
  left_join(comunas %>% select(comuna, poblacion)) %>% 
  #arrange(fecha, region) %>% 
  # #calcular provincia desde comunas
  # group_by(region, provincia, fecha, etapa) %>% 
  # summarize(poblacion = sum(poblacion)) %>% 
  # #calcular region desde provincias
  # group_by(region, fecha, etapa) %>% 
  # summarize(poblacion = sum(poblacion, na.rm=T)) %>%
  #calcular país
  group_by(fecha, etapa) %>% 
  summarize(poblacion = sum(poblacion, na.rm=T))
tictoc::toc()

cuarentenas_pais %>% 
  group_by(fecha) %>% 
  mutate(porcentaje = poblacion/sum(poblacion),
         total = sum(poblacion)) %>% 
  ggplot(aes(fecha, porcentaje, fill = etapa)) +
  geom_col(width = 1, alpha = 0.6, show.legend = F) +
  scale_y_continuous(labels = function (x) paste0(x*100, "%")) +
  theme_minimal() +
  theme(text = element_text(family = "Open Sans", color = gris_oscuro),
        panel.grid = element_blank(),
        axis.title.y = element_text(family = "Open Sans", size = 10),
        axis.text.y = element_text(margin = margin(r=-5), family = "Oswald"),
        axis.title.x = element_blank(),
        legend.title = element_blank(),
        legend.position = "bottom") +
  labs(y = "Porcentaje de la población\nen cada etapa de cuarentena") +
  geom_point(aes(fecha, porcentaje, col = etapa), size = 0, alpha = 0) +
scale_x_date(date_breaks = "months", date_labels = "%b", 
             expand = expansion(mult = c(0,0))) +
  scale_fill_manual(values = rev(c("lightgreen", "yellow1", "orange", "red")),
                    aesthetics = c("color", "fill")) +
  guides(fill = guide_legend(override.aes = list(size = 3, alpha=0.6), nrow = 2)) +
guides(col = guide_legend(override.aes = list(size = 5, alpha=0.6, fill=NA, text=NA), nrow = 2))



# prueba mezcla ----
#transformar ejes para poder poner gráfico de fondo



covid_pais <- covid_diarios %>% 
  #filter(fecha >= input$fecha[1], 
  #       fecha <= input$fecha[2]) %>% 
  group_by(fecha) %>% 
  summarize(casos = sum(casos, na.rm = T))

### pais()
movilidad %>% 
  filter(is.na(region)) %>% 
  mutate(unidad = "País") %>% 
  filter(sector %in% c("Transporte público", "Viviendas")) %>% #filtrar sectores
  mutate(valor = media_movil(valor, "No")) %>% 
###
  ggplot() +
  #cuarentenas
  geom_col(data = cuarentenas_pais, aes(fecha, (porcentaje*100)+25, fill=etapa), 
           width = 1, alpha = 0.4) +
  g_base_2 +
  #limites horizontales
  #coord_cartesian(xlim = c(input$fecha[1], input$fecha[2]))
  coord_cartesian(ylim = c(0, 200), expand = 0) +
###
  #escalas y tema
  g_escalas +
  g_temas +
  #eje normal
  g_primer_eje_2 +
###
#linea de covid
geom_line(data = covid_pais, #covid_pais(), 
          aes(fecha, scales::rescale(casos, to=c(10, 190))), 
          size = 0.6, alpha=0.6) +
          #linetype = "dashed", lineend="round") +
  #g_segundo_eje
  scale_y_continuous(labels = function (x) paste0(x-100, "%"), 
                     breaks = c(-75, -50, -25, 0, 25, 50, 75)+100,
                     #eje y secundario
                     #sec.axis = sec_axis(~., #breaks = 0, 
                     #sec.axis = sec_axis(~covid_pais()$casos,
                     sec.axis = sec_axis(~ scales::rescale(., to=c(0, max(covid_pais$casos))), #from=c(0, max(.))),
                                         breaks = scales::breaks_extended(6), #breaks covid
                                         #labels = function (x) ifelse(x<0, "", x), #eliminar negativos
                                         name = "Casos activos de Covid-19"
                     ))
cuarentenas_pais

cuarentenas_region %>% 
  filter(region == "Metropolitana de Santiago") %>% 
  group_by(fecha) %>% 
  mutate(porcentaje = poblacion/sum(poblacion)) %>% 
  mutate(total = sum(porcentaje)) %>% 
  print(n=50)

cuarentenas_provincia %>% 
  arrange(desc(fecha)) %>% 
  filter(provincia == "Melipilla") %>% 
  group_by(fecha) %>% 
  mutate(porcentaje = poblacion/sum(poblacion)) %>% 
  mutate(total = sum(porcentaje)) %>% 
  print(n=50)
