library(tidyverse)
library(echarts4r)
library(rvest)

# Importar datos ----------------------------------------------------------

Sys.setlocale(category = "LC_TIME", locale="es_ES.UTF-8") 

data <- readr::read_csv("datos/data_google.csv")
colnames(data)

data <- data %>% 
  dplyr::select(region = sub_region_1,
                provincia = sub_region_2,
                cod_provincia = iso_3166_2_code,
                fecha = date,
                ends_with(match = "_baseline")) %>% 
  tidyr::pivot_longer(cols = ends_with(match = "_baseline"), 
                      names_to = "sector", values_to = "baseline") %>% 
  dplyr::mutate(sector = stringr::str_remove(sector, "_percent_change_from_baseline"),
                sector = stringr::str_replace_all(sector, "_", " "),
                sector = dplyr::case_when(sector == "retail and recreation" ~ "Mercadería y farmacia",
                                   sector == "parks" ~ "Parques",
                                   sector == "residential" ~ "Viviendas",
                                   sector == "retail and recreation" ~ "Retail y recreación",
                                   sector == "transit stations" ~ "Transporte público",
                                   sector == "workplaces" ~ "Lugar de trabajo"),
                region = dplyr::recode(region, "Santiago Metropolitan Region" = "Metropolitana de Santiago",
                                       "Aysén" =  "Aysén del Gral. C. Ibáñez del Campo",
                                       "O'Higgins" = "Lib. Gral. Bernardo O'Higgins",
                                       "Araucania" = "La Araucanía",
                                       "Bio Bio" = "Biobío",
                                       "Magallanes and Chilean Antarctica" = "Magallanes"),
                provincia = gsub(pattern = " Province", replacement = "", provincia)) 

comuna_scrap <- rvest::read_html("https://es.wikipedia.org/wiki/Anexo:Comunas_de_Chile") %>% 
             rvest::html_table(header = TRUE) %>% 
             dplyr::bind_rows() %>% 
             janitor::clean_names() %>% 
             dplyr::select(-c(3)) %>% 
             dplyr::mutate(provincia2 = tolower(provincia),
                           provincia2 = stringr::str_remove(provincia2, "\\d+ "),
                           provincia2 = textclean::replace_non_ascii(provincia2)) 
            
data_join <- data %>% 
             dplyr::select(provincia) %>% 
             dplyr::distinct() %>% 
             tidyr::drop_na() %>% 
             dplyr::mutate(provincia2 = stringr::str_to_lower(provincia),
                           provincia2 = textclean::replace_non_ascii(provincia2)) %>% 
             dplyr::left_join(comuna_scrap %>% 
                              dplyr::rename(comuna = nombre) %>% 
                              dplyr::select(region, comuna, provincia, provincia2), 
                              by = c("provincia2" = "provincia2")) %>% 
  dplyr::select(provincia.x, region, comuna) %>% 
  dplyr::rename(provincia = provincia.x)

save(data, file = "datos.rdata")
save(data, file = "data_join.rdata")
readr::write_csv(x= data, file = "data.csv", col_names = TRUE, )

# Datos faltantes ---------------------------------------------------------

load(file = "datos.rdata")
any(!complete.cases(data))
purrr::map_dbl(data, .f = function(x){sum(is.na(x))})

# Visualización -----------------------------------------------------------


avg <- list(
  type = "average",
  name = "AVG"
)

data %>% 
  dplyr::group_by(sector) %>% 
  dplyr::filter(provincia == "Santiago") %>% 
  echarts4r::e_chart(x = fecha) %>% 
  echarts4r::e_line(serie = baseline, smooth = TRUE, symbol_size = 1,
                    coord_system = "cartesian2d") %>% 
  echarts4r::e_tooltip(trigger = "axis", axisPointer = list(type = "cross"),
                         textStyle = list(fontFamily="sans-serif",
                                        fontSize=10)) %>% 
  echarts4r::e_toolbox_feature(feature = "saveAsImage", title = "Guardar gráfico") %>% 
  echarts4r::e_toolbox_feature("dataZoom", title = list(zoom = "Ampliar gráfico", 
                                                        back = "Deshacer")) %>% 
  echarts4r::e_toolbox_feature("restore", title = 'Restaurar') %>% 
  echarts4r::e_title("Índice de movilidad Google", 
          left = "center", top = 2, 
          textStyle = list(fontSize = 20)) %>% 
  echarts4r::e_theme("royal") %>% 
  echarts4r::e_mark_line(data = avg, title = "Promedio")  %>% 
  echarts4r::e_datazoom()  %>% 
  echarts4r::e_timeline_opts(autoPlay = TRUE, top = 40) %>% 
  echarts4r::e_grid(right = "15%") %>%  
  echarts4r::e_legend(orient = "vertical", right = "5", top = "15%") %>% 
  echarts4r::e_mark_point(data = list(
    xAxis = as.Date("2020-05-27"),
    yAxis = 7,
    value = "Cuarentena 1"
  )) 
  
  








