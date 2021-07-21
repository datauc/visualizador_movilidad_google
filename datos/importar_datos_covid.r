#importar datos de covid activos diariamente
suppressPackageStartupMessages(library(dplyr))

setwd("~/Movilidad/Google/visualizador_movilidad_google")

#casos activos ----
covid_diarios <- readr::read_csv("https://coronavirus-api.mat.uc.cl/casos_activos_sintomas_comuna",
                                 col_types = readr::cols()) %>% 
  select(fecha, region, comuna, casos)

save(covid_diarios, file = "datos/covid_diarios.rdata")

max(covid_diarios$fecha)

#cuarentenas ----
cuarentenas <- readr::read_csv("https://github.com/MinCiencia/Datos-COVID19/raw/master/output/producto74/paso_a_paso.csv",
                  col_types = readr::cols()) %>% 
    tidyr::pivot_longer(starts_with("20"), names_to = "fecha", values_to = "etapa_n") %>% 
    mutate(etapa = case_when(etapa_n == 1 ~ "Cuarentena",
                             etapa_n == 2 ~ "Transición",
                             etapa_n == 3 ~ "Preparación",
                             etapa_n == 4 ~ "Apertura inicial",
                             etapa_n == 5 ~ "Apertura avanzada")) %>% 
    mutate(etapa = forcats::fct_reorder(etapa, etapa_n)) %>% 
    mutate(fecha = lubridate::ymd(fecha)) %>% 
    #anexar provincias y comunas a partir del codigo de comuna
    left_join(provincias_comunas %>% select(region, provincia, comuna, codigo_comuna), by = "codigo_comuna") %>% 
  select(fecha, region, provincia, comuna, etapa, etapa_n)

save(cuarentenas, file = "datos/cuarentenas_diarias.rdata")

max(cuarentenas$fecha)