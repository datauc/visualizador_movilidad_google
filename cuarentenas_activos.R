# Plan paso a paso --------------------------------------------------------

cuarentena <- readr::read_csv("https://github.com/MinCiencia/Datos-COVID19/raw/master/output/producto74/paso_a_paso.csv",
                              col_types = readr::cols()) %>% 
  tidyr::pivot_longer(starts_with("20"), names_to = "fecha", values_to = "etapa_n") %>% 
  mutate(etapa = case_when(etapa_n == 1 ~ "Cuarentena",
                           etapa_n == 2 ~ "Transición",
                           etapa_n == 3 ~ "Preparación",
                           etapa_n == 4 ~ "Apertura inicial",
                           etapa_n == 5 ~ "Apertura avanzada")) %>% 
  mutate(etapa = forcats::fct_reorder(etapa, etapa_n)) %>% 
  mutate(fecha = lubridate::ymd(fecha)) %>% 
  left_join(provincias_comunas %>% select(provincia, comuna, codigo_comuna), by = "codigo_comuna")

cuarentena %>% 
  dplyr::select(-c(codigo_region, codigo_comuna, comuna_residencia)) %>% 
  dplyr::filter(fecha == max(fecha),
                comuna == "La Florida") %>% 
  dplyr::distinct(etapa)

# Casos activos -----------------------------------------------------------

activos <- readr::read_csv("https://coronavirus-api.mat.uc.cl/casos_activos_sintomas_comuna")

activos %>% 
  dplyr::filter(fecha == max(fecha, na.rm = TRUE),
                comuna == "Arica") %>% 
  dplyr::select(fecha, region, comuna, poblacion, casos) 

prueba <- readxl::read_excel(path = "datos/activos.xlsx", col_names = TRUE)

prueba %>% 
  janitor::row_to_names(1) %>% 
  dplyr::select(-all_of(1)) %>% 
  dplyr::filter(fecha == max(fecha)) %>% 
  dplyr::distinct(casos)

activos %>% 
  dplyr::select(!-c(poblacion)) %>% 
  dplyr::slice_head()


cor <- movilidad
cor <- tidyr::pivot_wider(data = cor, names_from = sector, values_from = valor)
cor <- cor %>% dplyr::select_if(is.numeric)
cor <- round(cor(na.omit(cor)), 2)

ggcorrplot::ggcorrplot(corr = cor, type = "upper", show.legend = TRUE, 
                       lab = TRUE) + viridis::scale_fill_viridis()










