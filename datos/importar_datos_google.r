#setwd("~/Movilidad/Google/visualizador_movilidad_google/")
setwd("~/Movilidad/Google/visualizador_movilidad_google")

suppressPackageStartupMessages(library(dplyr))

# #descargar archivo completo
# download.file("https://www.gstatic.com/covid19/mobility/Global_Mobility_Report.csv", 
#               destfile = paste0("Reporte movilidad global/", 
#                                 "movilidad_google_", lubridate::today(), ".csv"))

#descargar carpeta de reportes nacionales
cat("DESCARGA...", fill=T)
download.file("https://www.gstatic.com/covid19/mobility/Region_Mobility_Report_CSVs.zip",
              destfile = paste0("datos/movilidad_regional/reporte_regional.zip"))

#descomprimir sólo chile
cat("DESCOMPRESIÓN...", fill=T)
unzip(zipfile = paste0("datos/movilidad_regional/reporte_regional.zip"),
      files = "2021_CL_Region_Mobility_Report.csv",
      exdir = paste0("datos/movilidad_regional/", lubridate::today()))


#cargar archivo
cat("CARGA...", fill=T)
movilidad <- readr::read_csv(paste0("datos/movilidad_regional/", lubridate::today(), 
                       "/2021_CL_Region_Mobility_Report.csv"))
                       #col_types = readr::cols())

movilidad_2020 <- readr::read_csv(paste0("datos/movilidad_regional/", "2020",
                                         "/2020_CL_Region_Mobility_Report.csv"))
#max(movilidad$date)
# #filtrar solo chile
# movilidad <- movilidad %>% 
#   filter(country_region == "Chile")

# #reescribir archivo completo
# readr::write_csv(movilidad, file = paste0("Reporte movilidad global/", 
#                                           "movilidad_google_", lubridate::today(), ".csv"))

# movilidad <- readr::read_csv(file = paste0("Reporte movilidad global/", 
#                                           "movilidad_google_", lubridate::today(), ".csv"))

#limpiar ----
cat("LIMPIEZA...", fill=T)

movilidad <- movilidad %>% 
  #anexar datos de 2020
  bind_rows(movilidad_2020) %>% 
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

#guardar
save(movilidad, file = "~/Movilidad/Google/visualizador_movilidad_google/movilidad.rdata")
