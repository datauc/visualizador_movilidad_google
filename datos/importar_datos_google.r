#descargar archivo completo
download.file("https://www.gstatic.com/covid19/mobility/Global_Mobility_Report.csv", 
              destfile = paste0("datos/Reporte movilidad global/", 
                                "movilidad_google_", lubridate::today(), ".csv"))

#cargar archivo
movilidad <- readr::read_csv(paste0("datos/Reporte movilidad global/", 
                       "movilidad_google_", lubridate::today(), ".csv"),
                       col_types = cols())

library(dplyr)

#filtrar solo chile
movilidad <- movilidad %>% 
  filter(country_region_code == "CL")

#reescribir archivo completo
readr::write_csv(movilidad, file = paste0("datos/Reporte movilidad global/", 
                                          "movilidad_google_", lubridate::today(), ".csv"))

#limpiar
movilidad <- movilidad %>% 
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
save(movilidad, file = "movilidad.rdata")