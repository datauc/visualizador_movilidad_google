library(dplyr)
library(ggplot2)

#importar y ordenar
reporte <- readr::read_csv("Region_Mobility_Report_CSVs/2020_CL_Region_Mobility_Report.csv") %>% 
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
  mutate_if(is.character, as.factor)

reporte %>% count(region)
reporte %>% count(sector) %>% select(1)

#graficar
reporte %>% 
  filter(region == "Santiago Metropolitan Region") %>% 
  ggplot() +
  geom_line(aes(fecha, valor, col = sector)) +
  facet_wrap(~provincia)

#selector de comunas que entregue la provincia