library(dplyr)
library(ggplot2)

Sys.setlocale(category = "LC_TIME", locale="es_ES.UTF-8") #Meses en español

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
  mutate_if(is.character, as.factor)

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

#selector de comunas que entregue la provincia