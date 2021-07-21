#importar comunas
library(rvest)

#comunas de chile
comunas <- read_html("https://es.wikipedia.org/wiki/Anexo:Comunas_de_Chile") %>% 
  html_table(convert = FALSE) %>% 
  bind_rows() %>% 
  janitor::clean_names() %>% 
  select(-x3) %>% 
  rename(codigo_comuna = cut_codigo_unico_territorial) %>% 
  mutate(provincia2 = tolower(provincia),
         provincia2 = stringr::str_remove(provincia2, "\\d+ "),
         provincia2 = textclean::replace_non_ascii(provincia2)) %>% 
  mutate(poblacion = stringr::str_remove_all(poblacion2020, "\\.") %>% as.numeric()) %>% 
  rename(comuna = nombre)

save(comunas, file = "datos/comunas.rdata")