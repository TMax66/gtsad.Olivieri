library(tidyverse)
library(openxlsx)
library(readxl)
library(here)

dati <- read_excel(here("data", "dati.xlsx"))


dt <- dati %>%
  filter(is.na(esito)) %>%
  distinct() %>% 
  
  group_by(specie, stadio) %>% 
  mutate(stadio = casefold(stadio), 
         stadio = ifelse(stadio %in% c("femmina", "maschio"), "adulto", stadio )) %>% 
  
  count() 
  


  
  
  
  
  
  
  




#    

# x <- dati %>% 
#   mutate(new_esito = str_remove(`Esito precodificato`,  '.*(?=con)'), 
#          new_esito = str_remove(new_esito,'con ' ))
# 

 