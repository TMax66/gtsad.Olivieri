library(tidyverse)
library(openxlsx)
library(readxl)

dati <- read_excel("data/dati.xlsx")


dt <- dt %>% 
  filter( `Prova - Singola` = !str_detect(`Prova - Singola`, "Identificazione"))


x <- dati %>% 
  mutate(new_esito = str_remove(`Esito precodificato`,  '.*(?=con)'), 
         new_esito = str_remove(new_esito,'con ' ))


 