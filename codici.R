library(tidyverse)
library(openxlsx)
library(readxl)

dati <- read_excel("data/dati.xlsx")


dt <- dt %>% 
  filter( `Prova - Singola` = !str_detect(`Prova - Singola`, "Identificazione"))
