library(tmap)
library(sf)
library(leaflet)
library(rmapshaper)
library(fst)

dt <- read_fst(here("data", "pat.fst"))

# comuni <- st_read("https://raw.githubusercontent.com/openpolis/geojson-italy/master/geojson/limits_IT_municipalities.geojson")
# 
# province <- st_read("https://raw.githubusercontent.com/openpolis/geojson-italy/master/geojson/limits_IT_provinces.geojson")
# 
# regioni <- st_read("https://raw.githubusercontent.com/openpolis/geojson-italy/master/geojson/limits_IT_regions.geojson")
# 
# saveRDS(
#   st_make_valid(comuni), file = "comuni.rds")
# saveRDS(
#   st_make_valid(province), file = "province.rds")
# saveRDS(
#   st_make_valid(regioni), file = "regioni.rds")


regioni <- readRDS(here("data", "regioni.rds"))
comuni <- readRDS(here("data", "comuni.rds"))

dt %>% 
  group_by(comune, PatCat) %>% 
  count() %>% 
  group_by(comune) %>% 
  nest() %>% 
  mutate(tot = map(data, ~sum(.$n,  na.rm = TRUE))) %>% 
  unnest(cols = c(data, tot)) %>%     
  mutate(risk = 100*n/tot) %>% 
  filter(PatCat == "Pos") %>% 
  select(-PatCat)-> risk

riskcom <- as.vector(risk$comune)


comuni %>% 
  filter(name %in% riskcom) %>% 
  left_join(risk, by = c("name" = "comune")) -> risk_comuni



tm_shape(regioni %>% 
           filter(reg_name == "Lombardia"))+tm_borders()+
  tm_shape(comuni %>%
             filter(reg_name == "Lombardia"))+tm_borders()+
  tm_shape(province)+tm_borders( "blue")+
  tm_shape(risk_comuni)+tm_borders()+tm_fill("risk")+
  
  tm_layout(main.title = "Prevalence of Ticks with tick-borne pathogens",
            main.title.size = 0.9, 
            legend.outside = TRUE, frame = FALSE, 
            legend.text.size = 0.8, 
            legend.title.size = 1,
            #legend.position = c("right","top"),
            legend.bg.color = "white",
            legend.bg.alpha = 1)+
  tm_scale_bar(breaks = c(0, 10, 30), text.size = .1,position = "right")+
  tm_compass(type = "8star", position = c("right", "bottom"), size = 0.8) 


#+tm_fill("risk", palette = "Blues")+tm_borders()

 

  
