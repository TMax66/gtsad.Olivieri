
#library(rstanarm)
#library(bayesplot)
#library(bayestestR)
#library(see)
#library(sjPlot)
#library(hrbrthemes)
#library(ggdist)
#library(bayesrules)
library(binom)
library(forestplot)


dati <- read_excel(here("data", "dati.xlsx"))



# dt <- dati %>%
#   filter(is.na(esito)) %>%
#   distinct() %>% 
#   
#   group_by(specie, stadio) %>% 
#   mutate(stadio = casefold(stadio), 
#          stadio = ifelse(stadio %in% c("femmina", "maschio"), "adulto", stadio )) %>% 
#   
#   count() 
  

dtpat <- dati %>%
  filter(!is.na(esito)) %>%  
 # distinct() %>%
  mutate(#stadio = casefold(stadio),
         stadio = ifelse(stadio %in% c("femmina", "maschio", "Maschio", "Femmina"), "Adult", 
                         ifelse(stadio == "larva", "Larvae", 
                                ifelse(stadio == "ninfa", "Nymphae", stadio))), 
         stagione = recode(mese, 
                           "dicembre" = "Winter",
                           "gennaio" = "Winter", 
                           "febbraio" = "Winter", 
                           "marzo" = "Spring",
                           "aprile" = "Spring",
                           "maggio" = "Spring", 
                           "giugno" = "Summer", 
                           "luglio" = "Summer", 
                           "agosto" = "Summer", 
                           "settembre" = "Autumn",
                           "ottobre" = "Autumn", 
                           "novembre" = "Autumn"),
         stagione = factor(stagione, levels = c("Spring", "Summer", "Autumn", "Winter")),
         stagioneanno = paste0(annoreg,stagione)) %>% 
  filter(stadio != "n.d.") %>% 
  mutate(specie = stringr::str_to_sentence(specie)) 


# pat <- dtpat %>% 
#   mutate(pat = ifelse(esito != "Non dimostrata presenza", "1", "0")) %>%  View()
#   pivot_wider(names_from = "prova", values_from = "pat", values_fill = '0' ) %>% 
#   select(annoreg,nconfcamp, stadio, specie, comune, provincia, stagione, 
#          annoreg, stagioneanno,  altitudine, 17:22 ) %>% 
#   mutate(across(c(10:13), as.numeric)) %>% View()
#   group_by(annoreg,nconfcamp, stadio, specie, comune, provincia, stagione, stagioneanno,altitudine) %>% 
#   summarise(across(where(is.numeric), ~ sum(.x, na.rm = TRUE))) %>% ungroup() %>% 
#   rowwise() %>% 
#   mutate(Pat = sum(c_across(c(10:13)), na.rm = T),
#          Pat2 = ifelse(Pat == 0, 0, 1), 
#          PatCat = ifelse(Pat2 == 0 , "Neg", "Pos"),
#          stadio = factor(stadio, levels =c("Larvae", "Nymphae",  "Adult"))
#          )%>%
#   filter(specie == "Ixodes ricinus")
 

  pat <- dtpat %>% select(annoreg,nconfcamp, stadio, specie, comune, provincia, stagione, 
                          annoreg, stagioneanno,  altitudine, prova, esito) %>% 
    mutate(pat = ifelse(esito != "Non dimostrata presenza", "1", "0")) %>%  
  pivot_wider(names_from = "prova", values_from = "pat", values_fill = '0' ) %>% 
    # select(annoreg,nconfcamp, stadio, specie, comune, provincia, stagione, 
    #        annoreg, stagioneanno,  altitudine, 17:22 ) %>% 
    mutate(across(c(11:16), as.numeric)) %>% 
    group_by(annoreg,nconfcamp, stadio, specie, comune, provincia, stagione, stagioneanno,altitudine) %>% 
    summarise(across(where(is.numeric), ~ sum(.x, na.rm = TRUE))) %>% 
    ungroup() %>% 
    rowwise() %>% 
    mutate(Pat = sum(c_across(c(10:15)), na.rm = T),
           Pat2 = ifelse(Pat == 0, 0, 1), 
           PatCat = ifelse(Pat2 == 0 , "Neg", "Pos"),
           stadio = factor(stadio, levels =c("Larvae", "Nymphae",  "Adult"))
    )
    
   # filter(specie == "Ixodes ricinus") 

    
# codici per tabella 1----
dtpat %>% select(annoreg,nconfcamp, stadio, specie, comune, provincia, stagione, 
                          annoreg, stagioneanno,  altitudine, prova, esito) %>% 
    mutate(pat = ifelse(esito != "Non dimostrata presenza", "pos", "neg")) %>% 
  filter(prova != "Sequenziamento acidi nucleici", 
         prova != "Identificazione zecche dell'Ordine Ixodida") %>% 
  group_by(specie, prova, pat) %>% 
  count() %>%  
  pivot_wider(names_from = pat, values_from = n, values_fill = 0) %>% 
  mutate(tested = neg+pos, 
         specie = recode(specie, 
                         "N.d." = "nd"), 
         prova2 = str_remove(prova, ": agente eziologico"), 
                prova2 = str_remove(prova2, "\\."), 
         specie = str_remove(specie, "\\.")) -> prev

prev_overall <- prev %>% 
  group_by(prova) %>% 
  summarise(pos = sum(pos), 
            tested = sum(tested))
  

 
resbinom <- binom.bayes(
  x = prev$pos, n = prev$tested,
  type = "central", conf.level = 0.95, tol = 1e-9)

options(digits = 1)
prev %>%
  select(specie, prova2, pos, tested) %>% 
  bind_cols(
    resbinom %>%
      select(mean, lower, upper)
  ) %>%  ungroup() %>% 
  mutate(across(6:8, ~ .x*100)) %>%  excel()


binom.bayes(
  x = prev_overall$pos, n = prev_overall$tested,
  type = "central", conf.level = 0.95, tol = 1e-9)-> overall
 
prev_overall %>% 
  bind_cols(
    overall %>% 
      select(mean, lower, upper)
  ) %>%  
  mutate(across(4:6, ~ .x*100), 
         across(4:6, ~ round(.x, 2))) %>% View() #excel()




#Prevalenza positività zecche by stadio e stagione----

pat %>% 
  select(stadio, PatCat, stagione) %>% 
  group_by(stagione,stadio, PatCat) %>% 
  count() %>% 
  mutate(stadio = as.character(stadio), 
    stadio = replace_na(stadio, "nd")) %>% 
  pivot_wider(names_from = PatCat, values_from = n, values_fill = 0) %>%  
  mutate(n = Neg+Pos, 
         perc = round(100*Pos/n, 2)) %>% 
  select(-Neg) -> prevall
  # adorn_totals(,,,, -perc) %>% 
  # mutate(perc = ifelse(stadio == "Total", round(100*Pos/n, 2), perc))

resbinom <- binom.bayes(
  x = prevall$Pos, n = prevall$n,
  type = "central", conf.level = 0.95, tol = 1e-9)

prevall %>% 
  bind_cols(
    resbinom %>% 
      select(mean, lower, upper)
  ) %>%   ungroup()->   tabOverall
  # mutate(across(6:8, ~ .x*100), 
  #        across(6:8, ~ round(.x, 2))) 
  # 
  
### forestplot overall-----
tabOverall %>% tibble() %>% 
 mutate (perc = as.numeric(perc),
   Prevalence = 100*mean, 
         liminf = 100*lower, 
         limsup = 100*upper,  
  across(where(is.double), round,2)) %>%  
  forestplot(labeltext = c(  stagione,stadio,   Pos,
                            n, perc,  Prevalence, liminf,limsup),
             clip = c(0,1),
             xlog = FALSE, 
             xlab= "Prevalence",
             title = "Ticks borne pathogens in Ticks",
             # txt_gp = fpTxtGp(cex=0.90),
             # axes = gpar(cex = 0.9),
             txt_gp = fpTxtGp(ticks=gpar(cex=1), 
                              xlab = gpar(cex=1)))  %>% 
  fp_add_header(stagione = "Season",
                stadio = "Stady",
                Pos = "N.positive" ,
                n = "N.tested"  ,
                perc = "% Positive", 
                Prevalence = "Prevalence",
                liminf = "95%CI liminf", 
                limsup = "95%CI limsup") 

  




 
  
#Ixodes ricinus----

## prevalenza pos borrelia b by stadio e stagione----
pat %>% 
  filter(specie == "Ixodes ricinus") %>% 
  select(stadio, `Borrelia burgdorferi sensu lato complex: agente eziologico`, stagione) %>% 
  rename(Borrelia = `Borrelia burgdorferi sensu lato complex: agente eziologico`) %>%  
  mutate(Borrelia = ifelse(Borrelia == 0, "Neg", "Pos")) %>%   
  group_by(stagione,stadio, Borrelia) %>% 
  count() %>%  
  mutate(stadio = as.character(stadio), 
         stadio = replace_na(stadio, "nd")) %>% 
  pivot_wider(names_from = Borrelia, values_from = n, values_fill = 0) %>%  
  mutate(n = Neg+Pos, 
         perc = round(100*Pos/n, 2)) %>% 
  select(-Neg) -> prevall

resbinom <- binom.bayes(
  x = prevall$Pos, n = prevall$n,
  type = "central", conf.level = 0.95, tol = 1e-9)

prevall %>% 
  bind_cols(
    resbinom %>% 
      select(mean, lower, upper)
  ) %>%   ungroup()->   tabOverall
# mutate(across(6:8, ~ .x*100), 
#        across(6:8, ~ round(.x, 2))) 
# 

### forestplot borrelia-----
tabOverall %>% tibble() %>% 
  mutate (perc = as.numeric(perc),
          Prevalence = 100*mean, 
          liminf = 100*lower, 
          limsup = 100*upper,  
          across(where(is.double), round,2)) %>%  
  forestplot(labeltext = c(  stagione,stadio,   Pos,
                             n, perc,  Prevalence, liminf,limsup),
             clip = c(0,1),
             xlog = FALSE, 
             xlab= "Prevalence",
             title = "Borrelia burgdorferi in Ixodes ricinus",
             # txt_gp = fpTxtGp(cex=0.90),
             # axes = gpar(cex = 0.9),
             txt_gp = fpTxtGp(ticks=gpar(cex=1), 
                              xlab = gpar(cex=1)))  %>% 
  fp_add_header(stagione = "Season",
                stadio = "Stady",
                Pos = "N.positive" ,
                n = "N.tested"  ,
                perc = "% Positive", 
                Prevalence = "Prevalence",
                liminf = "95%CI liminf", 
                limsup = "95%CI limsup") 







## prevalenza pos rickettsia by stadio e stagione----


## prevalenza pos borrelia b by stadio e stagione----
pat %>% 
  filter(specie == "Ixodes ricinus") %>% 
  select(stadio, `Rickettsia spp.: agente eziologico`, stagione) %>% 
  rename(Rickettsia = `Rickettsia spp.: agente eziologico`) %>%  
  mutate(Rickettsia = ifelse(Rickettsia == 0, "Neg", "Pos")) %>%   
  group_by(stagione,stadio, Rickettsia) %>% 
  count() %>%  
  mutate(stadio = as.character(stadio), 
         stadio = replace_na(stadio, "nd")) %>% 
  pivot_wider(names_from = Rickettsia, values_from = n, values_fill = 0) %>%  
  mutate(n = Neg+Pos, 
         perc = round(100*Pos/n, 2)) %>% 
  select(-Neg) -> prevall

resbinom <- binom.bayes(
  x = prevall$Pos, n = prevall$n,
  type = "central", conf.level = 0.95, tol = 1e-9)

prevall %>% 
  bind_cols(
    resbinom %>% 
      select(mean, lower, upper)
  ) %>%   ungroup()->   tabOverall



### forestplot borrelia-----
tabOverall %>% tibble() %>% 
  mutate (perc = as.numeric(perc),
          Prevalence = 100*mean, 
          liminf = 100*lower, 
          limsup = 100*upper,  
          across(where(is.double), round,2)) %>%  
  forestplot(labeltext = c(  stagione,stadio,   Pos,
                             n, perc,  Prevalence, liminf,limsup),
             clip = c(0,1),
             xlog = FALSE, 
             xlab= "Prevalence",
             title = "Rickettsia spp in Ixodes ricinus",
             # txt_gp = fpTxtGp(cex=0.90),
             # axes = gpar(cex = 0.9),
             txt_gp = fpTxtGp(ticks=gpar(cex=1), 
                              xlab = gpar(cex=1)))  %>% 
  fp_add_header(stagione = "Season",
                stadio = "Stady",
                Pos = "N.positive" ,
                n = "N.tested"  ,
                perc = "% Positive", 
                Prevalence = "Prevalence",
                liminf = "95%CI liminf", 
                limsup = "95%CI limsup") 


#Mappe----

dt <- read_fst(here("data", "pat.fst"))
regioni <- readRDS(here("data", "regioni.rds"))
province <- readRDS(here("data", "province.rds"))
comuni <- readRDS(here("data", "comuni.rds"))


dt %>% 
  group_by(comune, PatCat) %>% 
  count() %>% 
  group_by(comune) %>% 
  nest() %>% 
  mutate(tot = map(data, ~sum(.$n,  na.rm = TRUE))) %>% 
  unnest(cols = c(data, tot)) %>%     
  #mutate(risk = 100*n/tot) %>% 
  filter(PatCat == "Pos") %>%  
  select(-PatCat) -> pat

resbinom <- binom.bayes(x = pat$n, n = pat$tot, type = "central", conf.level = 0.95, tol = 1e-9 )


pat %>% 
  bind_cols(resbinom) %>% 
  select(comune, risk = mean) -> risk


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





#Mappa ixodes ricinus pos a borrelia burgdoferii
#
dt %>% filter(specie == "Ixodes ricinus") %>% 
  select(comune,  borrelia = `Borrelia burgdorferi sensu lato complex: agente eziologico`) %>%  
group_by(comune, borrelia) %>% 
  count() %>% 
  group_by(comune) %>% 
  nest() %>% 
  mutate(tot = map(data, ~sum(.$n,  na.rm = TRUE))) %>% 
  unnest(cols = c(data, tot)) %>%      
  #mutate(risk = 100*n/tot) %>% 
  filter(borrelia == 1) %>%  
  select(-borrelia) -> pat

resbinom <- binom.bayes(x = pat$n, n = pat$tot, type = "central", conf.level = 0.95, tol = 1e-9 )


pat %>% 
  bind_cols(resbinom) %>% 
  select(comune, risk = mean) -> risk


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
  tm_graticules() +
  
  tm_layout(main.title = "",
            main.title.size = 0.9, 
            legend.outside = TRUE, frame = FALSE, 
            legend.text.size = 0.6, 
            legend.title.size = 1,
            
            #legend.position = c("right","top"),
            legend.bg.color = "white",
            legend.bg.alpha = 1) -> mappa1
  # tm_scale_bar(breaks = c(0, 10, 20), position = c("right", "top"))+
  # tm_compass(type = "8star",  position = c("right", "top")) 


italymap <- tm_shape(regioni)+tm_borders()+
  tm_shape(regioni %>% 
             filter(reg_name == "Lombardia"))+tm_borders(col = "blue")




mappa1
print(italymap, vp = grid::viewport(0.54, 0.2, width = 0.4, height = 0.2))
tmap_save(mappa1, "mappa1.png")


## Mappa Ixodes ricinus pos a rickettsie

dt %>% filter(specie == "Ixodes ricinus") %>% 
  select(comune,  rickettsia = `Rickettsia spp.: agente eziologico`) %>%  
  group_by(comune, rickettsia) %>% 
  count() %>% 
  group_by(comune) %>% 
  nest() %>% 
  mutate(tot = map(data, ~sum(.$n,  na.rm = TRUE))) %>% 
  unnest(cols = c(data, tot)) %>%      
  #mutate(risk = 100*n/tot) %>% 
  filter(rickettsia == 1) %>%  
  select(-rickettsia) -> pat

resbinom <- binom.bayes(x = pat$n, n = pat$tot, type = "central", conf.level = 0.95, tol = 1e-9 )


pat %>% 
  bind_cols(resbinom) %>% 
  select(comune, risk = mean) -> risk


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
  
  tm_layout(main.title = "",
            main.title.size = 0.8, 
            legend.outside = TRUE, frame = FALSE, 
            egend.text.size = 0.6, 
            legend.title.size = 1,
            #legend.position = c("right","top"),
            legend.bg.color = "white",
            legend.bg.alpha = 1) -> mappa2
  # tm_scale_bar(breaks = c(0, 10, 30), text.size = .1,position = "right")+
  # tm_compass(type = "8star", position = c("right", "bottom"), size = 0.8) 

  tmap_save(mappa2, "mappa2.png")


italymap <- tm_shape(regioni)+tm_borders()+
  tm_shape(regioni %>% 
             filter(reg_name == "Lombardia"))+tm_borders(col = "blue")
  

 

mappa2
print(italymap, vp = grid::viewport(0.54, 0.2, width = 0.4, height = 0.2))





# CANTINA----

# sim <- function(p, t){
#   data.frame(pi = rbeta(10000, 0.5+p, 0.5+(t-p)))}
# 
# 
# 
# prev %>% 
#   split(., list(.$specie, .$prova), drop = TRUE) %>%  
#   map(~sim(.$pos, .$tested))-> SIM
# 
#  
# 
# 
# SIM %>% 
#   unlist() %>%  data.frame() %>%  
#   rownames_to_column() %>%   
#   rename("pi" = ".") %>% 
#   separate(rowname, c("Specie", "Pathogen", "p"), sep = "\\." ) -> simPrevalence
#   
#     
# simPrevalence %>% 
#   group_by(Specie, Pathogen) %>% 
#   summarise(medianP = median(pi), 
#           liminf = quantile(pi, p = 0.025),
#           limsup = quantile(pi, p = 0.975)) %>% excel()
# 
# 
# library(ggridges)
# library(grid)
# 
# p <- simPrevalence %>% 
#   filter(Specie == "Ixodes ricinus") %>% 
#   ggplot()+
#   aes(x = pi, y = Pathogen)+
#   geom_density_ridges(scale=1, rel_min_height= 0.01)
#   # facet_wrap(~Specie, scales = "free")+
#   # theme(strip.text.y = element_blank())
# 
# 
# gt <- ggplotGrob(p)
# grid.draw(gt)

# library(binom)
#   
# options(digits=2)
# 
# resbinom <- binom.bayes(
#   x = prev$pos, n = prev$tested,
#   type = "highest", conf.level = 0.95, tol = 1e-9)
# 
# prev %>% 
#   select(-neg) %>% 
#   bind_cols(
#     resbinom %>% 
#       select(mean, lower, upper)
#   ) %>% 
#   
#   filter(prova == "Borrelia burgdorferi sensu lato complex: agente eziologico") %>% 
#   View()


# pat %>% 
#   select(stadio, PatCat) %>%  
#   group_by(stadio, PatCat) %>% 
#   count() %>% 
#   pivot_wider(names_from = "PatCat", values_from = "n") %>% 
#   mutate(tot= Neg+Pos, 
#          prev = Pos/tot)
  


# filter(stagione != "Winter")
  
  
# 
# dt %>% 
#   group_by(stadio, PatCat) %>% 
#   count()
# 
# 
# 
# resbinomS <- binom.bayes(
#   x = prevS$N, n = prevS$Totale,
#   type = "highest", conf.level = 0.95, tol = 1e-9)






 
  
#glm patogeni--con rnstan
  

 


# t_prior <- student_t(df = 7, location = 0, scale = 2.5)
# 
# fit0 <- stan_glm(Pat2 ~  stadio,   data = pat,family = binomial(link = "logit"))
# fit1 <- stan_glm(Pat2 ~   stadio+stagione,   data = pat,family = binomial(link = "logit"))
# fit2<- stan_glm(Pat2 ~   stadio*stagione,   data = pat,family = binomial(link = "logit"))
# #fit3 <- stan_glm(Pat2 ~   stadio+stagioneanno,   data = pat,family = binomial(link = "logit"))
# #fit4 <- stan_glm(Pat2 ~   stadio+stagione+annoreg,   data = pat,family = binomial(link = "logit"))
# fit3 <- stan_glm(Pat2 ~   stadio+altitudine,   data = pat,family = binomial(link = "logit"))
# fit4 <- stan_glm(Pat2 ~   altitudine,   data = pat,family = binomial(link = "logit"))
# fit5 <- stan_glm(Pat2~ altitudine*stadio, data = pat, family = binomial(link = "logit"))
# fit6 <- stan_glm(Pat2~ altitudine+stadio+stagione, data = pat, family = binomial(link = "logit"))
# #We recommend calling 'loo' again with argument 'k_threshold = 0.7'
# loo0 <- loo(fit0, k_threshold = 0.7)
# loo1 <- loo(fit1, k_threshold = 0.7)
# loo2 <- loo(fit2, k_threshold = 0.7)
# loo3 <- loo(fit3, k_threshold = 0.7)
# loo4 <- loo(fit4, k_threshold = 0.7)
# loo5 <- loo(fit5, k_threshold = 0.7)
# loo6 <- loo(fit6, k_threshold = 0.7)
# 
# x <- loo_compare(loo0, loo1, loo2, loo3, loo4, loo5, loo6)
# 
# 
# # print(x)
# # library(knitr)
# # library(kableExtra)
# # 
# # options(digits = 2)
# # kbl(x) %>% as.
# #   rowid_to_column(var = "model") %>% View()
# #   kable_styling()
# 
# 
# fit0 <- stan_glm(Pat2 ~  stadio,   data = pat,family = binomial(link = "logit"))
# 
# 
# 
# posterior <- as.matrix(fit6)
# posterior %>% as_tibble() %>% 
#   pivot_longer( cols = 1:7, names_to = "factors", values_to = "est") %>%   
#   mutate(factors = str_remove(factors,  "stadio" )) %>% 
#          # factors = factor(factors, levels = c("Adult", "Nymphae", "Intercept")),
#          # est = invlogit(est)) %>% 
#   ggplot()+
#   aes(y = factors, x = est)+
#   stat_halfeye() 
# 
# posterior %>% as_tibble() %>%  
#   
#   mutate(
#          # stadioAdult = invlogit(stadioAdult), 
#          # stadioNymphae = invlogit(stadioNymphae), 
#          # stadioLarvae = invlogit(`(Intercept)`), 
#     stadioLarvae = `(Intercept)`,
#          "Nymphae vs Larvae" =    stadioNymphae - stadioLarvae, 
#          "Adult vs Larvae" =  stadioAdult - stadioLarvae , 
#          "Adult vs Nymphae" = stadioAdult - stadioNymphae) %>% 
#   
#   
#   select(5:7) %>% 
#   
#   
#   pivot_longer( cols = 1:3, names_to = "factors", values_to = "est") %>% View()
#   
#   
#   ggplot()+
#   aes(y = factors, x = est)+
#   stat_halfeye() 
# 
# 
# 
# 
# 
# 
# #library(tidybayes)
# library(performance)
# 
# 
# tfit <- describe_posterior(
#   fit0,
#   centrality = "mean",
#   test = c("rope", "p_direction"), 
#   rope_range = c(-0.1, 0.1)
# )
#   
#  model_performance(fit0)
# 
# tfit %>% 
#   select(Parameter, Mean, CI_low, CI_high, pd, ROPE_Percentage) %>%
#   mutate_at(2:6, round, 2) %>%  
#   mutate(Parameter = str_remove(Parameter,  "stadio" ),
#          Mean = round(exp(Mean),2), 
#          CI_low = round(exp(CI_low), 2), 
#          CI_high = round(exp(CI_high),2)) %>%  as.data.frame() %>% 
#   write.xlsx(file = "model.xlsx")
# 
# 
# 
# 
# plot_model(fit0, type = "est", 
#            show.values = TRUE,  
#            value.offset = .3, 
#            show.intercept = T, 
#            transform = "exp" ) +
#   theme_ipsum_rc()+labs(title = "")
# 
# 
# mod <- tab_model(fit0, transform = "exp")
# 
# 
# p <- plot_model(fit0)
# 
# p <- p[["data"]]
# 
# p %>% 
#   mutate(Parameter = str_remove(Parameter,  "stadio" )) %>% 
#          #Parameter = str_remove(Parameter, "stagione")) %>% 
#   ggplot(aes(y=exp(value), x = Parameter))+
#   stat_halfeye() +
#   coord_flip()+
#   geom_hline(yintercept = 1 )
# 
# plot(p_direction(fit0))+scale_fill_brewer(palette="Blues")+
#   theme_ipsum_rc()
# 
# plot(rope(fit0))+scale_fill_brewer(palette="Blues")+
#   theme_ipsum_rc()
# 
# 
# rope(fit0, ci = 1)
# 
# rope_range(fit0)
# 
#                  
# pplot<-plot(fit0, "areas", prob = 0.95, prob_outer = 1)
# pplot+ geom_vline(xintercept = 0)
# 
# 
# #    
# 
# 
# 
# 
# # x <- dati %>% 
# #   mutate(new_esito = str_remove(`Esito precodificato`,  '.*(?=con)'), 
# #          new_esito = str_remove(new_esito,'con ' ))
# # 
# 
#  