library(tidyverse)
library(openxlsx)
library(readxl)
library(here)
library(rstanarm)
library(bayesplot)
library(bayestestR)
library(see)
library(sjPlot)
library(hrbrthemes)
library(ggdist)

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
  mutate(stadio = casefold(stadio),
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
         stagioneanno = paste0(annoreg,stagione)) %>%
  filter(stadio != "n.d.")


pat <- dtpat %>% 
  mutate(pat = ifelse(esito != "Non dimostrata presenza", "1", "0")) %>%  
  pivot_wider(names_from = "prova", values_from = "pat", values_fill = '0' ) %>% 
  select(annoreg,nconfcamp, stadio, specie, comune, provincia, stagione, annoreg, stagioneanno,  altitudine, 17:22 ) %>%   
  mutate(across(c(10:13), as.numeric)) %>% 
  group_by(annoreg,nconfcamp, stadio, specie, comune, provincia, stagione, stagioneanno,altitudine) %>% 
  summarise(across(where(is.numeric), ~ sum(.x, na.rm = TRUE))) %>% ungroup() %>% 
  rowwise() %>%
  mutate(Pat = sum(c_across(c(10:13)), na.rm = T),
         Pat2 = ifelse(Pat == 0, 0, 1), 
         PatCat = ifelse(Pat2 == 0 , "Neg", "Pos"),
         stadio = factor(stadio, levels =c("Larvae", "Nymphae",  "Adult"))
         )%>%
  filter(specie == "Ixodes ricinus")
 

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
  

 


t_prior <- student_t(df = 7, location = 0, scale = 2.5)

fit0 <- stan_glm(Pat2 ~  stadio,   data = pat,family = binomial(link = "logit"))
fit1 <- stan_glm(Pat2 ~   stadio+stagione,   data = pat,family = binomial(link = "logit"))
fit2<- stan_glm(Pat2 ~   stadio*stagione,   data = pat,family = binomial(link = "logit"))
fit3 <- stan_glm(Pat2 ~   stadio+stagioneanno,   data = pat,family = binomial(link = "logit"))
fit4 <- stan_glm(Pat2 ~   stadio+stagione+annoreg,   data = pat,family = binomial(link = "logit"))
fit5 <- stan_glm(Pat2 ~   stadio+altitudine,   data = pat,family = binomial(link = "logit"))


#We recommend calling 'loo' again with argument 'k_threshold = 0.7'
loo0 <- loo(fit0, k_threshold = 0.7)
loo1 <- loo(fit1, k_threshold = 0.7)
loo2 <- loo(fit2, k_threshold = 0.7)
loo3 <- loo(fit3, k_threshold = 0.7)
loo4 <- loo(fit4, k_threshold = 0.7)
loo5 <- loo(fit5, k_threshold = 0.7)

print(loo_compare( loo1, loo2, loo3, loo4, loo5,loo0), simplify = FALSE)




fit0 <- stan_glm(Pat2 ~  stadio,   data = pat,family = binomial(link = "logit"))



posterior <- as.matrix(fit0)
posterior %>% as_tibble() %>% 
  pivot_longer( cols = 1:3, names_to = "factors", values_to = "est") %>%  
  mutate(factors = str_remove(factors,  "stadio" )) %>% #, 
         # factors = factor(factors, levels = c("Adult", "Nymphae", "Intercept")),
         # est = invlogit(est)) %>% 
  ggplot()+
  aes(y = factors, x = est)+
  stat_halfeye() 

posterior %>% as_tibble() %>%  
  
  mutate(
         # stadioAdult = invlogit(stadioAdult), 
         # stadioNymphae = invlogit(stadioNymphae), 
         # stadioLarvae = invlogit(`(Intercept)`), 
    stadioLarvae = `(Intercept)`,
         "Nymphae vs Larvae" = stadioNymphae - stadioLarvae, 
         "Adult vs Larvae" = stadioAdult-stadioLarvae, 
         "Adult vs Nymphae" = stadioAdult-stadioNymphae) %>% 
  
  
  select(5:7) %>% 
  
  
  pivot_longer( cols = 1:3, names_to = "factors", values_to = "est") %>% 
  
  
  ggplot()+
  aes(y = factors, x = est)+
  stat_halfeye() 






#library(tidybayes)
library(performance)


tfit <- describe_posterior(
  fit0,
  centrality = "median",
  test = c("rope", "p_direction"), 
  rope_range = c(-0.1, 0.1)
)
  
 model_performance(fit0)

tfit %>% 
  select(Parameter, Median, CI_low, CI_high, pd) %>%
  mutate_at(2:5, round, 2) %>%  
  mutate(Parameter = str_remove(Parameter,  "stadio" ),
         Median = round(exp(Median),2), 
         CI_low = round(exp(CI_low), 2), 
         CI_high = round(exp(CI_high),2)) 


plot_model(fit0, type = "est", 
           show.values = TRUE,  
           value.offset = .3, 
           show.intercept = T, 
           transform = "exp" ) +
  theme_ipsum_rc()+labs(title = "")


mod <- tab_model(fit0, transform = "exp")


p <- plot_model(fit0)

p <- p[["data"]]

p %>% 
  mutate(Parameter = str_remove(Parameter,  "stadio" )) %>% 
         #Parameter = str_remove(Parameter, "stagione")) %>% 
  ggplot(aes(y=exp(value), x = Parameter))+
  stat_halfeye() +
  coord_flip()+
  geom_hline(yintercept = 1 )

plot(p_direction(fit0))+scale_fill_brewer(palette="Blues")+
  theme_ipsum_rc()

plot(rope(fit0))+scale_fill_brewer(palette="Blues")+
  theme_ipsum_rc()


rope(fit0, ci = 1)

rope_range(fit0)

                 
pplot<-plot(fit0, "areas", prob = 0.95, prob_outer = 1)
pplot+ geom_vline(xintercept = 0)


#    




# x <- dati %>% 
#   mutate(new_esito = str_remove(`Esito precodificato`,  '.*(?=con)'), 
#          new_esito = str_remove(new_esito,'con ' ))
# 

 