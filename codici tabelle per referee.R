pat %>% 
  mutate(Bb = recode(`Borrelia burgdorferi sensu lato complex: agente eziologico`, 
                     "0" = "Neg", 
                     "1" = "Pos"), 
         FQ = recode(`Febbre Q da Coxiella burnetii: agente eziologico`, 
                     "0" = "Neg", 
                     "1" = "Pos"), 
         Tul = recode(`Francisella spp.: agente eziologico`, 
                      "0" = "Neg", 
                      "1" = "Pos"), 
         Ricz = recode(`Rickettsia spp.: agente eziologico`, 
                       "0" = "Neg", 
                       "1" = "Pos"), 
         TBE = recode(`Tick Borne Encephalitis virus (TBEV): agente eziologico`,
                      "0" = "Neg", 
                      "1" = "Pos")
                      ) %>%  
  select(-c(10:18)) %>%
  pivot_longer(10:14, names_to = "patogeni") -> pat2
  
  
  
  
  pat2 %>% 
  filter(annoreg == "2023",
         patogeni == "Bb", 
         specie == "Ixodes ricinus"
         ) %>% 
  select(stadio, value, stagione) %>%  
  group_by(stagione,stadio, value) %>% 
  count() %>% 
  mutate(stadio = as.character(stadio), 
         stadio = replace_na(stadio, "nd")) %>% 
  pivot_wider(names_from = value,  values_from = n, values_fill = 0) %>%  
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
             title = "",
             # txt_gp = fpTxtGp(cex=0.90),
             # axes = gpar(cex = 0.9),
             txt_gp = fpTxtGp(ticks=gpar(cex=1), 
                              xlab = gpar(cex=1)))  %>% 
  fp_add_header(stagione = "Season",
                stadio = "Stage",
                Pos = "N.positive" ,
                n = "N.tested"  ,
                perc = "% Positive", 
                Prevalence = "Prevalence",
                liminf = "95% lower lim.", 
                limsup = "95% upper lim.") 

