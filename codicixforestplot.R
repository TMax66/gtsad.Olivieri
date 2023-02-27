library(tidyverse)
library(here)
library(readxl)
library(binom)
library(ggthemes)
library(rmeta)
library(forestplot)

#DATI----
#dtsalm <- read_excel(here("salmodt.xlsx"))

dt <- read_excel("data/Prevalenze_studi.xlsx")

dt$nrow <- seq(1:nrow(dt))
 

df <- dt %>% 
  mutate(Specie = str_replace(Specie, "I. ", "Ixodes "), 
         Specie = str_replace(Specie, "D. ", "Dermacentor "), 
         Specie = str_replace(Specie, "Ixodesricinus", "Ixodes ricinus"), 
         Specie = str_replace(Specie, "H. ", "Haemaphysalis "), 
         Specie = str_replace(Specie, "R. ", "Rhipicephalus "),
         Specie = str_replace(Specie, "Rh. ", "Rhipicephalus "),
         Specie = str_replace(Specie, "A. ", "Amblyomma ")) 

prevS <- df %>% filter(str_detect(Specie, "Ixodes")) %>% 
  na.omit() %>% 
  mutate(Stady = ifelse(Stady %in% c("femmine", "maschi", "adulti"), "Adults", 
                        ifelse(Stady == "larve", "Larvaes", "Nymphs"))) %>% 
  group_by(Study, Stady, Specie, Totale) %>% 
  summarise(N = sum(N, na.rm = TRUE)) 




prevS

options(digits=2)

resbinomS <- binom.bayes(
  x = prevS$N, n = prevS$Totale,
  type = "highest", conf.level = 0.95, tol = 1e-9)




dtS <- cbind(prevS, resbinomS[,6:8])




#FORESTPLOT----



dtS <- dtS %>% 
  mutate(Prevalence = mean,
         liminf = lower,
         limsup = upper,
         across(where(is.double), round,2)) 
 


xticks <- seq(from = 0, to = 1, by = 0.05)
xtlab <- rep(c(TRUE, FALSE), length.out = length(xticks))
attr(xticks, "labels") <- xtlab



             
             
  #NINFE ----            

  ninfe <- dtS %>% ungroup() %>% 
    mutate(Prevalence = sprintf("%.2f", mean), .after = Totale) %>% 
    mutate(CI = paste0("[",sprintf("%.2f", lower),", ", sprintf("%.2f", upper), "]")) %>%  
    filter(Stady == "Nymphs", Specie == "Ixodes ricinus") %>%  
    select(-Stady, -Specie) %>% 
    forestplot(labeltext = c( Study,#Stady, Specie,
                              N,  Totale, Prevalence, CI), 
               clip = c(0,1),
               boxsize = 0.2,
               xlog = FALSE, 
               ci.vertices = TRUE,
               ci.vertices.height = 0.05,
               xlab= expression(bold("Estimated prevalence with 95% CI")),
               title = "Bayesian posterior estimated prevalence of Nimphys stady of Ixodes ricinus detected in different studies",
               align = "llrrrc",
               colgap = unit(4, "mm"),
               xticks = xticks,
               txt_gp = fpTxtGp(ticks=gpar(cex=1), 
                                xlab = gpar(cex=1))) %>% 
    fp_add_lines(h_1 = gpar(lty = 1),
                 h_2 = gpar(lty = 1),
                 h_10 = gpar(lty = 1),
                 h_11 = gpar(lty = 1)) %>%
    fp_add_header(Study = "Study", 
                  Totale = "Tested", 
                  N = "N",
                  Prevalence = "Prevalence",
                  CI = "95% CI") %>% 
    fp_append_row(mean  = 0.69,
                  lower = 0.65,
                  upper = 0.71,
                  Study = expression(bold("Our Study")),
                  N = expression(bold("618")),
                  Totale = expression(bold("900")),
                  Prevalence = expression(bold("0.69")),
                  CI = expression(bold("[0.65, 0.71]")),
                  position = "last",
                  is.summary = F) %>% 
    fp_set_style(box = c(rep("black", 9), "royalblue"))
  
  
  # LARVE----
  
  larve <- dtS %>% ungroup() %>% 
    mutate(Prevalence = sprintf("%.2f", mean), .after = Totale) %>% 
    mutate(CI = paste0("[",sprintf("%.2f", lower),", ", sprintf("%.2f", upper), "]")) %>%  
    filter(Stady == "Larvaes", Specie == "Ixodes ricinus") %>%  
    select(-Stady, -Specie) %>% 
    forestplot(labeltext = c( Study, 
                              N,  Totale, Prevalence, CI), 
               clip = c(0,1),
               boxsize = 0.15,
               xlog = FALSE, 
               ci.vertices = TRUE,
               ci.vertices.height = 0.05,
               xlab= expression(bold("Estimated prevalence with 95% CI")),
               title = "Bayesian posterior estimated prevalence of Larvaes stadyof Ixodes ricinus detected in different studies",
               align = "llrrrc",
               colgap = unit(4, "mm"),
               xticks = xticks,
               txt_gp = fpTxtGp(ticks=gpar(cex=1), 
                                xlab = gpar(cex=1))) %>% 
    fp_add_lines(h_1 = gpar(lty = 1),
                 h_2 = gpar(lty = 1),
                 h_10 = gpar(lty = 1),
                 h_11 = gpar(lty = 1)) %>%
    fp_add_header(Study = "Study", 
                  Totale = "Tested", 
                  N = "N",
                  Prevalence = "Prevalence",
                  CI = "95% CI") %>% 
    fp_append_row(mean  = 0.06,
                  lower = 0.04,
                  upper = 0.08,
                  Study = expression(bold("Our Study")),
                  N = expression(bold("58")),
                  Totale = expression(bold("900")),
                  Prevalence = expression(bold("0.06")),
                  CI = expression(bold("[0.04, 0.08]")),
                  position = "last",
                  is.summary = F) %>%  
   fp_set_style(box = c(rep("black", 9), "royalblue"))
  



  #ADULTI----
  
adulti <- dtS %>% ungroup() %>% 
    mutate(Prevalence = sprintf("%.2f", mean), .after = Totale) %>% 
    mutate(CI = paste0("[",sprintf("%.2f", lower),", ", sprintf("%.2f", upper), "]")) %>%  
    filter(Stady == "Adults", Specie == "Ixodes ricinus") %>%  
    select(-Stady, -Specie) %>% 
    forestplot(labeltext = c( Study,
                              N,  Totale, Prevalence, CI), 
               clip = c(0,1),
               boxsize = 0.15,
               xlog = FALSE, 
               ci.vertices = TRUE,
               ci.vertices.height = 0.05,
               xlab= expression(bold("Estimated prevalence with 95% CI")),
               title = "Bayesian posterior estimated prevalence of Adults stady  of Ixodes ricinus detected in different studies",
               align = "llrrrc",
               colgap = unit(4, "mm"),
               xticks = xticks,
               txt_gp = fpTxtGp(ticks=gpar(cex=1), 
                                xlab = gpar(cex=1))) %>% 
    fp_add_lines(h_1 = gpar(lty = 1),
                 h_2 = gpar(lty = 1),
                 h_11 = gpar(lty = 1),
                 h_12 = gpar(lty = 1)) %>%
    fp_add_header(Study = "Study", 
                  Totale = "Tested", 
                  N = "N",
                  Prevalence = "Prevalence",
                  CI = "95% CI") %>% 
  fp_append_row(mean  = 0.18,
                lower = 0.16,
                upper = 0.21,
                Study = expression(bold("Our Study")),
                N = expression(bold("170")),
                Totale = expression(bold("900")),
                Prevalence = expression(bold("0.18")),
                CI = expression(bold("[0.16, 0.21]")),
                position = "last",
                is.summary = F)   %>% 
    fp_set_style(box = c(rep("black", 10), "royalblue"))
  
  #######OLD CODE-----

# dtS %>% ungroup() %>% 
#   mutate(Prevalence = sprintf("%.2f", mean), .after = Totale) %>% 
#   mutate(CI = paste0("[",sprintf("%.2f", lower),", ", sprintf("%.2f", upper), "]")) %>% 
#   forestplot(labeltext = c( Study,Stady, Specie,
#                             N,  Totale, Prevalence, CI), 
#              clip = c(0,1),
#              boxsize = 0.2,
#              xlog = FALSE, 
#              ci.vertices = TRUE,
#              ci.vertices.height = 0.05,
#              xlab= expression(bold("Estimated prevalence with 95% CI")),
#              title = "Ixodes spp",
#              align = "llrrrc",
#              colgap = unit(4, "mm"),
#              xticks = xticks,
#              # txt_gp = fpTxtGp(cex=0.90),
#              # axes = gpar(cex = 0.9),
#              txt_gp = fpTxtGp(ticks=gpar(cex=1), 
#                               xlab = gpar(cex=1))) %>% 
#   fp_add_lines(h_1 = gpar(lty = 1),
#                h_2 = gpar(lty = 1),
#                h_29 = gpar(lty = 1),
#                h_32 = gpar(lty = 1)) %>%
#   fp_add_header(Study = "Study", 
#                 Stady =  "Stady", 
#                 Specie = "Specie", 
#                 Totale = "Tested", 
#                 N = "N",
#                 Prevalence = "Prevalence",
#                 CI = "95% CI") %>% 
#   fp_append_row(mean  = 0.69,
#                 lower = 0.65,
#                 upper = 0.71,
#                 Study = expression(bold("Our Study")),
#                 Stady = expression("ninfa"),
#                 Specie = expression("Ixodes ricinus"),
#                 N = expression(bold("618")),
#                 Totale = expression(bold("900")),
#                 Prevalence = expression(bold("0.69")),
#                 CI = expression(bold("[0.65, 0.71]")),
#                 position = "last",
#                 is.summary = F) %>% 
#   fp_append_row(mean  = 0.06,
#                 lower = 0.04,
#                 upper = 0.08,
#                 Study = expression(bold("Our Study")),
#                 Stady = expression("larve"),
#                 Specie = expression("Ixodes ricinus"),
#                 N = expression(bold("58")),
#                 Totale = expression(bold("900")),
#                 Prevalence = expression(bold("0.06")),
#                 CI = expression(bold("[0.04, 0.08]")),
#                # position = "last",
#                 is.summary = F) %>% 
#   fp_append_row(mean  = 0.18,
#                 lower = 0.16,
#                 upper = 0.21,
#                 Study = expression(bold("Our Study")),
#                 Stady = expression("adult"),
#                 Specie = expression("Ixodes ricinus"),
#                 N = expression(bold("170")),
#                 Totale = expression(bold("900")),
#                 Prevalence = expression(bold("0.18")),
#                 CI = expression(bold("[0.16, 0.21]")),
#                # position = "last",
#                 is.summary = F)  
#   
#   fp_set_style(box = c(rep("black", 27), "royalblue"))
