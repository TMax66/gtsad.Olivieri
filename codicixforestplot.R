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
 









prevS <- dtsalm %>% 
  group_by(specie, sex, age, salmonella) %>% 
  count() %>% 
  pivot_wider(names_from = "salmonella", values_from = "n", values_fill = 0) %>%  
  mutate(`tested` = sum(Neg, Pos)) %>%  
  #mutate(`N° animali analizzati` = sum(Neg, Pos)) %>%  
  #select(`Specie animale` = specie, `N° Positivi` = Pos, `N° animali analizzati`) 
  select(-Neg)  


prevS 

options(digits=2)

resbinomS <- binom.bayes(
  x = prevS$Pos, n = prevS$tested,
  type = "highest", conf.level = 0.95, tol = 1e-9)

# resbinomS <- binom.bayes(
#   x = prevS$`N° Positivi`, n = prevS$`N° animali analizzati`,
#   type = "highest", conf.level = 0.95, tol = 1e-9)


dtS <- cbind(prevS, resbinomS[,6:8])

dtS$age[is.na(dtS$age)] = "Not recorded" 
dtS$sex[is.na(dtS$sex)] = "Not recorded" 


#FORESTPLOT----
##OVERALL---------------------------------

x <-  dtsalm %>% 
  group_by(specie, salmonella) %>% 
  count() %>% 
  pivot_wider(names_from = "salmonella", values_from = "n", values_fill = 0) %>%  
  mutate(`tested` = sum(Neg, Pos)) %>%  
  #mutate(`N° animali analizzati` = sum(Neg, Pos)) %>%  
  #select(`Specie animale` = specie, `N° Positivi` = Pos, `N° animali analizzati`) 
  select(-Neg)

xx <- binom.bayes(
  x = 3, n = 27,
  type = "highest", conf.level = 0.95, tol = 1e-9)



##fplot tasso----
Tasso <- dtS %>% filter(specie == "Badgers")

Tasso <- Tasso %>% 
  mutate(Prevalence = mean,
         liminf = lower,
         limsup = upper,
         across(where(is.double), round,2)) %>% 
  select(-specie)

#xticks <- seq(from = 0, to = .6, by = 0.05)
xticks <- seq(from = 0, to = 1, by = 0.05)
xtlab <- rep(c(TRUE, FALSE), length.out = length(xticks))
attr(xticks, "labels") <- xtlab

fp_tasso <- Tasso %>% ungroup() %>% 
    mutate(Prevalence = sprintf("%.2f", mean), .after = tested) %>% 
    mutate(CI = paste0("[",sprintf("%.2f", lower),", ", sprintf("%.2f", upper), "]")) %>% 
    forestplot(labeltext = c( sex,age, Pos,
                            tested,  Prevalence, CI),
             clip = c(0,1),
             xlog = FALSE, 
             ci.vertices = TRUE,
             ci.vertices.height = 0.05,
             xlab= expression(bold("Estimated prevalence with 95% CI")),
             title = "Badgers",
             align = "llrrrc",
             colgap = unit(4, "mm"),
             xticks = xticks,
             # txt_gp = fpTxtGp(cex=0.90),
             # axes = gpar(cex = 0.9),
             txt_gp = fpTxtGp(ticks=gpar(cex=1), 
                              xlab = gpar(cex=1))) %>% 
    fp_add_lines(h_1 = gpar(lty = 1),
               h_2 = gpar(lty = 1),
               h_11 = gpar(lty = 1),
               h_12 = gpar(lty = 1)) %>% 
  fp_add_header(sex = "Sex", 
                age =  "Age", 
                Pos = "Positive", 
                tested = "Tested", 
                Prevalence = "Prevalence",
                CI = "95% CI") %>% 
  fp_append_row(mean  = 0.12,
                lower = 0.07,
                upper = 0.16,
                sex = expression(bold("Overall")),
                Pos = expression(bold("21")),
                tested = expression(bold("182")),
                Prevalence = expression(bold("0.12")),
                CI = expression(bold("[0.07, 0.16]")),
                position = "last",
                is.summary = F) %>% 
  fp_set_style(box = c(rep("black", 10), "royalblue"))

png(file='fp_tasso.png', width = 800, height = 500) # Open PNG device with specific file name
fp_tasso
dev.off() 

##fplot volpe----
volpe <- dtS %>% filter(specie == "Foxes")

volpe <- volpe %>% 
  mutate(Prevalence = mean,
         liminf = lower,
         limsup = upper,
         across(where(is.double), round,2)) %>% 
  select(-specie)

#xticks <- seq(from = 0, to = .3, by = 0.025)
xticks <- seq(from = 0, to = 1, by = 0.05)
xtlab <- rep(c(TRUE, FALSE), length.out = length(xticks))
attr(xticks, "labels") <- xtlab

fp_volpe <- volpe %>% ungroup() %>% 
      mutate(Prevalence = sprintf("%.2f", mean), .after = tested) %>% 
    mutate(CI = paste0("[",sprintf("%.2f", lower),", ", sprintf("%.2f", upper), "]")) %>% 
  forestplot(labeltext = c( sex,age, Pos,
                            tested,  Prevalence, CI),
             clip = c(0,1),
             xlog = FALSE, 
             ci.vertices = TRUE,
             ci.vertices.height = 0.05,
             align = "llrrrc",
             colgap = unit(4, "mm"),
             xticks = xticks,
             xlab= expression(bold("Estimated prevalence with 95% CI")),
             title = "Foxes",
             # txt_gp = fpTxtGp(cex=0.90),
             # axes = gpar(cex = 0.9),
             txt_gp = fpTxtGp(ticks=gpar(cex=1), 
                              xlab = gpar(cex=1))) %>%  
      fp_add_lines(h_1 = gpar(lty = 1),
               h_2 = gpar(lty = 1),
               h_11 = gpar(lty = 1),
               h_12 = gpar(lty = 1)) %>% 
  fp_add_header(sex = "Sex", 
                age =  "Age", 
                Pos = "Positive", 
                tested = "Tested", 
                Prevalence = "Prevalence",
                CI = "95% CI") %>% 
  fp_append_row(mean  = 0.06,
                lower = 0.04,
                upper = 0.08,
                sex = expression(bold("Overall")),
                Pos = expression(bold("42")),
                tested = expression(bold("718")),
                Prevalence = expression(bold("0.06")),
                CI = expression(bold("[0.04, 0.08]")),
                position = "last",
                is.summary = F) %>% 
  fp_set_style(box = c(rep("black", 10), "royalblue"))


png(file='fp_volpe.png', width = 800, height = 500) # Open PNG device with specific file name
fp_volpe
dev.off() 



##fplot lupo----
wolves <- dtS %>% filter(specie == "Wolves")

wolves <- wolves %>% 
  mutate(Prevalence = mean,
         liminf = lower,
         limsup = upper,
         across(where(is.double), round,2)) %>% 
  select(-specie)

xticks <- seq(from = 0, to = 1, by = 0.05)
xtlab <- rep(c(TRUE, FALSE), length.out = length(xticks))
attr(xticks, "labels") <- xtlab


fp_lupo <- wolves %>% ungroup() %>% 
        mutate(Prevalence = sprintf("%.2f", mean), .after = tested) %>% 
    mutate(CI = paste0("[",sprintf("%.2f", lower),", ", sprintf("%.2f", upper), "]")) %>% 
  forestplot(labeltext = c( sex,age, Pos,
                            tested,  Prevalence, CI),
             clip = c(0,1),
             xlog = FALSE, 
             ci.vertices = TRUE,
             ci.vertices.height = 0.05,
             align = "llrrrc",
             #colgap = unit(4, "mm"),
             xticks = xticks,
             xlab= expression(bold("Estimated prevalence with 95% CI")),
             title = "Wolves",
             # txt_gp = fpTxtGp(cex=0.90),
             # axes = gpar(cex = 0.9),
             txt_gp = fpTxtGp(ticks=gpar(cex=1), 
                              xlab = gpar(cex=1))) %>%  
    fp_add_lines(h_1 = gpar(lty = 1),
               h_2 = gpar(lty = 1),
               h_6 = gpar(lty = 1),
               h_7 = gpar(lty = 1)) %>% 
    fp_add_header(sex = "Sex", 
                age =  "Age", 
                Pos = "Positive", 
                tested = "Tested", 
                Prevalence = "Prevalence",
                CI = "95% CI") %>% 
  
  fp_append_row(mean  = 0.12,
                lower = 0.02,
                upper = 0.25,
                sex = expression(bold("Overall")),
                Pos = expression(bold("3")),
                tested = expression(bold("27")),
                Prevalence = expression(bold("0.12")),
                CI = expression(bold("[0.02, 0.25]")),
                position = "last",
                is.summary = F) %>% 
  fp_set_style(box = c(rep("black", 5), "royalblue"))

png(file='fp_lupo.png', width = 800, height = 350) # Open PNG device with specific file name
fp_lupo
dev.off() 


#revisione----

dt <- read_excel("meta.xlsx")

dt <- dt[, c(1, 2,3, 4, 5, 6, 9)]

dt <- dt %>% 
  arrange(Anno) %>% 
  mutate(ID = paste0("Study-", seq(1:nrow(.))))

# p <- rbeta(1000000, shape1 = 3+1, shape2 = 188-3+1)
# 
# p %>% tibble() %>% View()
# ggplot()+
#   aes(p)+
#   geom_density()
# 
# 
#  mybeta <- function(n, a, b){
#    
#    rbeta(n, a+1, b-a+1)
#  }
# 
# 
# x <- mybeta(100, a = dt$`N° Positivi`, b = dt$`N° animali analizzati`)


#amrbib <- read_excel(here("ANALYSIS",  "data", "raw",  "meta.xlsx"))
#amrbib<-amrbib %>% 
 # filter(articolo!="8") %>% 
  #filter(articolo!="10")

options(digits=2)

resbinom <- binom.bayes(
  x = dt$`N° Positivi`, n = dt$`N° animali analizzati`,
  type = "highest", conf.level = 0.95, tol = 1e-9)


dt<-cbind(dt, resbinom[,6:8])


# dt %>% 
#   ggplot( aes(y=mean,ymin=lower, ymax=upper, x=ID))+
#   geom_point( size=2)+geom_linerange(color=grey, size=.6)+
#   coord_flip()+
#   # theme_ipsum_rc(axis_title_just = "mc")+
#   facet_wrap(~ `Specie animale`,  scales = "free")+
#   labs(x="", y="Posterior Bayesian Estimated Prevalence with 95% Compatibility Interval")

 
##tasso----
Tasso <- dt  %>% filter(`Specie animale` == "Tasso")  %>% 
select(Study = ID, Year = Anno, Country = Nazione, 
       pos = `N° Positivi`,
       tested = `N° animali analizzati`, 
        mean,   lower, 
          upper) %>% 
  mutate(Prevalence = mean,
         liminf = lower,
         limsup = upper,
    across(where(is.double), round,2))

xticks <- seq(from = 0, to = 1, by = 0.05)
xtlab <- rep(c(TRUE, FALSE), length.out = length(xticks))
attr(xticks, "labels") <- xtlab

fp_rev_tasso <- Tasso %>% 
      mutate(Prevalence = sprintf("%.2f", mean), .after = tested) %>% 
    mutate(CI = paste0("[",sprintf("%.2f", lower),", ", sprintf("%.2f", upper), "]")) %>% 
  forestplot(labeltext = c( Study, Year, Country, pos,
                            tested,  Prevalence, CI),
             clip = c(0,1),
             xlog = FALSE, 
             ci.vertices = TRUE,
             ci.vertices.height = 0.05,
             xlab= expression(bold("Estimated prevalence with 95% CI")),
             title = "Badgers",
             align = "lclrrrc",
             colgap = unit(5, "mm"),
             xticks = xticks,
             # txt_gp = fpTxtGp(cex=0.90),
             # axes = gpar(cex = 0.9),
             txt_gp = fpTxtGp(ticks=gpar(cex=1), 
                              xlab = gpar(cex=1))) %>% 
      fp_add_lines(h_1 = gpar(lty = 1),
               h_2 = gpar(lty = 1),
               h_11 = gpar(lty = 1),
               h_12 = gpar(lty = 1)) %>% 
  fp_add_header(Study = "Study", 
                Year =  "Year", 
                Country = "Country", 
                pos = "Positive", 
                tested = "Tested", 
                Prevalence = "Prevalence",
                CI = "95% CI") %>% 
  fp_append_row(mean  = 0.117,
                lower = 0.072,
                upper = 0.165,
                Study = expression(bold("Our Study")),
                Year = expression(bold("2022")),
                Country = expression(bold("Italy")),
                pos = expression(bold("21")),
                tested = expression(bold("182")),
                Prevalence = expression(bold("0.12")),
                CI = expression(bold("[0.07, 0.16]")),
                position = "last",
                is.summary = F) %>% 
  fp_set_style(box = c(rep("black", 10), "royalblue"))
    
  
png(file='fp__rev_tasso.png', width = 800, height = 500) # Open PNG device with specific file name
fp_rev_tasso
dev.off() 






##volpe----
Volpe <- dt %>% filter(`Specie animale` == "Volpe") 
  

Volpe <- dt  %>% filter(`Specie animale` == "Volpe")  %>% 
  select(Study = ID, Year = Anno, Country = Nazione, 
         pos = `N° Positivi`,
         tested = `N° animali analizzati`, 
         mean,   lower, 
         upper) %>% 
  mutate(Prevalence = mean,
         liminf = lower,
         limsup = upper,
         across(where(is.double), round,2)) %>% 
  filter(!Country == "Irland")



fp_rev_volpe <- Volpe %>% 
        mutate(Prevalence = sprintf("%.2f", mean), .after = tested) %>% 
    mutate(CI = paste0("[",sprintf("%.2f", lower),", ", sprintf("%.2f", upper), "]")) %>% 
  forestplot(labeltext = c( Study, Year, Country, pos,
                            tested,  Prevalence, CI),
             clip = c(0 , 1),
             xlog = FALSE,
             ci.vertices = TRUE,
             ci.vertices.height = 0.05,
             xlab= expression(bold("Estimated prevalence with 95% CI")),
             title = "Foxes",
             align = "lllrrrc",
             colgap = unit(5, "mm"),
             xticks = xticks,
             # txt_gp = fpTxtGp(cex=0.90),
             # axes = gpar(cex = 0.9),
             txt_gp = fpTxtGp(ticks=gpar(cex=1), 
                              xlab = gpar(cex=1))) %>% 
        fp_add_lines(h_1 = gpar(lty = 1),
               h_2 = gpar(lty = 1),
               h_12 = gpar(lty = 1),
               h_13 = gpar(lty = 1)) %>% 
  fp_add_header(Study = "Study", 
                Year =  "Year", 
                Country = "Country", 
                pos = "Positive", 
                tested = "Tested", 
                Prevalence = "Prevalence",
                CI = "95% CI") %>% 
  fp_append_row(mean  = 0.059,
                lower = 0.042,
                upper = 0.0765,
                Study = expression(bold("Our Study")),
                Year = expression(bold("2022")),
                Country = expression(bold("Italy")),
                pos = expression(bold("42")),
                tested = expression(bold("718")),
                Prevalence = expression(bold("0.06")),
                CI = expression(bold("[0.04, 0.08]")),
                position = "last",
                is.summary = F) %>% 
  fp_set_style(box = c(rep("black", 11), "royalblue"))



png(file='fp__rev_volpe.png', width = 800, height = 500) # Open PNG device with specific file name
fp_rev_volpe
dev.off() 













# tabletextV <- cbind( c("Year",Volpe$Anno),c("Study", Volpe$ID),c("Country", Volpe$Nazione),
#                      c("#positive", Volpe$`N° Positivi`),
#                      c("#tested", Volpe$`N° animali analizzati`),
#                      c("Prevalence", round(Volpe$mean,2)),
#                      c("lim inf", round(Volpe$lower,2)), 
#                      c("lim sup", round(Volpe$upper, 2)))
# 
# mv<- c(NA, Volpe$mean)
# lv <- c(NA,Volpe$lower)
# uv <- c(NA, Volpe$upper)
# 
# 
# 
# 
# forestplot(tabletextV, mv, lv, uv)


## provare anche https://cran.r-project.org/web/packages/forestploter/vignettes/forestploter-intro.html

#______________________________________________________________________________________________________________
