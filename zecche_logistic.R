library(dplyr)
library(ggplot2)
library(gmodels)
library(rlang)
library(openxlsx)
library(ggplot2)
library(gridExtra)
library(flextable)
library(crosstable)
library(stringr)
library(frequency)
library(tidyverse)
library(lubridate)




#rename the name of the columns
dati1 <-dati %>% 
  transmute(
    year = `Anno di registrazione`, conf = `Numero di conferimento`,sample=`Numero Campione`,
    subsamples = `Numero conferimento e numero campione`,aim= `Prova - Singola`, test=Tecnica,
    result=`Esito precodificato`,stage=Stadio,species=Specie, munic=`Comune del prelievo`,
    prov=Provincia, month= mese, alt=altitudine, altim=`Altimetria ISTAT`, lat=Latitudine,long=Longitudine)



#rename the values of aim, which diagnosis has been performed
#this is one method, brutal

dati1$aim[dati1$aim=="Borrelia burgdorferi sensu lato complex: agente eziologico"] <- "borr_complex"
dati1$aim[dati1$aim =="Febbre Q da Coxiella burnetii: agente eziologico"] <- "cox"
dati1$aim[dati1$aim =="Francisella spp.: agente eziologico"] <- "franc"
dati1$aim[dati1$aim =="Identificazione zecche dell'Ordine Ixodida"] <- "id_species"
dati1$aim[dati1$aim=="Rickettsia spp.: agente eziologico"] <- "rick"
dati1$aim[dati1$aim=="Tick Borne Encephalitis virus (TBEV): agente eziologico"] <- "tbev"
dati1$aim[dati1$aim=="Sequenziamento acidi nucleici"] <- "seq"
dati1$aim[dati1$aim=="Sequenziamento acidi nucleici"] <- "seq"
table(dati1$aim)



#aske massimo second method more elegant, but there must be something quickier, how do I recall the value of a column variable 



dati2 = dati1%>%
   mutate(result = recode(result, "Sequenza negativa" = "neg_seq", "Sequenza mista" = "mix_seq"), 
               stage = recode(stage, "ninfa" = "nymph", "Femmina"="female","Maschio"="male","maschio"="male"),
          month=recode(month,"gennaio"="Jan","febbraio"="Feb","marzo"="Mar","aprile"="Apr","maggio"="May",
                       "giugno"="Jun","luglio"="Jul","agosto"="Aug","settembre"="Sep", "ottobre"="Oct","novembre"="Nov","dicembre"="Dec"))


#creo un'altra colonna
dati2 <- cbind(dati2,seq_result=NA)
#sposo la colonna seq_result
dati2=dati2[,c(1:7, 17, 8:17)]
dati2=dati2 %>% relocate(seq_result, .after = result)
dati2$seq_result.1 <- NULL





dati2$result[dati2$result=="Confermata presenza di Borrelia spp"] <- "Borrelia spp"
dati2$result[dati2$result=="Confermata presenza di Rickettsia spp"] <- "Rickettsia spp"
dati2$result[dati2$result=="Confermata presenza di Borrelia afzelii"] <- "Borrelia afzelii"
dati2$result[dati2$result=="Dimostrata presenza"] <- "pos"
dati2$result[dati2$result=="Non dimostrata presenza"] <- "neg"
dati2$result[dati2$result=="Confermata presenza di Borrelia spp."]<-"Borrelia spp"
dati2$result[dati2$result=="Confermata presenza di Borrelia spp."]<-"Borrelia spp"
dati2$result[dati2$result=="Rilevata presenza di Rickettsia monacensis con identità nucleotidica del 100% in Blast."]<-"Rickettsia monacensis"
dati2$result[dati2$result=="Rilevata presenza di Rickettsia monacensis con identità nucleotidica del 100% in Blast"]<-"Rickettsia monacensis"
dati2$result[dati2$result=="Rilevata presenza di Rickettsia helvetica"]<-"Rickettsia helvetica"
dati2$result[dati2$result=="Rilevata presenza di Ricketsia helvetica"]<-"Rickettsia helvetica"
dati2$result[dati2$result=="Rilevata presenza di Borrelia afzelii con identità nucleotidica del 100%"]<-"Borrelia afzelii"
dati2$result[dati2$result=="Rilevata identità nucleotidica del 100% con Rickettsia helvetica"]<-"Rickettsia helvetica"
dati2$result[dati2$result=="Rilevata identità nucleotidica del 99.72% con Borrelia sp."]<-"Borrelia spp"
dati2$result[dati2$result=="Rilevata identità nucleotidica del 100% con Rickettsia monacensis"]<-"Rickettsia monacensis"
dati2$result[dati2$result=="Rilevata identità nucleotidica del 100% con Rickettsia helvetica."]<-"Rickettsia helvetica"
dati2$result[dati2$result=="Rilevata identità nucleotidica del 100% con Borrelia spp."]<-"Borrelia spp"
dati2$result[dati2$result=="Rilevata identità nucleotidica del 100% con Borrelia spielmanii"]<-"Borrelia spielmanii"
dati2$result[dati2$result=="Rilevata identità nucleotidica del 100% con Borrelia sp"]<-"Borrelia spp"
dati2$result[dati2$result=="Rilevata identità nucleotidica del 100% con Borrelia afzelii"]<-"Borrelia afzelii"
dati2$result[dati2$result=="Rilevata identità nucleotidica del 99.43% con Borrelia sp."]<-"Borrelia spp"
dati2$result[dati2$result=="Rilevata identità nucleotidica del 99,74% con Rickettsia helvetica"]<-"Rickettsia helvetica"
dati2$result[dati2$result=="Rilevata identità nucleotidica del 99,72% con Francisella tularensis"]<-"Francisella tularensis"
dati2$result[dati2$result=="Rilevata identità nucleotidica del 99,72% con Borrelia sp."]<-"Borrelia spp"
dati2$result[dati2$result=="Rilevata identità nucleotidica del 99,72% con Borrelia garinii/bavariensis/burgdorferi/afzelii"]<-"Borrelia garinii/bavariensis/burgdorferi/afzelii"
dati2$result[dati2$result=="Rilevata identità nucleotidica del 99,64% con Borrelia maritima/garinii/bavariensis/burgdorferi/bissettii"]<-"Borrelia maritima/garinii/bavariensis/burgdorferi/bissettii"
dati2$result[dati2$result=="Rilevata identità nucleotidica del 99,43% con Borrelia garinii"]<-"Borrelia garinii"
dati2$result[dati2$result=="Rilevata identità nucleotidica del 95.81% con Borrelia sp."]<-"Borrelia spp"
dati2$result[dati2$result=="Rilevata identità nucleotidica del 100% con Francisella cf. novicida"]<-"Francisella cf. novicida"
dati2$result[dati2$result=="Rilevata identità nucleotidica del 100% con Coxiella burnetii"]<-"Coxiella burnetii"
dati2$result[dati2$result=="Rilevata identità nucleotidica del 100% con Borreliella garinii"]<-"Borreliella garinii"
dati2$result[dati2$result=="Rilevata identità nucleotidica del 100% con Borreliella afzelii"]<-"Borreliella afzelii"
dati2$result[dati2$result=="Rilevata identità nucleotidica del 100% con Borreliella afzelii"]<-"Borreliella afzelii"
dati2$result[dati2$result=="Rilevata identità nucleotidica del 100% con Borrelia lusitaniae/burgdorferi"]<-"Borrelia lusitaniae/burgdorferi"
dati2$result[dati2$result=="Rilevata identità nucleotidica del 100% con Borrelia lusitaniae e Borrelia burgdorferi"]<-"Borrelia lusitaniae/burgdorferi"
dati2$result[dati2$result=="Rilevata identità nucleotidica del 100% con Borrelia garinii/bavariensis/burgdorferi"]<-"Borrelia garinii/bavariensis/burgdorferi"
dati2$result[dati2$result=="Rilevata identità nucleotidica del 100% con Borrelia garinii"]<-"Borrelia garinii"
dati2$result[dati2$result=="Rilevata identità nucleotidica del 100% con Borrelia burgdorferi sensu lato complex"]<-"Borrelia burgdorferi sensu lato complex"
dati2$result[dati2$result=="Rilevata identità nucleotidica del 100% con Borrelia afzelii/Borrelia burgdorferi/SCW-31"] <- "Borrelia burgdorferi/SCW-31"
dati2$result[dati2$result=="Rilevata identità nucleotidica del 100% con Borrelia afzelii."] <- "Borrelia afzelii"
dati2$result[dati2$result=="Rilevata identità nucleotidica del 100% con B. burgdorferi, B. garinii, B. californiensis e B. bissettii."] <- "B. burgdorferi/garinii/californiensis/bissettii"
dati2$result[dati2$result=="Rilevata identità nucleotidica del 100 % con Rickettsia helvetica."] <- "Rickettsia helvetica"
dati2$species[dati2$species=="ixodes hexagonus"] <- "Ixodes hexagonus"


#to look how many identification of species have been done every year

id_species<- dati2%>%filter(aim=="id_species") %>% group_by(year) %>% summarise(count=n())

#take a look at the variable I created
id_species

#create a barchart with the count of id_species (identification) have been done across the the year of the study
ggplot(id_species) +
  geom_bar(aes(x=year,y=count),stat='identity')



# create a variable for the species identified through the study 
species_ident<- dati2%>%filter(aim=="id_species") %>% group_by(species) %>% summarise(count=n())
species_ident

#create a barchart with the count of the species identified through the study
ggplot(species_ident)+
  geom_bar(aes(x=species,y=count),stat="identity")+
  coord_flip()
#create the same barchart with stacked species
ggplot(species_ident, aes(x="", y=count, fill=species))+
  geom_bar(width = 1, stat = "identity")


#create a table withe species identified each year of the study
species_ident_year <- dati2 %>% filter(aim=="id_species") %>% group_by(year,species) %>% summarise(count=n())

#See what I have created
species_ident_year


# create a bar chart position dodge with the species identified very year
ggplot(data=species_ident_year, aes(x=year, y=count, fill=species)) +
  geom_bar(stat="identity",position = position_dodge())

#we are going to explore the number of species identification performed in each province
table(dati2$prov[dati2$aim=="id_species"], useNA ="always")

#second method to see how many identification have been made in each province
ident_prov <- filter(dati2,aim=="id_species") %>% group_by(prov) %>% summarise(count=n())
ident_prov
#create a bar chart with the identification made in each province through the study
ggplot(ident_prov)+
  geom_bar(aes(x=prov,y=count),stat = "identity")

#we explore how many identification have been made by month

dati2$month <- factor(dati2$month, levels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul","Aug","Sep","Oct","Nov","Dec"))
ident_month = dati2 %>% select(aim,month,stage) %>% 
  mutate(stage=recode(stage,"female"="adult","male"="adult")) %>% 
  mutate(stage = na_if(stage, "N.D.")) %>% 
  drop_na(stage) %>% 
  filter(aim=="id_species") %>% group_by(month,stage) %>% summarise(count=n())

#check to see what i have created
ident_month

#we explore the distribution of the identification by month
ggplot(ident_month)+
  geom_bar(aes(x=month,y=count),stat = "identity")

#we explore the distribution of the stages by month
ggplot(data=ident_month, aes(x=stage, y=count, fill=month)) +
  geom_bar(stat="identity",position = position_stack())

ggplot(data=ident_month, aes(x=stage, y=count, fill=month)) +
  geom_bar(stat="identity",position = position_dodge())


#start exploring the distribution of the stadia of ticks
stadia=dati2 %>% filter(aim=="id_species") %>% group_by(stage) %>% summarise(count=n())
stadia

#create a barchart with stadia identified through the study
ggplot(stadia)+
  geom_bar(aes(x=stage,y=count),stat = "identity")

# iwant to see how the stadia are distributed according to the alt
stadia_month=dati2 %>% select(aim,stage,alt,month,prov) %>% filter(aim=="id_species") %>% 
  mutate(stage=recode(stage,"female"="adult","male"="adult"))%>% 
  mutate(stage = na_if(stage, "N.D.")) %>% 
  drop_na(stage)

# create box_plot with the distribution of altitude according to the stadia
ggplot(stadia_month, aes(x=stage, y= alt)) + 
  geom_boxplot()
#create a geom point
ggplot(stadia_month, aes(x=stage, y= alt,color=stage))+
  geom_jitter()
#explore month, altidtude and stadia 
ggplot(data = stadia_month) + 
  geom_point(mapping = aes(x =stage , y = alt)) + 
  facet_wrap(~ month, nrow = 3)

#check for normality of the data regarding altitude by stage
#norm_nymph
ggplot(stadia_month)+
  geom_qq(aes(sample=alt,color=stage))

#let's check if there is difference between the stadia in alt mean
 stadia_month %>%
  group_by(stage) %>%
  summarise_at(vars(alt), list(stage_alt = mean))
stadia_month %>%
   group_by(stage) %>%
   summarise_at(vars(alt), list(stage_alt_sd=sd))



# iwant to see how the stadia are distributed according to the  month
stadia_month=dati2 %>% select(aim,stage,alt,month) %>% filter(aim=="id_species") %>% 
  mutate(stage=dplyr:::recode(stage,"female"="adult","male"="adult"))%>% 
  mutate(stage = na_if(stage, "N.D.")) %>% 
  drop_na(stage)

#let's have a look at what i have
stadia_month

table(stadia_month$month,stadia_month$stage)

table(stadia_month$stage)

#here the code split

#lets create some variable in order to test if the stage nymph. no nymph can be modeled as function
# of the alt


#lets'start doing it as logistic regression analisis 



stadia_month =stadia_month%>% mutate(alt=cut(alt,breaks=c(0,1000, 2000, 3000),labels=c('alt0', 'alt1', 'alt3'))) %>% mutate(alt=recode(alt, "alt0"="alt0","alt1"="alt1","alt3"="alt3"))

table(stadia_month$alt)

library(epiDisplay)


stadia_month=stadia_month %>% mutate(month=recode(month, "Jan"="Spring","Feb"="Spring", "Mar"="Spring","Apr" ="Spring","May" ="Spring","Jun"= "Summer","Jul"="Summer","Aug"="Summer", "Sep" = "Autumn","Oct" ="Autumn","Nov" ="Autumn","Dec"="Autumn"))

stadia_month=stadia_month %>% mutate(stage=recode(stage,"larva"="no_nymph","adult"="no_nymph"))
table(stadia_month$stage,stadia_month$alt)

stadia_month=stadia_month %>% mutate(stage=recode(stage,"nymph"=1,"no_nymph"=0))

stadia_month$stage

stage <- stadia_month$stage
alt <- stadia_month$alt
season <- stadia_month$month

table(stage,season)

ggplot(stadia_month, aes(x=season, y= stage))+ 
  geom_boxplot()

#i test if the association between nymph or n on nymph stadium differs acrosa the altitude


CrossTable(stage,alt,chisq = TRUE)
# the probability of finding a nymph or another stage does not differ across the altitude

#test wheather there is a difference a difference between being bitten by nymph or non nymph across the season

CrossTable(stage,season,chisq = TRUE)

#differs acrosss the seasons



library(epiR)


#testo se la morsicatura da ninfa ha più probabilitá di essere associato all'estate rispetto alla primavera
epi.2by2(matrix(c(444,132,159,36),2),method="cross.sectional",conf.level = 0.95)
#non é associato all'estate più della primavera

#testo se la morsicatura da ninfa ha più probabilitá di essere associato all'autunno rispetto alla primavera

epi.2by2(matrix(c(48,132,41,36),2),method="cross.sectional",conf.level = 0.95)
#l'autunno sembra essere asspciato piu alla puntura da non ninfa rispetto alla primavera

#testo se la morsicatura da ninfa ha più probabilitá di essere associato all'autunno rispetto all'estate
epi.2by2(matrix(c(48,444,41,159),2),method="cross.sectional",conf.level = 0.95)

# se la variabile ha significato in un. modello di regressone logistica

model <- glm(stage~season,family = "binomial",data=stadia_month)
summary(model)
logistic.display(model)


#I test the variabkle categorized alt withe probability of being bitten by a nymmph and no nymph
#across the seasons


#In autunno si hanno meno probabilità di essere morsii da un altro stadio sip alla ninfa 





#let's analyse the results for the each disease
borrelia = dati2 %>% filter(aim=="borr_complex") %>% group_by(result) %>% summarise(count=n())


#i have 900 borrelia test

#count how many subsamples i have
n_distinct(dati2$subsamples)
#there are 898

#i have more borrelia test than subsamples
# there must be a something wrong ask massimo

#calculate proportion of success an confidence interval 0.95 for borrelia
n_borrelia<- 900
prop_borrelia <-round(91/900,digits = 3)
prop_borrelia
borrelia_se <- round(sqrt(prop_borrelia*(1-prop_borrelia)/n_borrelia),digits = 3)
borrelia_lowinterval <- round(prop_borrelia-1.96*borrelia_se,digits = 3)

borrelia_upinterval <-round(prop_borrelia+1.96*borrelia_se,digits = 3)
#in a much easier way use the proptest from gmodels package
prop.test(91,900)




#start exploring the proportion for coxiella
coxiella = dati2 %>% filter(aim=="cox") %>% group_by(result) %>% summarise(count=n())
coxiella
prop.test(1,900)

#start exploring for proportio and confidence interval for francisella
francisella = dati2 %>% filter(aim=="franc") %>% group_by(result) %>% summarise(count=n())
francisella
prop.test(2,900)

#start exploring for proportion and confidence interval for rickettsia
rickettsia= dati2 %>% filter(aim=="rick") %>% group_by(result) %>% summarise(n())
rickettsia
prop.test(68,900)

#start exploring for proportion and confidence interval for tbev
tbev = dati2 %>% filter(aim=="tbev") %>% group_by(result) %>% summarise(n())
tbev
prop.test(1,899)


disease <- c("borrelia","rickettsia")
prop_disease <- c(0,101,0.076)






