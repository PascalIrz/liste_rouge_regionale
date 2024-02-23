#_______________________________________________________________________________
##################         INDICATEURS REGIONAUX        #######################
#_______________________________________________________________________________


# Pour les indicateurs calculés au point, on agrège chaque année leur valeur à 
# l'échelle régionale. 

# Le taux d'occurence de chaque espèce est directement calculée annuellement, à 
# l'échelle régionale, comme le pourcentage de sites prospectés où l'espèce a 
# été trouvée. 



## Chargement des packages ----

library(ggthemes)
library(tidyverse)
library(aspe)
library(ggplot2)
library (khroma)
library(dplyr)
library(wesanderson)

## Chargement des données ----

load(file = "processed_data/selection_pop_ope.rda")
load(file = "processed_data/pre_traitements_donnees_env.rda")
load(file = "processed_data/assemblage_tab_par_ope.rda")

rdata_tables <- misc_nom_dernier_fichier(
  repertoire = "../../../projets/ASPE/raw_data/rdata",
  pattern = "^tables")
load(rdata_tables)

mei_table <- misc_nom_dernier_fichier(
  repertoire = "../../../projets/ASPE/raw_data/rdata",
  pattern = "^mei")

load(mei_table)

source(file = "R/mk_st_by_group.R")

# Chargement de la palette de couleur utilisée : 
pal <- wes_palette("AsteroidCity1")




#_______________________________________________________________________________
##################### CONSTITUTION JEU DE DONNEES ##############################
#_______________________________________________________________________________

# Construction d'un df pour les indicateurs calculés au point de prélèvement (pop_id)  ----
# Calcul des données par année et pop_id en calculant la médiane de la valeur de l'indicateur ----
pop_indicateur <- ope_indicateur %>%
  group_by(esp_code_alternatif, annee, statut, indicateur) %>%
  summarize(valeur = median(valeur)) %>%
  ungroup() %>% 
  distinct()



# Représentation graphique de mes données : 

mes_do <- sample(unique(pop_indicateur$esp_code_alternatif), 5)

graphique <- pop_indicateur %>% 
  filter(esp_code_alternatif%in%mes_do) %>% 
  ggplot(aes(x= annee, y =valeur, group = statut, color = statut)) + 
  geom_line() +
  facet_grid(indicateur ~ esp_code_alternatif , scales = "free") + 
  scale_color_manual(values= pal)


print(graphique)

################################################################################

# On souhaite observer les différentes tendances présentes sur les différents
# indicateurs calculés : 

per_25 <- pop_indicateur %>% 
  filter (indicateur== "25_percentiles") %>% 
  select(-indicateur)


mk_st_by_group(per_25, valeur, annee)







