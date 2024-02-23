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
source(file = "R/calcul_biomasse.R")




#_______________________________________________________________________________
##################### CONSTITUTION JEU DE DONNEES ##############################
#_______________________________________________________________________________

# Construction d'un df pour les indicateurs calculés au point ----
# Calcul des données par année et pop_id en calculant la médiane de la valeur de l'indicateur ----
pop_indicateur <- ope_indicateur %>%
  group_by(esp_code_alternatif, annee, statut, indicateur) %>%
  summarize(median_value = median(valeur)) %>%
  ungroup() %>% 
  distinct()


