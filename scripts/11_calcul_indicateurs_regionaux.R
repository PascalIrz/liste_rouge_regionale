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
  scale_color_manual(values= pal) +
  labs(title = "Indicateurs de tendances pour 5 espèces de poissons d'eau douce", 
       x = "Années", 
       y = "Valeurs")

print(graphique)


#_______________________________________________________________________________
##################### CALCUL DU TAUX D'OCCURRENCE ##############################
#_______________________________________________________________________________

ope_id <- unique(ope_indicateur$ope_id)
esp_code_alternatif <- unique(ope_indicateur$esp_code_alternatif)
statut <- unique(ope_indicateur$statut)

annees <- ope_indicateur %>% 
  ungroup() %>% 
  select(ope_id,
         annee) %>% 
  distinct()

tab_oc <- crossing(ope_id, esp_code_alternatif, statut) 

densites <- ope_indicateur %>% 
  filter(indicateur == "densite_surface") 





taux_occurrence <- tab_oc %>% 
  left_join(densites) %>% 
  mutate(valeur = ifelse(is.na(valeur), 0, valeur)) %>% 
  select(-indicateur,
         -annee,
         -pop_id) %>% 
  left_join(annees) %>% 
  group_by(esp_code_alternatif,
           statut,
           annee) %>% 
  summarise(n_ope = n_distinct(ope_id),
            n_oc = n_distinct(ope_id[valeur > 0]),
            taux_oc = n_oc / n_ope) %>% 
  mutate(indicateur = "taux_occurrence",
         valeur = taux_oc) %>% 
  select(-n_ope,
         -n_oc,
         -taux_oc)



# J'empile mon indicateur de taux d'occurence dans le tab de données

pop_indicateur <-

################################################################################

# On souhaite observer les différentes tendances présentes sur les différents
# indicateurs calculés : 


# Indicateurs démographiques : Les tendance populationnelles seront d’abord 
# évaluées par site. Les méthodes restent à préciser mais sont envisagés des 
# tests de Mann-Kendall : 

# On réduit le jeu de données par site ( pop_id = 41683)

source(file="R/mann_kendall_sen.R")
library(trend)

test_41683_longueur <- ope_indicateur %>% 
  filter (pop_id == "41683") %>% 
  filter (statut == "toutes") %>% 
  filter (indicateur== "longueur_medianne") %>% 
  select(-ope_id,
         -pop_id,
         -indicateur,
         -statut)

tendance_indicateur <- mk_st_by_group(ope_indicateur,
               var_x = annee,
               var_y = valeur,
               esp_code_alternatif,
               indicateur,
               statut, 
               pop_id)



test_41683_longueur <- taux_occurrence %>% 
  select(-indicateur,
         -statut)


tendance_indicateur %>% 
  filter(statut == "toutes",
         esp_code_alternatif == "ANG") %>% 
  group_by(indicateur,
           trend) %>% 
  summarise(n = n_distinct(pop_id))  %>% 
  ggplot(aes(x = trend,
             y = n, 
             fill= trend,
             color = trend)) +
  geom_bar(stat = "identity") +
  facet_wrap(~indicateur)  + 
  scale_color_manual(values= pal)




########################################################################

tendance_indicateur <- mk_st_by_group(taux_occurrence,
                                      var_x = annee,
                                      var_y = valeur,
                                      esp_code_alternatif,
                                      indicateur,
                                      statut)


