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
library(zoo)
library(lemon)
library(trend)
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
source(file="R/mann_kendall_sen.R")

# Définition des couleurs de l'OFB en format RGB
pal <- c("#007844", "#92D050", "#0087C1", "#FCEE21", "#00AEEF", "#1D1D1B", "#A97B30", "#B9D9EB")


#_______________________________________________________________________________
##################### CONSTITUTION JEU DE DONNEES ##############################
#_______________________________________________________________________________

# Calcul des données par année et pop_id en calculant la médiane de la valeur de l'indicateur ----
pop_indicateur <- ope_indicateur %>%
  group_by(esp_code_alternatif, annee, indicateur, statut) %>% # Est-ce que je conserve le statut ? 
  summarize(valeur = median(valeur)) %>%
  ungroup() %>% 
  distinct()



# Représentation graphique de mes données : 
mes_do <- sample(unique(pop_indicateur$esp_code_alternatif), 5) # Ech de 5 espèces
graphique <- pop_indicateur %>% 
  filter(esp_code_alternatif%in%mes_do) %>% 
  ggplot(aes(x= annee, y =valeur, group = statut, color = statut)) + 
  geom_line() +
  facet_grid(indicateur ~ esp_code_alternatif , scales = "free") + 
  scale_color_manual(values= pal) +
  labs(title = "Les différents indicateurs de tendances à l'échelle régionale pour 5 espèces de poissons d'eau douce", 
       x = "Années", 
       y = "Valeurs")
print(graphique)



################################################################################
################### OBSERVATION DES TENDANCES GLOBALES #########################

# On souhaite observer les différentes tendances présentes sur les différents
# indicateurs calculés : 
# Indicateurs démographiques : Les tendance populationnelles seront d’abord 
# évaluées par site. Les méthodes restent à préciser mais sont envisagés des 
# tests de Mann-Kendall : 

tendance_indicateur <- mk_st_by_group(ope_indicateur,
                                      var_x = annee,
                                      var_y = valeur,
                                      esp_code_alternatif,
                                      indicateur,
                                      statut, 
                                      pop_id)


mes_id_1 <- sample(unique(tendance_indicateur$esp_code_alternatif), 1) 
tendance_indicateur %>% 
  filter(esp_code_alternatif%in% mes_id_1) %>% 
  group_by(indicateur,
           trend,
           statut,
           esp_code_alternatif) %>% 
  summarise (n = n_distinct (pop_id)) %>% 
  ggplot(mapping = aes(x = trend,
                       y = n,
                       fill = trend)) +
  geom_bar(stat="identity") +
  facet_grid( ~ statut ~ indicateur ~ esp_code_alternatif, 
              scales = "free") +
  labs(title = "Tendances indicateurs", 
       x = "Trend", 
       y = "Valeurs") +
  theme_bw() +
  scale_color_manual(values = pal)


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

densites_surface <- ope_indicateur %>% 
  filter(indicateur == "densite_surface")  

taux_occurrence_densite_surface <- tab_oc %>% 
  left_join(densites_surface) %>% 
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


################################################################################
############# OBSERVATION DES TENDANCES SUR LES TAUX D'OCCURRENCE ###############

tendance_indicateur_t_oc_densite_surface <- mk_st_by_group(taux_occurrence_densite_surface,
                                      var_x = annee,
                                      var_y = valeur,
                                      esp_code_alternatif,
                                      indicateur,
                                      statut)


mes_id_2 <- sample(unique(tendance_indicateur_t_oc_densite_surface$esp_code_alternatif), 9) 
tendance_indicateur_t_oc_densite_surface %>% 
  filter(esp_code_alternatif%in% mes_id_2) %>% 
  group_by(trend,
           esp_code_alternatif,
           statut) %>% 
  summarise (n = mk_pvalue) %>% 
  ggplot(mapping = aes(x = trend,
                       y = n,
                       fill = trend)) +
  geom_bar(stat="identity") +
  facet_grid( ~ statut ~ esp_code_alternatif, 
              scales = "free") +
  labs(title = "Tendances des taux d'occurrence", 
       x = "Trend", 
       y = "Valeurs") +
  theme_bw() +
  scale_color_manual(values = pal)




################################################################################
############################## TAUX EVOLUTION ##################################
################################################################################
# Le taux d'évolution : On souhaite calculer le taux d'évolution inter-annuel, 
# c'est à dire entre l'année n et l'année n-1, par espèce. Si le taux d'évolution
# est positif, cela correspond à une croissance de la population ; si il est 
# négatif, cela correspond à un déclin. 

source(file = "R/calcul_taux_evolution.R")

# Taux d'évolution du taux d'occurence de densité de surface : 
# FONCTION : 

calcul_taux_evol_oc_densite_surface <- calcul_taux_evolution(taux_occurrence_densite_surface,
                                                             annee,
                                                             statut,
                                                             esp_code_alternatif,
                                                             valeur)

# Représentation graphique :
ggplot(calcul_taux_evol_oc_densite_surface, aes(x = annee)) +
  geom_line(aes(y = Taux_evol_ANG, color = "ANG")) +
  geom_line(aes(y = Taux_evol_ABL, color = "ABL")) +
  geom_line(aes(y = Taux_evol_BRE, color = "BRE")) +
  geom_line(aes(y = Taux_evol_BOU, color = "BOU")) +
  # Ajoutez d'autres lignes pour chaque espèce
  labs(title = "Évolution des taux d'évolution par espèce",
       x = "Année",
       y = "Taux d'évolution") +
  theme_minimal() +
  scale_color_manual(values= pal)


########### Pourcentage des stations pour laquelle l'espèce
#### est présente une année donnée (et aussi en fonction de son statut)

# !!!! Attention !!!!! La fonction ne marche pas
#source(file = "R/calcul_pourcentage_presence_station.R")
#calcul_pourcentage_presence_station <- calcul_pourcentage_presence_station(pop_indicateur,
#                                                                           esp_code_alternatif,
#                                                                           statut,
#                                                                           pop_id,
#                                                                           annee)