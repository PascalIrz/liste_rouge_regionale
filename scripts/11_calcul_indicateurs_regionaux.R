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

# Définition des couleurs de l'OFB en format RGB
pal <- c("#007844", "#92D050", "#0087C1", "#FCEE21", "#00AEEF", "#1D1D1B", "#A97B30", "#B9D9EB")


#_______________________________________________________________________________
##################### CONSTITUTION JEU DE DONNEES ##############################
#_______________________________________________________________________________

# Calcul des données par année et pop_id en calculant la médiane de la valeur de l'indicateur ----
pop_indicateur <- ope_indicateur %>%
  group_by(pop_id, esp_code_alternatif, annee, indicateur, statut) %>% # Est-ce que je conserve le statut ? 
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
  labs(title = "Les différents indicateurs de tendances pour 5 espèces de poissons d'eau douce", 
       x = "Années", 
       y = "Valeurs")
print(graphique)


#_______________________________________________________________________________
##################### CALCUL DU TAUX D'OCCURRENCE ##############################
#_______________________________________________________________________________


#################### Sur les données de densité de surface ####################
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
################### OBSERVATION DES TENDANCES GLOBALES #########################

# On souhaite observer les différentes tendances présentes sur les différents
# indicateurs calculés : 


# Indicateurs démographiques : Les tendance populationnelles seront d’abord 
# évaluées par site. Les méthodes restent à préciser mais sont envisagés des 
# tests de Mann-Kendall : 

# On réduit le jeu de données par site ( pop_id = 41683)

source(file="R/mann_kendall_sen.R")
library(trend)


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
  labs(title = "Diagrammes en baton des tendances indicateurs", 
       x = "Trend", 
       y = "Valeurs") +
  theme_bw() +
  scale_color_manual(values = pal)



################################################################################
############# OBSERVATION DES TENDANCES SUR LES TAUX D'OCCURENCE ###############

################################################################################
#################### Sur les données de densités de surface ####################
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
  labs(title = "Diagrammes en baton des tendances indicateurs de densités de surface", 
       x = "Trend", 
       y = "Valeurs") +
  theme_bw() +
  scale_color_manual(values = pal)



#################### Sur les données de longueur médianes ####################
tendance_indicateur_t_oc_longueur_mediane <- mk_st_by_group(taux_occurrence_longueur_mediane,
                                                           var_x = annee,
                                                           var_y = valeur,
                                                           esp_code_alternatif,
                                                           indicateur,
                                                           statut)


mes_id_2 <- sample(unique(tendance_indicateur_t_oc_longueur_mediane$esp_code_alternatif), 9) 
tendance_indicateur_t_oc_longueur_mediane %>% 
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
  labs(title = "Diagrammes en baton des tendances de longueurs médianes", 
       x = "Trend", 
       y = "Valeurs") +
  theme_bw() +
  scale_color_manual(values = pal)



#################### Sur les données de densité volumique ####################
tendance_indicateur_t_oc_densite_volumique <- mk_st_by_group(taux_occurrence_densite_volumique,
                                                            var_x = annee,
                                                            var_y = valeur,
                                                            esp_code_alternatif,
                                                            indicateur,
                                                            statut)


mes_id_2 <- sample(unique(tendance_indicateur_t_oc_densite_volumique$esp_code_alternatif), 9) 
tendance_indicateur_t_oc_densite_volumique %>% 
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
  labs(title = "Diagrammes en baton des tendances de densite volumique", 
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



#######################   Taux de densité surfacique ######################### 
# Les taux de densité de surface sont des mesures de changement de densité de 
# surface d'une espèce entre deux périodes de temps différentes. Ils représentent
# le pourcentage de variation dans la densité de surface d'une espèce entre 2 
# moments donnés. 


# ATTENTION : Ne marche pas
tab_densite_s <- pop_indicateur %>% 
  select(esp_code_alternatif,
         indicateur,
         valeur,
         annee) %>% 
  filter (indicateur == "densite_surface") %>% 
  select(-indicateur)

tab_densite_s$annee <- as.Date(tab_densite_s$annee, format="%Y")
resultats_densite_surface <- list()
especes <- unique(tab_densite_s$esp_code_alternatif)


for(espece in especes) {
  # Filtrer les données pour l'espèce en cours
  donnees_espece_s <- subset(tab_densite_s, esp_code_alternatif == espece)
  # Trier les données par années
  donnees_espece_s <- donnees_espece_s[order(donnees_espece_s$annee),]
  
  # Calculer les taux de densité de surface
  taux_surface <- c(NA)
  for(i in 2:nrow(donnees_espece_s)){
    densite_surface_actuelle <- donnees_espece_s$valeur[i]
    densite_surface_precedente <- donnees_espece_s$valeur[i-1]
    
    # Gérer les valeurs manquantes
    if(is.na(densite_surface_actuelle) || is.na(densite_surface_precedente)) {
      taux_surface <- c(taux_surface, NA)
    } else {
      taux_surface <- c(taux_surface, ((densite_surface_actuelle / densite_surface_precedente)-1)*100)
    }
  }
  
  # Stocker les résultats dans la liste
  noms_colonne_surface <- paste("Taux_surface_", espece, sep="")
  resultats_densite_surface[[noms_colonne_surface]] <- taux_surface
}

# Convertir la liste de résultats en dataframe
resultats_df <- as.data.frame(resultats_densite_surface)

# Ajouter une colonne 'annee' au dataframe de résultats
resultats_df$annee <- tab_densite_s$annee[-1]
print(resultats_df)



####################### Calcul du pourcentage des stations pour laquelle l'espèce
#### est présente une année donnée (et aussi en fonction de son statut)

resultats_pourcentage_esp_pop <- list() # Créer une liste de résultats


# Parcourir chaque espèce de poissons
especes <- unique(ope_indicateur$esp_code_alternatif)

# Fonction pour calculer le pourcentage de présence
calcul_pourcentage <- function(ope_indicateur, pop_id, annee) {
  stations_par_annee <- aggregate(ope_indicateur$pop_id ~ ope_indicateur$annee,
                                  data = ope_indicateur,
                                  FUN = function(x) length(unique(x)))
  
  pourcentage_presence <- stations_par_annee$pop_id / length(unique(ope_indicateur$pop_id)) * 100
  return(pourcentage_presence)
}

# Parcourir les combinaisons d'espèces et de statuts
for(espece in especes) {
  # Filtrer les données pour l'espèce et le statut en cours
  donnees_espece_statut <- subset(ope_indicateur, ope_indicateur$esp_code_alternatif == espece)
  
  # Calculer le pourcentage de présence
  nom_colonne <- paste("Pourcentage_presence_", espece, "_", statut, sep = "")
  resultats_pourcentage_esp_pop[[nom_colonne]] <- calcul_pourcentage(donnees_espece_statut, pop_id, annee)
}

# Convertir la liste de résultats en dataframe
resultats_df <- as.data.frame(resultats_pourcentage_esp_pop)

# Ajouter une colonne 'annee' au dataframe de résultats
#stations_par_annee <- aggregate(ope_indicateur$pop_id ~ ope_indicateur$annee, data = ope_indicateur, FUN = function(x) length(unique(x)))
resultats_df$annee <- stations_par_annee$annee

# Affichage du dataframe avec les pourcentages de présence
print(resultats_df)


source(file = "R/calcul_pourcentage_presence_station.R")
calcul_pourcentage_presence_station <- calcul_pourcentage_presence_station(pop_indicateur,
                                                                           esp_code_alternatif,
                                                                           statut,
                                                                           pop_id,
                                                                           annee)




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
  theme_minimal()

