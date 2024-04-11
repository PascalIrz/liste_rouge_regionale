############################  BIOMASSES ET DENSITES ############################

## CHARGEMENT DES PACKAGES ----
library(aspe)
library(tidyverse)
library(sf)
library(ggplot2)
library(rmapshaper)
library(dplyr)
library(mapview)
library(COGiter)

source(file = "R/borner_series.R")

##############################  PARAMETRES #####################################

# Le nombre minimum d'années d'échantillonnage pour mon traitement de données
n_mini_annee <- 9 

# Le nombre de d'années "trou" minimum et maximum dans les données
n_max_manquant <- 2

# La taille du buffer (en mètres)
taille_buffer <- 1000


#########################  Chargement de fonctions ###############################

source(file = "R/calcul_biomasse.R")

#########################  Chargement de données ###############################

rdata_tables <- misc_nom_dernier_fichier(
  repertoire = "../../../projets/ASPE/raw_data/rdata",
  pattern = "^tables")

load(rdata_tables)

rdata_tables <- misc_nom_dernier_fichier(
  repertoire = "../../../projets/ASPE/raw_data/rdata",
  pattern="^mei")

load(rdata_tables)




 # Vérification de la présence des tables dans l'environnement : 

map(.x = ls()[sapply(ls(), function(x) any(is.data.frame(get(x))))], # liste des dataframes
    .f = function(x) {
      nom <- x
      lignes <- x %>% get() %>% nrow()
      colonnes <- x %>% get() %>% ncol()
      data.frame(nom, lignes, colonnes)
    }) %>%
  reduce(rbind) %>%
  DT::datatable()



######################## MISE EN FORME DES DONNEES #############################

passerelle <- mef_creer_passerelle()

# Filtrage sur le département 56 ----

id_stations_56 <- passerelle %>%
  mef_ajouter_dept() %>% # création du champ dept à partir du code Insee station (ou point si manquant)
  filter(dept == '56' & # sélection des observations sur les numéros de dept
           !is.na(sta_id)) %>% # suppression des sta_id manquants
  pull(sta_id) %>%  # extraction des identifiants des stations
  unique()


data <- passerelle %>% 
  filter(sta_id %in% id_stations_56)

data <- data %>%
  mef_ajouter_dept() %>% 
  mef_ajouter_type_protocole() %>%
  filter(pro_libelle %in% c("Pêche complète à un ou plusieurs passages",
                            "Pêche partielle par points (grand milieu)",
                            "Pêche par ambiances",
                            "Pêche partielle sur berge"))

# Incorporation des mesures individuelles ----


data_ind <- data %>%
  mef_ajouter_libelle() %>% # ajout nom station ou point
  mef_ajouter_ope_date() %>% # date et année d'échantillonnage
  mef_ajouter_passage() %>% # numéro du passage pour éventuellement filtrer dessus
  mef_ajouter_lots() %>% # lots
  mef_ajouter_esp() %>% # noms des espèces
  mef_ajouter_mei() %>% # mesures individuelles
  mef_ajouter_type_longueur() %>% # type de longueur mesurée
  select(mei_id,
         sta_id,
         pop_id,
         dept,
         annee,
         ope_date,
         ope_id,
         lop_id,
         esp_code_alternatif,
         esp_nom_commun,
         mei_taille,
         mei_poids,
         mei_poids_estime,
         mei_mesure_reelle,
         tlo_libelle,-mei_mep_id,-tyl_id,-tlo_id) %>%
  mutate(esp_code_alternatif2 = paste0(esp_nom_commun, "(", esp_code_alternatif, ")"))


# Selection de la station l'AER le Croisty dans le 56 - constitution jeu de donnée réduit

data_ind_aer <- data_ind %>%
  filter(sta_id %in% c("11601"))



# utilisation des relations taille - poids du package aspe
# data("taille_poids")

taille_poids <- data_taille_poids %>%
  group_by(esp_code_alternatif,
           tlo_libelle) %>%
  arrange(source) %>%
  slice(1)


# nb vérifier qu'on a bien les tailles pour tous les poissons avant d'appliquer la taille-poids
# combinaisons espèces - type de longueur absentes de la table de conversion
tp_manquantes <- data_ind_aer %>% 
  select(esp_code_alternatif, tlo_libelle) %>% 
  distinct() %>% 
  left_join(y = taille_poids) %>% 
  filter(is.na(a))


# on voit que le pb est que pas mal d'espèces n'ont pas de relation en longueur fourche
# on approxime par la longueur totale
esp_tp_manquantes <- tp_manquantes %>% 
  pull(esp_code_alternatif)


data_ind_aer <- data_ind_aer %>%
  mutate(tlo_libelle = ifelse(esp_code_alternatif %in% esp_tp_manquantes &
                                tlo_libelle == "Fourche",
                              "Totale",
                              tlo_libelle))


data_ind_aer <- data_ind_aer %>% 
  left_join(y = taille_poids,
            by = c("esp_code_alternatif", "tlo_libelle")) %>%
  mutate(mei_taille = mei_taille / 10,   # passage en cm
         poids_tp = a * (mei_taille ^ b),
         erreur_abs = poids_tp - mei_poids,
         erreur_rel = erreur_abs / poids_tp)



# poids_tp manquants : essentiellement des écrevisses. + qq erreurs de saisie
# poids_tp_manquants <- data_ind_aer %>%
#  filter(is.na(poids_tp),!str_detect(string = esp_nom_commun,
#                                     "Ecrevi"))


reg_ope_poids <- data_ind_aer %>% 
  group_by(ope_id, esp_code_alternatif) %>%
  summarise(poids = sum(poids_tp)) %>% 
  ungroup()


reg_dept_poids <- data_ind_aer %>% 
  group_by(esp_nom_commun,
           annee,
           dept) %>% 
  summarise(poids = sum(poids_tp, na.rm = TRUE),
            .groups = "drop")





biomasse <- data %>% 
  calcul_biomasse(df= data, 
                  var_longueur = mei_taille, 
                  var_type_longueur = tlo_libelle,
                  var_taxon = esp_code_alternatif)




