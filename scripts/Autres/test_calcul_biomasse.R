
#### Test Calcul des biomasses ----

# Objectif : Il s'agit d'un scirpt (exemple) pour utiliser la fonction créée pour 
# calculer les biomasses individuelles des poissons pour chacune des opérations. 


# --- Chargement des packages : 
library(tidyverse)
library(aspe)
source(file = "R/calcul_biomasse.R")


# --- Chargement du fichier de données : 

load(file = "processed_data/selection_pop_ope.rda")

rdata_tables <- misc_nom_dernier_fichier(
  repertoire = "../../../projets/ASPE/raw_data/rdata",
  pattern = "^tables")

load(rdata_tables)

mei_table <- misc_nom_dernier_fichier(
  repertoire = "../../../projets/ASPE/raw_data/rdata",
  pattern = "^mei")

load(mei_table)


# Création d'un df avec les données individuelles : 

data_ind <- passerelle %>%
  mef_ajouter_libelle() %>% 
  mef_ajouter_ope_date() %>% 
  mef_ajouter_passage() %>% 
  mef_ajouter_lots() %>% 
  mef_ajouter_esp() %>%
  mef_ajouter_mei() %>%
  mef_ajouter_type_longueur() %>%
  select(mei_id,
         sta_id,
         pop_id,
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
         tlo_libelle)


# Construction d'un jeu de données réduit pour entrainement ----
# Exemple : Selection de la station l'AER le Croisty dans le 56 - 

data_ind_aer <- data_ind %>%
 filter(pop_id == "45254") %>% 
 select(mei_id,
        ope_id,
        annee,
        esp_code_alternatif,
        mei_taille,
        mei_poids,
        tlo_libelle)


# Utilisation de la fonction créée pour calculer les biomasses ----
# (à partir des relations tailles-poids qui produise un poids estimé). 

test <- calcul_biomasse(df = data_ind_aer)
View(test)
