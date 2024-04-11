#### Test Calcul des densités ----

# Objectif : Il s'agit d'un scirpt (exemple) pour utiliser la fonction créée pour 
# calculer les densités i pour chacune des opérations. 


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




# Construction du df avec les surfaces calculées et les données individuelles : ----
data_ind <- passerelle %>%
  mef_ajouter_libelle() %>% 
  mef_ajouter_ope_date() %>% 
  mef_ajouter_ope_env() %>% 
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
         profondeur,
         lop_id,
         esp_code_alternatif,
         esp_nom_commun,
         mei_taille,
         mei_poids,
         mei_poids_estime,
         mei_mesure_reelle,
         tlo_libelle
         )


# Construction d'un df réduit à un point de prélèvement : ----
# Exemple : Selection du pop_id 46671 
data_pop_46671 <- data_ind %>%
  filter(pop_id == "46671") %>% 
  select(mei_id,
         ope_id,
         lop_id,
         annee,
         esp_code_alternatif,
         mei_taille,
         mei_poids,
         tlo_libelle,
         profondeur)


# Calcul de la biomasse de chacun des individus avec la fonction "calcul_biomasse" : ---- 
data_pop_46671 <- calcul_biomasse(df = data_pop_46671)
View(data_pop_46671)


#### Calcul de densité massique (en gramme par mètres carré) ----
# Réduction du df avec la sélection d'une seule operation (exemple)
data_ope_50772 <- data_pop_46671 %>% 
  select(-a,
         -b,
         -source,
         -n_etudes) %>% 
  filter (ope_id == "50772")


# Ajout des surfaces calculées 
data_ope_50772 <- data_ope_50772 %>% 
  left_join(y = operation %>%
              select(ope_id, ope_surface_calculee))


data_ind_densite_50772 <- data_ope_50772 %>% 
  mutate(densite_g_m2 = poids_tp / ope_surface_calculee *1000) %>% 
  select(-ope_surface_calculee)

data_ind_densite_50772 %>%
  DT::datatable(rownames = FALSE)




# Calcul de densité volumique (en gramme par mètres cube) ----

data_ind_densite_50772 <- data_ind_densite_50772 %>% 
  mutate(densite_g_m3 = densite_g_m2 / profondeur) %>% 
  select(-profondeur,
         -tlo_libelle,
         -esp_nom_latin,
         -mei_id,
         -mei_taille,
         -poids_tp,
         -mei_poids)

