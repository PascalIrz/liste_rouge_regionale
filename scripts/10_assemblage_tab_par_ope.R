# Chargement des packages et des données

library(tidyverse)
library(aspe)

load(file = "processed_data/selection_pop_ope.rda")

rdata_tables <- misc_nom_dernier_fichier(
  repertoire = "../../../projets/ASPE/raw_data/rdata",
  pattern = "^tables")

load(rdata_tables)

mei_table <- misc_nom_dernier_fichier(
  repertoire = "../../../projets/ASPE/raw_data/rdata",
  pattern = "^mei")

load(mei_table)

source(file = "R/calcul_biomasse.R")



## Je complète mon df ope_selection pour calculer mes surfaces échantillonnées

ope_selection <- passerelle %>% 
  left_join(y=lot_poissons %>% 
              select(lop_id,
                     esp_id = lop_esp_id,
                     lop_effectif)) %>% 
  left_join (y=  ref_espece %>% 
               select(esp_id,
                      esp_code_alternatif)) %>% 
  select(-esp_id)


# Calcul des surfaces échantillonnées


ope_selection <- ope_selection %>% 
  left_join (y=operation %>% 
               select (ope_id, ope_surface_calculee))


# Agrégation par opération et par espèces (somme des effectifs par espèce pour chaque opération)

ope_selection <- ope_selection %>% 
  group_by(ope_id,
           esp_code_alternatif,
           ope_surface_calculee) %>% 
  summarise(effectif=sum(lop_effectif)) %>% 
  ungroup()

# Calcul des densités (en individus pour 1000 m²)

ope_selection <- ope_selection %>% 
  mutate (densite_p_1000_m2 = 1000*effectif / ope_surface_calculee) %>% 
  select (-ope_surface_calculee)



# Création de mon df tab_densite_surfacique


ope_selection$indicateur= c("densite_surfacique")


ds_ope <- ope_selection %>% #ds pour densité surfacique
  select(-effectif)
























