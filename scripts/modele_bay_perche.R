
# Modèle spécifique à la PERCHE - package pop_dynmodel : 
# Application de la fonction mod_popgrow


## Chargement des packages ----

library(tidyr)
library(aspe)
library(aspe2)
library(ggplot2)
library(stringr)
library(dplyr)
library(nimble)
library(popdynmodel)
library(mcmcplots)


# Chargement des données ----
# rdata_tables <- misc_nom_dernier_fichier(
#   repertoire = "../../../projets/ASPE/raw_data/rdata",
#   pattern = "^tables")
# load(rdata_tables)
# 
# mei_table <- misc_nom_dernier_fichier(
#   repertoire = "../../../projets/ASPE/raw_data/rdata",
#   pattern = "^mei")
# load(mei_table)



# Paramètres de mon étude - Objectif Liste Rouge Régionale : 

# Les sta_id retenues :
mes_sta_id <- c("10468", "10502","10546","10565","12447",
                "10647", "10734", "10844", "10925", "12436",
                "10950","11067", "11158","11224", "11245", 
                "11290", "11302", "11329", "11432","11456",
                "11539", "11593","11601","11602","11603",
                "11605", "11735", "11743","11745", "11875", 
                "11913", "12007","12060", "12118", "12148", 
                "12291", "12316","12325","12371","12402",
                "12453","12824","13103")



# 1.1 Data processing

#library(devtools)
#devtools::install_github("PascalIrz/aspe")
#devtools::install_github("manue6/aspe2")

data <- mef_creer_passerelle() %>% # generating of link table
  mef_ajouter_ope_date_complete() %>% # adding date, year, month and day
  mef_ajouter_surf_calc() %>% # adding fishing surface
  mef_ajouter_type_protocole() %>% # adding fishing protocol
  mef_ajouter_passage() %>% # adding fishing pass numbers
  left_join(select(point_prelevement, pop_id, pop_bas_id)) %>% # adding basin
  filter(annee %in% 1990:2023, 
         pop_bas_id == 4,
         sta_id == mes_sta_id)

data$pas_numero[is.na(data$pas_numero)] <- 1 # J'ajoute 1 lorsque le passage est "NA"

data <- data %>% 
  filter(pas_numero == 1)



data <- mef_filtrer_operation(data,
                              var_id = pop_id,
                              var_tmp = annee,
                              var_surf = ope_surface_calculee,
                              var_pro = pro_libelle,
                              var_date = mois,
                              default = TRUE)


def <- def_compter_obs(data,
                       var_id = pop_id,
                       var_tmp = annee,
                       var_pro = pro_libelle)


data <- mef_filtrer_obs(data,
                        def,
                        var_id = pop_id,
                        var_pro = pro_libelle,
                        min_obs = 9,
                        max_na_cons = 3,
                        max_pro = 3)


data <- mef_ajouter_lots(data) %>%
  summarise(catch_size = sum(lop_effectif),
            .by = c(pop_id,
                    annee, 
                    esp_code_alternatif, 
                    ope_id, 
                    ope_surface_calculee))



data <- mef_ajouter_absence(data,
                            var_id = pop_id,
                            var_taxon = esp_code_alternatif,
                            var_abs = catch_size,
                            var_obs = ope_id)



# On réalise un tri pour l'espèce PER (La Perche)

data_per <- filter(data, 
                   esp_code_alternatif == "PER") %>%
  mef_ajouter_na(var_id = pop_id,
                 var_taxon = esp_code_alternatif,
                 var_obs = annee,
                 vec_obs = 1990 : 2023)


data_per <- data_per %>%
  mutate(ope_surface_calculee = ifelse(ope_surface_calculee == 0, NA,
                                       ope_surface_calculee)) %>%
  mef_imputevalue(var_id="pop_id",
                  var_tmp="annee",
                  var_imp="ope_surface_calculee")



# Application de la fonction mod_pop_grow sur "data_per" : 

result_per_31_05 <- mod_popgrow(data_per,
                      var_id = "pop_id",
                      var_tmp = "annee",
                      var_cnt = "catch_size",
                      var_surf = "ope_surface_calculee",
                      period = list(c(2000,2013),
                                    c(2010,2023)),
                      n_iter = 1005000,
                      n_thin = 100,
                      n_burnin = 5000)


# save(data,
#      data_per,
#      result_per_31_05,
#      file = "processed_data/modele_bay_result_31_05_per.rda")
