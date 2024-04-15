#############################################################################################
## Préparation d'un extrait du jeu de données nécessaires à l'estimation des indicateurs de tendance 
# sur l'évolution des populations piscicoles bretonnes sur la période 1990-2023 
#############################################################################################

## Chargement des packages ----

library(tidyr)
library(aspe)
library(aspe2)
library(ggplot2)
library(dplyr)
library(nimble)
library(popdynmodel)

load(file = "processed_data/modeles_occupation.rda")  # Chemin d'accès à adapter avec le fichier rda


# Chargement des données ----
rdata_tables <- misc_nom_dernier_fichier(
  repertoire = "../../../projets/ASPE/raw_data/rdata",
  pattern = "^tables")
load(rdata_tables)


#############################################################################################
## Etape 1 : chargement et sélection des données 
#############################################################################################

# Pour une facilité de compréhension je réduit le jeu de données contenant mes indicateurs simples
# (seulement 3 espèces,1 seul stade (indifférencié), et un seul indicateur (densité surface))

mes_especes <- c("ANG", "LOF", "BRO")
mes_stations <- c("12436", "11290", "12402","11743", "12291")

ope_indicateur_reduit <- ope_indicateur %>% 
  filter (esp_code_alternatif %in% mes_especes,
          indicateur == "densite_surface",
          stade == "ind")


# Je prépare mon jeu de données à appliquer avec ASPE2, en sélectionnant seulement
# 3 espèces (pour simplifier les analyses dans un premier temps)

mei_ope_selection_reduit <- mei_ope_selection_t %>% 
  filter (esp_code_alternatif %in% mes_especes,
          sta_id %in% mes_stations)


# Je pars de mon fichier mei_ope_selection_reduit qui ne contient que les 3 espèces (ANG, LOF et BRO) :
# J'applique le package ASPE2 pour la préparation des données : 

don <- mei_ope_selection_reduit %>%
  # ajout de la date et de l'année de chaque opération de pêche
  # mef_ajouter_ope_date() %>% # -> les données de dates sont déjà présentes dans le jeu de données mei_ope_selection_test
  # ajout du mois et de la saison de chaque opération de pêche
  # mef_ajouter_ope_saison() %>%  # -> les données de saison sont déjà présentes dans le jeu de données mei_ope_selection_test
  # sélection des pêches de printemps et d'automne
  filter(!is.na(saison)) %>%
  # ajout des surfaces de pêche
  # mef_ajouter_surf_calc() %>%    # -> les données de surface sont déjà présentes dans le jeu de données mei_ope_selection_test
  # ajout des protocoles de pêche
  # mef_ajouter_type_protocole() %>%     #-> les données de protocole sont déjà présentes dans le jeu de données mei_ope_selection_test
  # sélection des données selon le protocole de pêche
  # filter(pro_libelle %in% c("Pêche complète à un ou plusieurs passages","Pêche partielle par points (grand milieu)","Pêche partielle sur berge","Pêche par ambiances")) %>%
  # ajout du numéro du passage
  # mef_ajouter_passage() %>%   # -> les données de passage sont déjà présentes dans le jeu de données mei_ope_selection_test
  # conservation du premier passage de pêche 
  mutate(pas_numero = replace_na(pas_numero, 1)) %>%
  filter(pas_numero == 1) #%>%
# ajout du poids les lots 
#mef_ajouter_poids() %>%                               #-> les données de poids sont déjà présentes dans le jeu de données mei_ope_selection_test
# ajout des mesures individuelles
# mef_ajouter_mei()



#############################################################################################
## Etape 2 : conservation d'une seule opération de pêche par points de prélèvement et par an
#############################################################################################
don_operation <- mef_filtrer_operation(don,
                                       var_id = pop_id,
                                       var_tmp = annee,
                                       var_surf = ope_surface_calculee,
                                       var_pro = pro_libelle,
                                       var_mei = mei_id,
                                       default=FALSE) %>%
  mef_filtrer_operation(var_id = pop_id,
                        var_tmp = annee,
                        var_pds = lop_poids,
                        var_date = mois,
                        default = TRUE) %>%
  select(sta_id, pop_id, ope_id, lop_id, annee, pro_libelle) %>%
  distinct()

#############################################################################################
## Etape 3 : sélection des points de prélèvements selon la fréquence des opérations de pêche
#############################################################################################
## Estimation du nombre d'années de pêche, d'années consécutives sans pêche, de protocoles
# et de changement de protocoles par points de prélèvement pour chaque combinaison de protocoles
def_popid <- def_compter_obs(don_operation,
                             var_id = pop_id,
                             var_tmp = annee,
                             var_pro = pro_libelle) 

## Sélection des opérations et des points de prélèvements suivis sur une base annuelle
# 1 protocole de pêche, 20 années de pêche minimum et 3 années consécutives sans pêche maximum
don_an <- mef_filtrer_obs(don_operation,
                          def_popid,
                          var_id = pop_id,
                          var_pro = pro_libelle,
                          min_obs = 20,
                          max_na_cons = 3,
                          max_pro = 1)

## Regroupement des opérations et des points de prélèvements sélectionnés
don_popid <- don_an %>% distinct()



###########
# J'ai un problème avec les poids (valeurs manquantes au niveau de l'estimation des poids des lots estimés)
# Blocage pour la suite des traitements ; en + il manque des ope_id mais je ne sais pas pourquoi ! 






#############################################################################################
## Etape 4 : préparation du jeux de données - capture de l'ensemble de la population
#############################################################################################
don_capture <- don_popid %>%
  # ajouter les informations sur les lots 
  mef_ajouter_lots() %>%
  # ajouter les poids des lots 
  mef_ajouter_poids() %>%
  # corriger et complèter les poids des lots à partir des mesures individuelles
  mef_completer_poids() %>%
  # agréger les effectifs et les poids par taxon, opération de pêche et année
  summarise(effectif = sum(lop_effectif),
            poids = sum(lop_poids),
            poids_estime = sum(lop_poids_estime),
            .by = c(pop_id,ope_id,annee,esp_code_alternatif)) %>%
  # ajouter les absences (i.e. non-observation d'un taxon lors d'une opération de pêche sur les points de prélèvement ou celui-ci a été observé au moins une fois)
  mef_ajouter_absence(var_id = pop_id,
                      var_taxon = esp_code_alternatif,
                      var_abs = c(effectif, poids, poids_estime),
                      var_obs= ope_id) %>%
  # ajouter les années sans opération de pêche 
  mef_ajouter_na(var_id = pop_id,
                 var_taxon = esp_code_alternatif,
                 var_obs = annee,
                 vec_obs = 1990:2023) %>%
  # ajouter les surface de pêche et imputer les valeurs manquantes
  mef_ajouter_surf_calc() %>%
  mutate(ope_surface_calculee = ifelse(ope_surface_calculee == 0, NA, ope_surface_calculee)) %>%
  mef_imputevalue(var_id="pop_id", var_tmp="annee", var_imp="ope_surface_calculee") %>%
  mutate(ope_surface_calculee = replace_na(ope_surface_calculee, 1)) %>%
  # ajouter les protocoles de pêche et imputer les valeurs manquantes
  mef_ajouter_type_protocole() %>%
  mef_imputevalue(var_id="pop_id", var_tmp="annee", var_imp="pro_libelle") 


# Modélisation de la croissance à partir des effectifs
mcmc.out <- mod_popgrow(don_capture, 
                        var_id = "pop_id",
                        var_tmp = "annee",
                        var_tax = "esp_code_alternatif",
                        var_cnt = "effectif",
                        var_surf = "ope_surface_calculee",
                        n_iter = 10,
                        n_thin = 1,
                        n_burnin = 1)

# Modélisation de la croissance à partir des effectifs et de la biomasse
mcmc.out <- mod_popgrow(don_capture, 
                        var_id = "pop_id",
                        var_tmp = "annee",
                        var_tax = "esp_code_alternatif",
                        var_cnt = "effectif",
                        var_wei = "biomasse",
                        var_surf = "ope_surface_calculee")

#--------------------------------------------------------------------------------------------
## Sous-échantillonnage des points de prélèvement pour la biomasse
don_biomasse <- don_capture %>%
  # créer la variable biomasse
  mutate(biomasse = ifelse(is.na(poids), poids_estime, poids)) %>%
  # supprimer pour chaque taxon les points de prélèvement ne comportant que des 0 et des NA
  mef_filtrer_presence(var_id = pop_id,
                       var_taxon = esp_code_alternatif,
                       var_obs = biomasse) 





#############################################################################################
## Etape 4 : préparation du jeux de données - capture des adultes
#############################################################################################
don_capture_adt <- don_popid %>%
  mef_ajouter_lots() %>%
  mef_reconstituer_effectif_par_taille() %>%
  mutate(tlo_id = ifelse(tlo_id %in% c(3,NA), 2, tlo_id)) %>%
  mef_reconstituer_effectif_par_stade(var_id = c(pop_id,ope_id,annee), var_taxon = esp_code_alternatif) %>%
  summarise(effectif = sum(std_effectif),
            .by = c(pop_id,ope_id,annee,esp_code_alternatif,stade)) %>%
  # ajouter les absences (i.e. non-observation d'un stade d'un taxon lors d'une opération de pêche sur les points de prélèvement ou le taxon a été observé au moins une fois)
  mef_ajouter_absence(var_id = pop_id,
                      var_taxon = c(esp_code_alternatif, stade),
                      var_abs = effectif,
                      var_obs= ope_id) %>%
  # sélectionner les captures d'adultes
  filter(stade %in% "S2") %>%
  # ajouter les années sans opération de pêche 
  mef_ajouter_na(var_id = pop_id,
                 var_taxon = c(esp_code_alternatif,stade),
                 var_obs = annee,
                 vec_obs = 1990:2023) %>%
  # supprimer pour chaque taxon les points de prélèvement ne comportant que des 0 et des NA
  mef_filtrer_presence(var_id = pop_id,
                       var_taxon = esp_code_alternatif,
                       var_obs = effectif) %>%
  # ajouter les surface de pêche et imputer les valeurs manquantes
  mef_ajouter_surf_calc() %>%
  mutate(ope_surface_calculee = ifelse(ope_surface_calculee == 0, NA, ope_surface_calculee)) %>%
  mef_imputevalue(var_id="pop_id", var_tmp="annee", var_imp="ope_surface_calculee") %>%
  mutate(ope_surface_calculee = replace_na(ope_surface_calculee, 1)) %>%
  # ajouter les protocoles de pêche et imputer les valeurs manquantes
  mef_ajouter_type_protocole() %>%
  mef_imputevalue(var_id="pop_id", var_tmp="annee", var_imp="pro_libelle") 
