#_______________________________________________________________________________
################## ASSEMBLAGE DES TABLEAUX PAR OPERATION #######################
#_______________________________________________________________________________


# Objectif : Ce script propose la constitution de tableaux nécessaires à la 
# préparation des analyses temporelles. 


## Chargement des packages ----
#install.packages("khroma")
#install.packages("lemon")
#install.packages("ggthemes")

#library(lemon)
#library (khroma)
library(ggthemes)
library(tidyverse)
library(aspe)
library(ggplot2)
library(dplyr)


## Chargement des données ----

load(file = "processed_data/selection_pop_ope.rda")
load(file = "processed_data/pre_traitements_donnees_env.rda")

rdata_tables <- misc_nom_dernier_fichier(
  repertoire = "../../../projets/ASPE/raw_data/rdata",
  pattern = "^tables")
load(rdata_tables)

mei_table <- misc_nom_dernier_fichier(
  repertoire = "../../../projets/ASPE/raw_data/rdata",
  pattern = "^mei")

load(mei_table)
source(file = "R/calcul_biomasse.R")
source(file= "R/calcul_50_percentile.R")
source(file= "R/calcul_ecart_interquartile.R")
source(file = "R/calcul_25_percentile.R")
source(file = "R/calcul_75_percentile.R")
source(file = "R/calcul_densite_surface.R")

#_______________________________________________________________________________
##############################  PARAMETRES #####################################
#_______________________________________________________________________________

## Selection des espèces (vecteur 1) et de leurs tailles adultes minimum (vecteur 2) ----
vecteur1 <- c("ABH","ABL","BBG","ALF","CTI","ANG","ATB","BOU","BRB","BRE",
              "BRO","CAG","CAA","CCO","CHA","CHE","EPI","EPT","FLE","GAH",
              "GAR","GOU","ALA","GRE","IDE","LPP","LPR","LPM","LOF","MUP",
              "SDF","PER","PES","PLI","PCH","PSR","ROT","SAN","SAT","SIL",
              "SPI","TAN","TAC","TRF","VAI","VAR","VAN","TRM","ASP","CMI",
              "GBT","CCU")
vecteur2 <- c(55,100, 137.5,490,775,425,50,40.5,200,275,330,300,200,475,65,250,
              35,40,215,30,68.5,110,400,57.5,300,150,250,700,55,285,270,150,80,
              270,200,52.5,200,300, 400,700,100,240,212.5,185,70,140,137,185,
              400,450,38,500)


## Sélection des espèces à retirer du jeu de données initial ----
especes_a_retirer <- c("APP","ASL","BRX","CAX","CYP","HBG","OCL","PCC",
                       "PFL","CCU","CAS")


# Selection du nombre de passage souhaité ----
# Remarque : si je souhaite enlever les premiers passages je dois sélectionner"0" et "1" 
passage_a_retirer <- c("2","3")


# Selection des lots à retirer du jeu de données ----
# tyl_libelle  = G (cf ref_type_lot)
type_lot_a_retirer <- c("G")


# Selection des types de pêches à inclure au jeu de données (pro_libelle) ---- 
type_peche_a_inclure <- c("Pêche complète à un ou plusieurs passages",
                          "Pêche partielle par points (grand milieu)",
                          "Pêche par ambiances",
                          "Pêche partielle sur berge")


#_______________________________________________________________________________
##################### CONSTITUTION JEU DE DONNEES ##############################
#_______________________________________________________________________________

# Ajout des mesures individuelles en partant de mes ope_selection ----
ope_selection <- passerelle %>%
  mef_ajouter_ope_date() %>% 
  mef_ajouter_mei() %>%
  mef_ajouter_lots() %>% 
  mef_ajouter_type_protocole() %>% 
  mef_ajouter_passage() %>% 
  mef_ajouter_type_lot() %>% 
  mef_ajouter_type_longueur() %>% 
  select(ope_id,
         lop_id,
         lop_effectif,
         esp_code_alternatif,
         mei_id,
         sta_id,
         pop_id,
         mei_taille,
         pas_numero,
         tyl_libelle,
         pro_libelle,
         annee,
         tlo_libelle)


# Suppression des espèces, des passages, des répétitions et des lots non souhaités du jeu de données ----
ope_selection <- subset(ope_selection, !esp_code_alternatif %in% especes_a_retirer)
ope_selection <- subset(ope_selection, !pas_numero %in% passage_a_retirer)
ope_selection <- subset(ope_selection, pro_libelle %in% type_peche_a_inclure)
ope_selection <- subset(ope_selection, !tyl_libelle%in% type_lot_a_retirer)
ope_selection <- distinct(ope_selection)


# Remplacement des passages = "NA" en "0" (= premier et unique passage) ----
ope_selection <- ope_selection %>% 
  dplyr::mutate(pas_numero = replace_na(pas_numero,0))


#Suppression des "NA" restants ----
ope_selection <- na.omit(ope_selection)



#_______________________________________________________________________________
######################## DISTINCTION DES CLASSES D'AGES ########################
#_______________________________________________________________________________

### Création df avec les mesures seuils d'adultes calculées ---- 
tab_ref_taille <- data.frame(esp_code_alternatif=vecteur1, taille_min_adu=vecteur2) #PARAMETRE

### Ajout de la colonne "taille_min_adulte" par espèces ----
ope_selection <- merge(ope_selection, tab_ref_taille, by ="esp_code_alternatif")

# Ajout du statut par individus ----
ope_selection$statut <- ifelse (ope_selection$mei_taille < ope_selection$taille_min_adu, "juvénile","adulte")


#_______________________________________________________________________________
################# CALCUL DES INDICATEURS PAR OPERATIONS DE PECHE ###############
#_______________________________________________________________________________

# Indicateurs sur les mesures individuelles des poissons : 

########################### LONGUEURS MEDIANES ################################
resultats_longueur_mediane <- calcul_50_percentile(ope_selection,mei_taille,ope_id,esp_code_alternatif, statut)
ope_50_percentile <- resultats_longueur_mediane$df1 # Construction d'un Df avec les longueurs médianes des espèces par opération toutes tailles confondues + des différents statuts ----
ope_50_percentile_esp <- resultats_longueur_mediane$df2 # Construction d'un Df avec les longueurs médianes des espèces par opération toutes tailles confondues ----
ope_50_percentile_statut <- resultats_longueur_mediane$df3 # Construction d'un Df avec les longueurs médianes des différents statuts des espèces (juvéniles / adultes) ----


#########################    ECART INTERQUARTILE   #############################
resultats_ecart_interquartile <- calcul_ecart_interquartile(ope_selection,mei_taille,ope_id,esp_code_alternatif, statut)
ope_ecart_interqua <- resultats_ecart_interquartile$df1 # Construction d'un df de l'écart interquartile des tailles des différents statuts des espèces (juvéniles / adultes) ----
ope_ecart_interqua_esp <- resultats_ecart_interquartile$df2 # Construction d'un Df de l'écart interquartile des espèces par opération toutes tailles confondues ----
ope_ecart_interqua_statut <- resultats_ecart_interquartile$df3 # Construction d'un Df avec les écarts interquartiles des espèces par opération toutes tailles confondues + des différents statuts ----


######################### PERCENTILES (25) ###################################
resultats_25_percentile <- calcul_25_percentile(ope_selection,mei_taille,ope_id,esp_code_alternatif, statut)
ope_25_percentile <- resultats_25_percentile$df1 # Construction d'un df des percentiles 25 des tailles des différents statuts des espèces (juvéniles / adultes) ----
ope_25_percentile_esp <- resultats_25_percentile$df2 # Construction d'un Df des percentiles 25 des espèces par opération toutes tailles confondues ----
ope_25_percentile_statut <- resultats_25_percentile$df3 # Construction d'un Df ades percentiles 25 des espèces par opération toutes tailles confondues + des différents statuts ----


######################### PERCENTILES (75) ###################################
resultats_75_percentile <- calcul_75_percentile(ope_selection,mei_taille,ope_id,esp_code_alternatif, statut)
ope_75_percentile <- resultats_75_percentile$df1 # Construction d'un df des percentiles 75 des tailles des différents statuts des espèces (juvéniles / adultes) ----
ope_75_percentile_esp <- resultats_75_percentile$df2 # Construction d'un Df des percentiles 75 des espèces par opération toutes tailles confondues ----
ope_75_percentile_statut <- resultats_75_percentile$df3 # Construction d'un Df des percentiles 75 des espèces par opération toutes tailles confondues + des différents statuts ----



# indicateurs sur les opérations de pêches : 

##################### EFFECTIF TOTAL ####################################

ope_effectif_total_statut <- ope_selection %>%
  group_by(ope_id,
           esp_code_alternatif,
           statut) %>%
  summarise(valeur=sum(length(mei_id))) %>% 
  mutate(indicateur= "effectif_total") %>% 
  select(ope_id,
         esp_code_alternatif,
         indicateur,
         valeur,
         statut)

ope_effectif_total_esp <- ope_effectif_total_statut %>% 
  group_by(ope_id,
           esp_code_alternatif) %>% 
  summarise(valeur=sum(valeur)) %>% 
  mutate(indicateur= "effectif_total") %>%
  mutate (statut = "toutes") %>% 
  select(ope_id,
         esp_code_alternatif,
         indicateur,
         valeur,
         statut)

ope_effectif_total <- bind_rows(ope_effectif_total_statut, ope_effectif_total_esp)


############################     BIOMASSE    ###################################
# On calcul la biomasse par espèce et par statut : 
source(file = "R/calcul_biomasse.R")
ope_biomasse_statut <- calcul_biomasse(ope_selection,
                                       ope_id,
                                       esp_code_alternatif,
                                       statut,
                                       mei_taille,
                                       tlo_libelle)
ope_biomasse_statut <- ope_biomasse_statut %>% 
  mutate(indicateur = "biomasse")


ope_biomasse_esp <- ope_biomasse_statut %>% 
  group_by(ope_id,
           esp_code_alternatif) %>% 
  summarise(valeur = sum(valeur)) %>% 
  mutate(indicateur= "biomasse") %>%
  mutate (statut = "toutes") %>% 
  select(ope_id,
         esp_code_alternatif,
         indicateur,
         valeur,
         statut)

ope_biomasse <- bind_rows(ope_biomasse_statut, ope_biomasse_esp)


######################    DENSITE DE SURFACE   ####################
# Ajout des surfaces échantillonnées dans ope_selection ----
ope_selection <- ope_selection %>% 
  left_join (y=operation %>% 
               select (ope_id, 
                       ope_surface_calculee,
                       passage$pas_numero))


# Vérification JDD : J'ai autant de ligne lop_id que lop_effectif (sauf quand le lot est S/M et à une valeur de 30)
verif_effectif <- ope_selection %>% 
  group_by(lop_id,lop_effectif) %>% 
  summarise(nbr_lignes = n()) %>% 
  ungroup()
resultat_verif_effectif <- verif_effectif %>% 
  filter(nbr_lignes!= lop_effectif)

# Vérification JDD : Je n'ai qu'un mei_id par ligne (et pas de doublons) : 
# nb_unique doit être égal au nombre total de lignes dans la colonne. 
nb_unique <- ope_selection %>% 
  summarise(nb_unique = n_distinct(mei_id))
print(nb_unique)


# Ajout des effectifs dans un df ope_densite_statut ----
ope_densite_statut_eff <- ope_selection %>% 
  group_by(ope_id,
           esp_code_alternatif,
           ope_surface_calculee,
           statut) %>% 
  summarise(effectif=sum(length(mei_id))) %>% 
  ungroup() %>% 
  mutate(indicateur= "densite_surface")


source(file = "R/calcul_densite_surface.R")
resultats_densite <- calcul_densite_surface(ope_selection,
                                            ope_surface_calculee,
                                            ope_id,esp_code_alternatif, 
                                            statut,
                                            mei_id)

ope_densite_surface <- resultats_densite$df1 
ope_densite_surface_esp <- resultats_densite$df2 
ope_densite_surface_statut <- resultats_densite$df3 



######################### DENSITE VOLUME ######################################

#Ajout des profondeurs pour réaliser les densités volumiques 
ope_selection_param_profondeur <- ope_selection_param_env %>% 
  filter(parametre == "profondeur") %>% 
  select(ope_id,
         valeur) %>% 
  rename(ope_valeur_profondeur=valeur) %>% 
  distinct()


ope_densite_surface_1 <- ope_densite_surface %>% 
  rename(valeur_ds = valeur)


ope_densite_vol<- left_join(ope_selection_param_profondeur, ope_densite_surface_1, by = "ope_id") %>% 
  mutate(valeur = valeur_ds /ope_valeur_profondeur) %>%
  ungroup() %>% 
  mutate(indicateur = "densite_volumique") %>% 
  select(ope_id, 
         esp_code_alternatif, 
         indicateur,
         valeur,
         statut)

ope_densite_volume_statut <- ope_densite_vol %>% 
  filter (statut == "juvénile"| statut == "adulte")

ope_densite_volume_esp <- ope_densite_vol %>% 
  filter (statut == "toutes")


########################## POURCENTAGE DE JUVENILES ############################
# Calcul des pourcentages de juvéniles pour les différents statuts des espèces (juvéniles / adultes) ----
ope_pourcentage_juveniles_statut_eff <- ope_selection %>%
  group_by(ope_id,
           esp_code_alternatif,
           ope_surface_calculee,
           statut) %>% 
  summarise(effectif=sum(length(mei_id))) %>% 
  ungroup() %>% 
  mutate(indicateur = "densite_surface")



# Calcul des pourcentages de juvéniles pour les différents statuts des espèces (juvéniles / adultes) ----
ope_pourcentjuv_esp_eff <- ope_densite_statut_eff %>% 
  group_by(ope_id, esp_code_alternatif) %>% 
  summarise (total_effectif = mean(sum(effectif))) 

ope_pourcentjuv_juv_eff <- ope_densite_statut_eff %>% 
  filter(statut == "juvénile") %>% 
  group_by(ope_id, esp_code_alternatif) %>% 
  summarise(total_juveniles = mean(sum(effectif)))

ope_pourcentjuv_esp <- left_join(ope_pourcentjuv_esp_eff, ope_pourcentjuv_juv_eff, by = c("ope_id", "esp_code_alternatif")) %>%
  mutate(total_juveniles = coalesce(total_juveniles, 0)) %>% # Remplacer les NA par 0
  mutate(valeur = round((total_juveniles / total_effectif) * 100,2)) %>%
  mutate(statut = "toutes", indicateur ="pourcentage_juveniles") %>%
  select(ope_id, esp_code_alternatif, indicateur, valeur, statut)

# Ajout des pourcentages de juvéniles aux différents statuts des espèces (juvéniles / adultes) ----
ope_pourcentjuv_statut <- ope_densite_statut_eff %>% 
  select(ope_id, esp_code_alternatif, statut) %>% 
  mutate(indicateur = "pourcentage_juveniles")

ope_pourcentjuv_esp_select <- ope_pourcentjuv_esp %>% 
  select(ope_id, esp_code_alternatif, valeur)

ope_pourcentjuv_statut <- left_join(ope_pourcentjuv_statut, ope_pourcentjuv_esp_select, 
                                    by = c("ope_id", "esp_code_alternatif"))
# Construction d'un Df avec les pourcentages de juvénils des espèces par opération toutes tailles confondues + des différents statuts ----
ope_pourcentjuv <- bind_rows(ope_pourcentjuv_statut, ope_pourcentjuv_esp)







######################## CREATION TABLEAU FINAL EMPILE #######################

# Création du tableau pré-final avec tous les indicateurs calculés
ope_indicateur <- rbind(ope_50_percentile,
                        #ope_ecart_interqua,
                        #ope_25_percentile,
                        #ope_75_percentile,
                        ope_densite_surface,
                        ope_densite_vol,
                        ope_pourcentjuv,
                        ope_biomasse,
                        ope_effectif_total)

# Vérification des valeurs des différents indicateurs (doivent être égales)
table(ope_indicateur$indicateur)



# Ajout des années d'opération au site et à l'année (pop_id) et (ope_date)

ope_indicateur <- ope_indicateur %>% 
  left_join(y=operation %>% 
              select(ope_id,
                     pop_id= ope_pop_id,
                     ope_date)) 

ope_indicateur <- ope_indicateur %>% 
  mef_ajouter_ope_date()

ope_indicateur <- ope_indicateur %>% 
  select(ope_id, 
         esp_code_alternatif,
         indicateur,
         valeur,
         statut,
         pop_id,
         annee)


# SAUVEGARDE ----
save(ope_indicateur,
     file = "processed_data/assemblage_tab_par_ope.rda")

