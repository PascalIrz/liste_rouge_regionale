#_______________________________________________________________________________
################## ASSEMBLAGE DES TABLEAUX PAR OPERATION #######################
#_______________________________________________________________________________


# Objectif : Ce script propose la constitution de tableaux nécessaires à la 
# préparation des analyses temporelles. 


## Chargement des packages ----
#install.packages("khroma")
#install.packages("lemon")
#install.packages("ggthemes")

library(lemon)
library(ggthemes)
library(tidyverse)
library(aspe)
library(ggplot2)
library (khroma)
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



#_______________________________________________________________________________
##################### CONSTITUTION JEU DE DONNEES ##############################
#_______________________________________________________________________________

# Ajout des mesures individuelles en partant de mes ope_selection ----

ope_selection <- passerelle %>% 
  mef_ajouter_mei() %>%
  mef_ajouter_lots() %>% 
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
         tyl_libelle)



# Suppression des espèces, des passages, des répétitions et des lots non souhaités du jeu de données ----

ope_selection <- subset(ope_selection, !esp_code_alternatif %in% especes_a_retirer)
ope_selection <- subset(ope_selection, !pas_numero %in% passage_a_retirer)
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


########################### LONGUEURS MEDIANES ################################
# Construction d'un Df avec les longueurs médianes des différents statuts des espèces (juvéniles / adultes) ----
ope_lm_statut <- ope_selection %>% 
  group_by(esp_code_alternatif, ope_id, statut) %>% 
  summarise(valeur = median(mei_taille))

ope_lm_statut <- ope_lm_statut %>% 
  mutate(indicateur = "longueur_medianne") %>% 
  select(ope_id,
         esp_code_alternatif,
         indicateur,
         valeur,
         statut)


# Construction d'un Df avec les longueurs médianes des espèces par opération toutes tailles confondues ----
ope_lm_esp <- ope_selection %>% 
  group_by(esp_code_alternatif, ope_id) %>% 
  summarise(valeur = median(mei_taille))

ope_lm_esp <- ope_lm_esp %>% 
  mutate(indicateur="longueur_medianne", statut = "toutes") %>% 
  select(ope_id,
         esp_code_alternatif,
         indicateur,
         valeur,
         statut)


# Construction d'un Df avec les longueurs médianes des espèces par opération toutes tailles confondues + des différents statuts ----
ope_lm <- bind_rows(ope_lm_statut, ope_lm_esp)




#########################    ECART INTERQUARTILE   #############################
# Construction d'un df de l'écart interquartile des tailles des différents statuts des espèces (juvéniles / adultes) ----
ope_interqua_statut <- ope_selection %>% 
  group_by(ope_id, esp_code_alternatif, statut) %>% 
  summarise(valeur=IQR(mei_taille, na.rm=TRUE))

ope_interqua_statut <- ope_interqua_statut %>% 
  mutate(indicateur="ecart_interquartile") %>% 
  select(ope_id,
         esp_code_alternatif,
         indicateur,
         valeur,
         statut)


# Construction d'un Df de l'écart interquartile des espèces par opération toutes tailles confondues ----
ope_interqua_esp <- ope_selection %>% 
  group_by(esp_code_alternatif, ope_id) %>% 
  summarise(valeur = IQR(mei_taille, na.rm=TRUE))


ope_interqua_esp <- ope_interqua_esp %>% 
  mutate(indicateur="intervalle_interquartile", statut = "toutes") %>% 
  select(ope_id,
         esp_code_alternatif,
         indicateur,
         valeur,
         statut)

# Construction d'un Df avec les écarts interquartiles des espèces par opération toutes tailles confondues + des différents statuts ----
ope_interqua <- bind_rows(ope_interqua_statut, ope_interqua_esp)





######################### PERCENTILES (25) ###################################
# Construction d'un df des percentiles 25 des tailles des différents statuts des espèces (juvéniles / adultes) ----
ope_percent25_statut <- ope_selection %>% 
  group_by(ope_id,esp_code_alternatif,statut) %>% 
  summarise(valeur=quantile(mei_taille, probs = c(.25), na.rm=TRUE))

ope_percent25_statut <- ope_percent25_statut %>% 
  mutate(indicateur="25_percentiles") %>% 
  select(ope_id,
         esp_code_alternatif,
         indicateur,
         valeur,
         statut)


# Construction d'un Df des percentiles 25 des espèces par opération toutes tailles confondues ----
ope_percent25_esp <- ope_selection %>% 
  group_by(esp_code_alternatif, ope_id) %>% 
  summarise(valeur=quantile(mei_taille, probs = c(.25), na.rm=TRUE))


ope_percent25_esp <- ope_percent25_esp %>% 
  mutate(indicateur="25_percentiles", statut = "toutes") %>% 
  select(ope_id,
         esp_code_alternatif,
         indicateur,
         valeur,
         statut)

# Construction d'un Df avec les percentiles 25 des espèces par opération toutes tailles confondues + des différents statuts ----
ope_percent25 <- bind_rows(ope_percent25_statut, ope_percent25_esp)




######################### PERCENTILES (75) ###################################
# Construction d'un df des percentiles 75 des tailles des différents statuts des espèces (juvéniles / adultes) ----
ope_percent75_statut <- ope_selection %>% 
  group_by(ope_id,esp_code_alternatif,statut) %>% 
  summarise(valeur=quantile(mei_taille, probs = c(.75), na.rm=TRUE))

ope_percent75_statut <- ope_percent75_statut %>% 
  mutate(indicateur="75_percentiles") %>% 
  select(ope_id,
         esp_code_alternatif,
         indicateur,
         valeur,
         statut)

# Construction d'un Df des percentiles 75 des espèces par opération toutes tailles confondues ----
ope_percent75_esp <- ope_selection %>% 
  group_by(esp_code_alternatif, ope_id) %>% 
  summarise(valeur=quantile(mei_taille, probs = c(.75), na.rm=TRUE))


ope_percent75_esp <- ope_percent75_esp %>% 
  mutate(indicateur="75_percentiles", statut = "toutes") %>% 
  select(ope_id,
         esp_code_alternatif,
         indicateur,
         valeur,
         statut)

# Construction d'un Df avec les percentiles 75 des espèces par opération toutes tailles confondues + des différents statuts ----
ope_percent75 <- bind_rows(ope_percent75_statut, ope_percent75_esp)




##########################    DENSITES SURFACIQUES    ##########################
# Ajout des surfaces échantillonnées dans ope_selection ----
ope_selection <- ope_selection %>% 
  left_join (y=operation %>% 
               select (ope_id, 
                       ope_surface_calculee,
                       passage$pas_numero))



#???????????????????????????????????????????????????????????????????????????????
#???????????????????????????????????????????????????????????????????????????????

# Vérification de chose un peu inquiétantes....

verif_effectif <- ope_selection %>% 
  group_by(lop_id,lop_effectif) %>% 
  summarise(nbr_lignes = n()) %>% 
  ungroup()

resultat <- verif_effectif %>% 
  filter(nbr_lignes!= lop_effectif)
# La j'ai deux données qui font le bazar mais bon ... 
#???????????????????????????????????????????????????????????????????????????????
#???????????????????????????????????????????????????????????????????????????????



# Ajout des effectifs dans un df ope_densite_statut ----
ope_densite_statut <- ope_selection %>% 
  group_by(ope_id,
           esp_code_alternatif,
           ope_surface_calculee,
           statut) %>% 
  summarise(effectif=sum(length(mei_id))) %>% 
  ungroup()


# Vérification qu'il y a bien que 1 mei_id par ligne (et pas de doublons) : 
# nb_unique doit être égal au nombre total de lignes dans la colonne. 
nb_unique <- ope_selection %>% 
  summarise(nb_unique = n_distinct(mei_id))
print(nb_unique)



# Calcul des densités (en individus pour 1000 m²) des différents statuts des espèces (juvéniles / adultes) ----
ope_densite_statut <- ope_densite_statut %>% 
  mutate(indicateur= "densite_surface") %>% 
  mutate (valeur = (1000*effectif / ope_surface_calculee))

ope_densite_statut <- ope_densite_statut %>% 
  select(ope_id,
         esp_code_alternatif,
         indicateur,
         valeur,
         statut)

ope_densite_esp <- ope_selection %>% 
  group_by(ope_id,
           esp_code_alternatif,
           ope_surface_calculee) %>% 
  summarise(effectif=sum(length(mei_id))) %>% 
  ungroup()


ope_densite_esp <- ope_densite_esp %>% 
  group_by(esp_code_alternatif, ope_id) %>% 
  summarise(valeur = (1000*effectif / ope_surface_calculee))


ope_densite_esp <- ope_densite_esp %>% 
  mutate(indicateur="densite_surface", statut = "toutes") %>% 
  select(ope_id,
         esp_code_alternatif,
         indicateur,
         valeur,
         statut)


ope_densite <- bind_rows(ope_densite_statut, ope_densite_esp)




########################## POURCENTAGE DE JUVENILES ############################

# Pour calculer le pourcentage de juvénils par espèces et par opération, je débute
# du tableau densites1 qui regroupe les effectifs d'adultes et de juvénils par espèces
# et par opération


totals <- densites1 %>% 
  group_by(ope_id, esp_code_alternatif) %>% 
  summarise (total_effectif = mean(sum(effectif)))

juveniles <- densites1 %>% 
  filter(statut == "juvénile") %>% 
  group_by(ope_id, esp_code_alternatif) %>% 
  summarise(total_juveniles = mean(sum(effectif)))


pourjuv_ope <- left_join(totals, juveniles, by = c("ope_id", "esp_code_alternatif")) %>%
  mutate(total_juveniles = coalesce(total_juveniles, 0)) %>% # Remplacer les NA par 0
  mutate(valeur = round((total_juveniles / total_effectif) * 100,2)) %>%
  mutate(statut = "toutes", indicateur ="pourcentage_juveniles") %>%
  select(ope_id, esp_code_alternatif, indicateur, valeur, statut)



######################## CREATION TABLEAU FINAL EMPILE #######################

# J'empile les différentes valeurs pour créer le tableau pré-final
indicateur_ope <- rbind(ds_ope_global,
                        lm_ope_global,
                        l25_ope_global,
                        l75_ope_global,
                        interqua_ope_global,
                        pourjuv_ope)

#Vérification que nous avons bien la même valeur pour nos différents indicateurs (sauf pour pourcentage juvénils)
table(indicateur_ope$indicateur)


# Je complet le tableau pour associer chaque opération à son site et à son année (pop_id) et (annee)

indicateur_ope <- indicateur_ope %>% 
  mef_ajouter_ope_date() %>% 
  select(ope_id,
         esp_code_alternatif,
         indicateur,
         valeur,
         statut,
         annee)


indicateur_ope <- indicateur_ope %>% 
  left_join(y=operation %>% 
              select(ope_id,
                     pop_id= ope_pop_id))




############################ DENSITE VOLUMIQUE #################################

# Calcul de la densité volumique par opération, espèce et statut "toutes" (juvéniles et adultes confondus)

dsv_ope <- ds_ope %>%
  inner_join(ope_selection_param_env2 %>% filter(parametre == "profondeur"), by = "ope_id") %>%
  mutate(densite_volumique = ds_ope$valeur * ope_selection_param_env2$valeur) %>%
  select(ope_id, esp_code_alternatif, indicateur, valeur = densite_volumique, statut = "toutes")


# Calcul de la densité volumique par opération, espèce et statut "juvéniles", "adultes" et "toutes"

dsv_ope_global <- ds_ope_global %>%
  inner_join(ope_selection_param_env2 %>% filter(parametre == "profondeur"), by = "ope_id") %>%
  mutate(densite_volumique = ds_ope_global$valeur * ope_selection_param_env2$valeur)


# Affichage des tableaux résultants
print(dsv_ope)
print(dsv_ope_global)








################## Indicateurs régionaux :
# Pour les indicateurs calculés au point, on agrège chaque année leur valeur à 
# l'échelle régionale. 

# Le taux d'occurence de chaque espèce est directement calculée annuellement, à l'échelle régionale,
# comme le pourcentage de sites prospectés où l'espèce a été trouvée. 








