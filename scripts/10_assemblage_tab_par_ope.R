#_______________________________________________________________________________
################## ASSEMBLAGE TABLEAUX PAR OPERATION ###########################
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

## La liste des espèces souhaitée dans l'analyse et leurs tailles adultes minimum ----

vecteur1 <- c("ABH","ABL","BBG","ALF","CTI",
              "ANG","ATB","BOU","BRB","BRE",
              "BRO","CAG","CAA","CCO","CHA",
              "CHE","EPI","EPT","FLE","GAH",
              "GAR","GOU","ALA","GRE","IDE",
              "LPP","LPR","LPM","LOF","MUP",
              "SDF","PER","PES","PLI","PCH",
              "PSR","ROT","SAN","SAT","SIL",
              "SPI","TAN","TAC","TRF","VAI",
              "VAR","VAN","TRM","ASP",
              "CMI","GBT","CCU")

vecteur2 <-
  c(55,100, 137.5,490,775,425,50,40.5,200,275,330,300,200,475,65,250,35,40,215,
    30,68.5,110,400,57.5,300,150,250,700,55,285,270,150,80,270,200,52.5,200,300,
    400,700,100,240,212.5,185,70,140,137,185,400,450,38,500)



## Les espèces à retirer du jeu de données initial ----
especes_a_retirer <- c("APP","ASL","BRX","CAX","CYP","HBG","OCL","PCC","PFL","CCU","CAS")


# Le nombre de passage souhaiter (ou selection entre le premier ou le second passage)

passage_a_retirer <- c("2","3")

# Remarque : si je souhaite enlever les premiers passages je dois sélectionner
# "0" et "1" (cf ligne 137)

#_______________________________________________________________________________
######################## DISTINCTION DES CLASSES D'AGES ########################
#_______________________________________________________________________________


### Création d'un dataframe avec les mesures seuils d'adultes calculées ---- 
tab_ref_taille <- data.frame(esp_code_alternatif=vecteur1, taille_min_adu=vecteur2) #PARAMETRE


### Création d'un df contenant les valeurs seuils des tailles minimum des adultes ----

age_ind <- ref_espece %>% 
  select(-esp_code_sandre,
         -esp_nom_latin,
         -esp_ordre_affichage,
         -esp_statut,
         -esp_code_ipr,
         -esp_code_taxref,
         -esp_eligible_calcul_ipr,
         -esp_eligible_calcul_iprplus) %>% 
   left_join(tab_ref_taille) %>% 
  select(-esp_id,
         -esp_nom_commun,
         -esp_taille_maximale) 



########################### LONGUEURS MEDIANNES #################################

# Ajout des mesures individuelles en partant de mes ope_selection 

ope_selection <- passerelle %>% 
  mef_ajouter_mei() %>%
  mef_ajouter_lots() %>% 
  mef_ajouter_passage() %>% 
  mef_ajouter_esp() %>% 
  mef_ajouter_type_longueur() %>% 
  select(ope_id,
         lop_id,
         esp_code_alternatif,
         mei_id,
         sta_id,
         pop_id,
         mei_taille,
         pas_numero)


# Je retire les espèces non souhaitées de mon jeu de données : les écrevisses, 
# les espèces indeterminées, ... et les passages non souhaités. 

ope_selection <- subset(ope_selection, !esp_code_alternatif %in% especes_a_retirer)
ope_selection <- subset(ope_selection, !pas_numero %in% passage_a_retirer)


# Remplacer les "NA" des passages en 0 (car c'est un premier et unique passage)

ope_selection <- ope_selection %>% 
  dplyr::mutate(pas_numero = replace_na(pas_numero,0))


# Je retire de mon jeu de données les tailles individuelles issues de lots "G" - inexploitables.
ope_selection <- na.omit(ope_selection)

# Je compose un df avec les longueurs médianes d'une espèce par opération - tailles confondues (lm_ope)
# Je compose un df avec toutes les longueurs médiannes de chacune tailles des espèces (lm_ope_esp)
ope_selection1 <- merge(ope_selection, age_ind, by ="esp_code_alternatif")

ope_selection1$statut <- ifelse (ope_selection1$mei_taille < ope_selection1$taille_min_adu, "juvénile","adulte")

  
medianes <- ope_selection1 %>% 
  group_by(esp_code_alternatif, ope_id, statut) %>% 
  summarise(valeur = median(mei_taille))

medianes <- medianes %>% 
  mutate(indicateur = "longueur_medianne") %>% 
  select(ope_id,
         esp_code_alternatif,
         indicateur,
         valeur,
         statut)

medianes_globales <- ope_selection1 %>% 
  group_by(esp_code_alternatif, ope_id) %>% 
  summarise(valeur = median(mei_taille))

lm_ope <- medianes_globales %>% 
  mutate(indicateur="longueur_medianne", statut = "toutes") %>% 
  select(ope_id,
         esp_code_alternatif,
         indicateur,
         valeur,
         statut)



lm_ope_global <- bind_rows(medianes, lm_ope)


######################### INTERVALLE INTERQUARTILE #############################
# Je cherche maintenant à calculer l'intervalle interquartile des longueurs


l_inter_qua <- ope_selection1 %>% 
  group_by(ope_id,esp_code_alternatif,statut) %>% 
  summarise(valeur=IQR(mei_taille, na.rm=TRUE))

l_inter_qua <- l_inter_qua %>% 
  mutate(indicateur="intervalle_interquartile") %>% 
  select(ope_id,
         esp_code_alternatif,
         indicateur,
         valeur,
         statut)


linterqua_globales <- ope_selection1 %>% 
  group_by(esp_code_alternatif, ope_id) %>% 
  summarise(valeur = IQR(mei_taille, na.rm=TRUE))


l_inter_qua_ope <- linterqua_globales %>% 
  mutate(indicateur="intervalle_interquartile", statut = "toutes") %>% 
  select(ope_id,
         esp_code_alternatif,
         indicateur,
         valeur,
         statut)


interqua_ope_global <- bind_rows(l_inter_qua, l_inter_qua_ope)



######################### PERCENTILES (25) ###################################


l_25 <- ope_selection1 %>% 
  group_by(ope_id,esp_code_alternatif,statut) %>% 
  summarise(valeur=quantile(mei_taille, probs = c(.25), na.rm=TRUE))

l_25 <- l_25 %>% 
  mutate(indicateur="25_percentiles") %>% 
  select(ope_id,
         esp_code_alternatif,
         indicateur,
         valeur,
         statut)


l25_globales <- ope_selection1 %>% 
  group_by(esp_code_alternatif, ope_id) %>% 
  summarise(valeur=quantile(mei_taille, probs = c(.25), na.rm=TRUE))


l25_ope <- l25_globales %>% 
  mutate(indicateur="25_percentiles", statut = "toutes") %>% 
  select(ope_id,
         esp_code_alternatif,
         indicateur,
         valeur,
         statut)


l25_ope_global <- bind_rows(l_25, l25_ope)




######################### PERCENTILES (75) ###################################


l_75 <- ope_selection1 %>% 
  group_by(ope_id,esp_code_alternatif,statut) %>% 
  summarise(valeur=quantile(mei_taille, probs = c(.75), na.rm=TRUE))

l_75 <- l_75 %>% 
  mutate(indicateur="75_percentiles") %>% 
  select(ope_id,
         esp_code_alternatif,
         indicateur,
         valeur,
         statut)


l75_globales <- ope_selection1 %>% 
  group_by(esp_code_alternatif, ope_id) %>% 
  summarise(valeur=quantile(mei_taille, probs = c(.75), na.rm=TRUE))


l75_ope <- l75_globales %>% 
  mutate(indicateur="75_percentiles", statut = "toutes") %>% 
  select(ope_id,
         esp_code_alternatif,
         indicateur,
         valeur,
         statut)


l75_ope_global <- bind_rows(l_75, l75_ope)



############################# DENSITES SURFACIQUES ###############################
## Je complète mon df ope_selection pour calculer mes surfaces échantillonnées



ope_selection <- ope_selection %>%
  left_join(y=lot_poissons %>% 
              select(lop_id,
                     esp_id = lop_esp_id,
                     lop_effectif)) %>% 
  left_join (y=  ref_espece %>% 
               select(esp_id,
                      esp_code_alternatif)) %>% 
  select(-esp_id)


ope_selection2 <- merge(ope_selection, age_ind, by ="esp_code_alternatif")
ope_selection2$statut <- ifelse (ope_selection2$mei_taille < ope_selection2$taille_min_adu, "juvénile","adulte")



# Calcul des surfaces échantillonnées ----

densites <- ope_selection2 %>% 
  left_join (y=operation %>% 
               select (ope_id, 
                       ope_surface_calculee,
                       passage$pas_numero))



densites <- densites %>% 
  select (-ope_code_wama,
          -ope_id_wama,
          -ope_pop_id,
          -taille_min_adu,
          -sta_id)




densites1 <- densites %>% 
  group_by(ope_id,
           esp_code_alternatif,
           ope_surface_calculee,
           pas_numero,
           lop_effectif,
           statut) %>% 
  summarise(effectif=sum(lop_effectif)) %>% 
  ungroup()

# C'est une technique approbable mais ça marche pour rendre mon effectif bon ! 
densites <- densites1 %>% 
  group_by(ope_id,
           esp_code_alternatif,
           ope_surface_calculee,
           pas_numero,
           lop_effectif,
           statut) %>% 
  summarise(effectif=sum(lop_effectif)) %>% 
  ungroup()




# Calcul des densités (en individus pour 1000 m²) ----

densites2 <- densites %>% 
  mutate(indicateur= "densite_surface") %>% 
  mutate (valeur = (1000*effectif / ope_surface_calculee))


densites3 <- densites2 %>% 
  group_by(ope_id,
           esp_code_alternatif,
           ope_surface_calculee) %>% 
  summarise(effectif=sum(effectif)) %>% 
  ungroup()


densites_globales <- densites2 %>% 
  group_by(esp_code_alternatif, ope_id) %>% 
  summarise(valeur = (1000*effectif / ope_surface_calculee))


ds_ope <- densites_globales %>% 
  mutate(indicateur="densite_surface", statut = "toutes") %>% 
  select(ope_id,
         esp_code_alternatif,
         indicateur,
         valeur,
         statut)


ds_ope_global <- bind_rows(densites1, ds_ope) 
ds_ope_global <- ds_ope_global %>% 
  select(-pas_numero,
         -effectif,
         -ope_surface_calculee)



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








