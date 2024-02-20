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



# Je retire de mon jeu de données les lignes avec des "mei_id" NA (problème, à avoir avec Thibault !) ----
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


densites <- densites %>% 
  group_by(ope_id,
           esp_code_alternatif,
           ope_surface_calculee,
           pas_numero,
           statut) %>% 
  summarise(effectif=sum(lop_effectif)) %>% 
  ungroup()




# Calcul des densités (en individus pour 1000 m²) ----

densites1 <- densites %>% 
  mutate(indicateur= "densite_surfacique") %>% 
  mutate (valeur = 1000*effectif / ope_surface_calculee)


densites2 <- densites1 %>% 
  group_by(ope_id,
           esp_code_alternatif,
           ope_surface_calculee) %>% 
  summarise(effectif=sum(effectif)) %>% 
  ungroup()


densites_globales <- densites2 %>% 
  group_by(esp_code_alternatif, ope_id) %>% 
  summarise(valeur = 1000*effectif / ope_surface_calculee)


ds_ope <- densites_globales %>% 
  mutate(indicateur="densites_surface", statut = "toutes") %>% 
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
  group_by(ope_id, esp_code_alternatif, statut) %>% 
  summarise (total_effectif = sum(effectif))

juveniles <- totals %>% 
  filter(statut == "juvénile") %>% 
  rename(total_juveniles = total_effectif)


# Fusionner les totaux avec les totaux de juvéniles

result <- totals %>%
  left_join(juveniles, by = c("ope_id", "esp_code_alternatif")) %>%
  mutate(total_juveniles = coalesce(total_juveniles, 0)) %>% # Remplacer les NA par 0
  group_by(ope_id, esp_code_alternatif) %>%
  summarise(total_effectif = sum(total_effectif),
            total_juveniles = sum(total_juveniles)) %>%
  mutate(percentage_juveniles = (total_juveniles / total_effectif) * 100,
         statut = "toutes") %>%
  select(ope_id, esp_code_alternatif, statut, percentage_juveniles, total_effectif)

# Ici j'ai des pourcentages qui vont au dela de 100 donc c'est un problème, a revoir !




######################### INTERVALLE INTERQUARTILE #############################
# Je cherche maintenant à calculer l'intervalle interquartile des longueurs


l_inter_qua_ope <- ope_selection_lm %>% 
  group_by(ope_id,esp_code_alternatif) %>% 
  summarise(valeur=IQR(mei_taille, na.rm=TRUE))

l_inter_qua_ope <- l_inter_qua_ope %>% 
  mutate(indicateur="intervalle_interquartile") %>% 
  select(ope_id,
         esp_code_alternatif,
         indicateur,
         valeur)


######################### PERCENTILES (25) ###################################

l_25_ope <- ope_selection_lm %>% 
  group_by(ope_id,esp_code_alternatif) %>% 
  summarise(valeur=quantile(mei_taille, probs = c(.25), na.rm=TRUE))

l_25_ope <- l_25_ope %>% 
  mutate(indicateur="25_percentiles") %>% 
  select(ope_id,
         esp_code_alternatif,
         indicateur,
         valeur)



######################### PERCENTILES (75) ###################################

l_75_ope <- ope_selection_lm %>% 
  group_by(ope_id,esp_code_alternatif) %>% 
  summarise(valeur=quantile(mei_taille, probs = c(.75), na.rm=TRUE))

l_75_ope <- l_75_ope %>% 
  mutate(indicateur="75_percentiles") %>% 
  select(ope_id,
         esp_code_alternatif,
         indicateur,
         valeur)





########################### DONNEES ENVIRONNEMENTALES #########################


# Je créer un tableau avec mes variables environnement : 

ope_selection_param_env <- passerelle %>% 
  select(-lop_id,
         -pre_id) %>% 
  distinct() %>% 
  mef_ajouter_ope_env() %>% 
  mef_ajouter_ope_date() %>% 
  select(-ope_date,
         -sta_id,
         -distance_mer)


ope_selection_param_env2 <- ope_selection_param_env %>% 
  pivot_longer(altitude:temp_janvier,
               names_to = "parametre",
               values_to= "valeur") 


# GESTION DES NA DANS LE JEU DE DONNEES ENV ---- 

# Je fais apparaître mes NA dans mon jeu de données environnement : 


ope_selection_param_env3 <- left_join(ope_selection_param_env, 
                                      ope_selection_param_env2,
                                      by = "annee")


# Une fois que mes NA seront apparants : je remplace ces NA par les médianes :

ope_selection_param_env3 %>% 
  group_by(parametre, pop_id.x) %>% 
  mutate (valeur = (ifelse (is.na (valeur), 
                           yes = median (valeur, na.rm = TRUE), 
                           no = valeur)))




# Représentation de mes variables environnements (avec facet_wrap) : 

mes_id <- sample(unique(ope_selection_param_env2$pop_id), 2)
ope_selection_param_env2 %>% 
  filter(pop_id%in% mes_id) %>% 
  ggplot(aes(x=annee, y=valeur)) +
  geom_bar(stat="identity", fill = "darkcyan") +
  facet_wrap(vars(pop_id, parametre),
             ncol = 8,
             scales="free_y") +
  labs(title = "Diagrammes en baton des variables environnementales", 
       x = "Années", 
       y = "Valeurs") +
  theme_clean() +
  theme(
    axis.title.y = element_text (color = "#993333"),
    axis.text.y.left = element_text (color = "#993333"))
  


# Représentation de mes variables environnements (avec facet_grid) : mais problème d'échelle


mes_id <- sample(unique(ope_selection_param_env2$pop_id), 2) 
ope_selection_param_env2 %>% 
  filter(pop_id%in% mes_id) %>% 
  ggplot(mapping = aes(x = annee, y = valeur)) +
  geom_bar(stat="identity", fill = "darkcyan") +
  facet_rep_grid(pop_id ~ parametre, 
             scales= "free_y", 
             repeat.tick.labels = TRUE) +
  labs(title = "Diagrammes en baton des variables environnementales", 
       x = "Années", 
       y = "Valeurs") +
  theme_bw()





############################ DENSITE VOLUMIQUE #################################

# Je créer un tableau contenant à la fois les densités surfaciques et les profondeurs

tab_ds <- ds_ope %>% 
  select(ope_id,
         valeur)

tab_prof <- ope_selection_param_env3 %>%
  filter(parametre == "profondeur") %>% 
  select(ope_id.x,
         valeur)


# Ici je suis bloquée car je n'ai pas encore mes valeurs médianes qui remplaces les NA ... donc pas
# la même longueur au niveau de mes colonnes ! 

dv_ope <- tab_ds %>% 
  mutate(valeur = valeur / tab_prof %>% 
           select(valeur))









######################## CREATION TABLEAU FINAL EMPILE #######################

# J'empile les différentes valeurs pour créer le tableau pré-final
indicateur_ope <- rbind(ds_ope) #,lm_ope)#l_inter_qua_ope,l_25_ope,l_75_ope)

#Vérification que nous avons bien la même valeur pour nos différents indicateurs
table(indicateur_ope$indicateur)




