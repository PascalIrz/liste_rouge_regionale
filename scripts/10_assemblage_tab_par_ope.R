## Chargement des packages et des données ----

library(tidyverse)
library(aspe)
library(ggplot2)

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


############################# DENSITES SURFACIQUES ###############################
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


ope_selection_surf <- ope_selection %>% 
  left_join (y=operation %>% 
               select (ope_id, ope_surface_calculee))


# Agrégation par opération et par espèces (somme des effectifs par espèce pour chaque opération)

ope_selection_surf <- ope_selection_surf %>% 
  group_by(ope_id,
           esp_code_alternatif,
           ope_surface_calculee) %>% 
  summarise(effectif=sum(lop_effectif)) %>% 
  ungroup()

# Calcul des densités (en individus pour 1000 m²)

ope_selection_densite <- ope_selection_surf %>% 
  mutate (valeur = 1000*effectif / ope_surface_calculee) %>% 
  select (-ope_surface_calculee)



# Création d'une nouvelle colonne "indicateur"


ope_selection_densite <- ope_selection_densite %>% 
  mutate(indicateur= "densite_surfacique")




# Création de mon df ds_ope (qui correspond à mes densite surfacique)
ds_ope <- ope_selection_densite %>% #ds pour densité surfacique
  select(ope_id,
         esp_code_alternatif,
         indicateur,
         valeur,
         -effectif)




########################### LONGUEURS MEDIANNES #################################


# En partant de mes ope_selection je dois maintenant ajouter le tableau des mesures individuelles

ope_selection_lm <- passerelle %>% 
  mef_ajouter_mei() %>%
  mef_ajouter_lots() %>% 
  mef_ajouter_esp() %>% 
  mef_ajouter_type_longueur() %>% 
  select(ope_id,
         lop_id,
         esp_code_alternatif,
         mei_id,
         sta_id,
         pop_id,
         mei_taille)


# Je cherche maintenant à calculer les longueurs mediannes

ope_selection_lmed <- ope_selection_lm %>% 
  group_by(ope_id,esp_code_alternatif) %>% 
  summarise(valeur=median(mei_taille, na.rm=TRUE))

lm_ope <- ope_selection_lmed %>% 
  mutate(indicateur="longueur_medianne") %>% 
  select(ope_id,
         esp_code_alternatif,
         indicateur,
         valeur)



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




# J'empile les différentes valeurs pour créer le tableau pré-final
indicateur_ope <- rbind(ds_ope,lm_ope,l_inter_qua_ope,l_25_ope,l_75_ope)

#Vérification que nous avons bien la même valeur pour nos différents indicateurs
table(indicateur_ope$indicateur)













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
 

# Représentation de mes variables environnements

mes_id <- sample(unique(ope_selection_param_env2$pop_id), 5)
ope_selection_param_env2 %>% 
  filter(pop_id%in% mes_id) %>% 
  ggplot(aes(x=annee, y=valeur)) +
  geom_bar(stat="identity") +
  facet_wrap(vars(pop_id, parametre),
             ncol = 8,
               scales="free_y")
  



