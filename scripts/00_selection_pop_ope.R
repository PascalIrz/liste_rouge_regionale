#_______________________________________________________________________________
#################### CONSTITUTION DU JEU DE DONNEES ###########################
#_______________________________________________________________________________


# Objectif : Ce script propose la constitution d'un jeu de données regroupant
# la sélection de l'air géographique traitée, les réseaux de pêches ciblés, 
# ainsi que la sélectioon des sites et opérations de pêches (dans l'optique)
# de constitution de séries temporelles spécifiques. 


# Chargement des packages ----

library(aspe)
library(tidyverse)
library(mapview)
library(COGiter)
library(sf)
library(ggplot2)
library(rmapshaper)
library(dplyr)
library(wesanderson)

source(file = "R/borner_series.R")


# Chargement des données ----

rdata_tables <- misc_nom_dernier_fichier(
  repertoire = "../../../projets/ASPE/raw_data/rdata",
  pattern = "^tables")

load(rdata_tables)


# Chargement de la palette de couleur utilisée : 
pal <- wes_palette("AsteroidCity1")



#_______________________________________________________________________________
##############################  PARAMETRES #####################################
#_______________________________________________________________________________

## La taille du buffer (en mètres) ----
taille_buffer <- 1000


## Les réseaux de pêche ----

mes_reseaux <- c("RCS – Réseau de Contrôle de Surveillance",
                 "RRP – Réseau de Référence Pérenne",
                 "RHP – Réseau Hydrobiologique Piscicole")

## Les types de pêche ----

type_de_peche <- c("Pêche complète à un ou plusieurs passages",
                   "Pêche partielle par points (grand milieu)",
                   "Pêche par ambiances",
                   "Pêche partielle sur berge")



## Le nombre minimum d'années sur les séries de pêches  ----
n_mini_annees <- 9 

## Le nombre de d'années "trou" minimum et maximum dans les données ----
n_max_manquant <- 2





#_______________________________________________________________________________
############################## JEU DE DONNEES  ################################
#_______________________________________________________________________________

## Création d'une passerelle ----

passerelle <- mef_creer_passerelle()


## Sélection de l'aire géographique ----
# Echelle départementale : Exemple : La Bretagne

mes_depts <- departements_metro_geo %>% 
  filter (DEP %in% c("22", "29", "35", "56"))

# Visualisation de la zone géographique sélectionnée
mes_depts %>%
  mapview::mapview()


# Par précaution, création d'un buffer pour englober des stations frontalières
# aux limites départementales. La taille du buffer est ajustable dans la section
# des paramètres (ci-dessus).

## Mise en place d'un buffer ----

bzh_buff <- st_buffer(mes_depts, taille_buffer) # PARAMETRES

# Visualisation des départements et du buffer
mapview(
  list(bzh_buff, mes_depts),
  layer.name = c("Bretagne avec un buffer de 1 km", "Bretagne"),
  col.regions = list("#440154FF", "#FDE725FF")
)


# Visualisation des points de prélèvements présents dans la zone géographique
# sélectionnée 
pop_geo <- point_prelevement %>%
  sf::st_as_sf(coords = c("pop_coordonnees_x", "pop_coordonnees_y"),
               crs = 2154)

pop_bzh <- pop_geo %>% 
  aspe::geo_attribuer(bzh_buff) %>% 
  filter(!is.na(DEP))

mapview(
  list(bzh_buff, mes_depts),
  layer.name = c("Bretagne avec un buffer de 1 km", "Bretagne"),
  col.regions = list("#440154FF", "#FDE725FF")
) + mapview (pop_bzh, color = "darkred", lwd =3)


# Sauvegarde des points de prélèvements sélectionnés dans la passerelle
passerelle <- passerelle %>%
  filter(pop_id %in% pop_bzh$pop_id)


## Sélection des réseaux de pêches (cf paramètres) ---- 

passerelle <- passerelle %>%
  mef_ajouter_objectif() %>% 
  filter(obj_libelle %in% mes_reseaux)


## Création d'un dataframe contenant seulement la sélection des pêches choisies ---- 

mes_ope <- passerelle %>% 
  mef_ajouter_type_protocole() %>%
  filter(pro_libelle %in% type_de_peche) %>% 
  select(sta_id:ope_id, pro_libelle) %>% 
  distinct()



## Sélection des séries de pêches avec au moins 9 années de données (paramètres) ----

annee_de_donnees <- mes_ope %>%
  mef_ajouter_ope_date()

bilan_annee_de_donnees <- annee_de_donnees %>% 
  group_by (pop_id) %>% 
  summarise(n_annee = n_distinct(annee), 
            premier_annee=min(annee),dernier_annee=max(annee),duree=dernier_annee-premier_annee)

mes_pop_id <- bilan_annee_de_donnees %>% 
  filter(n_annee > n_mini_annees) %>%  # PARAMETRES
  pull(pop_id)

colnames(bilan_annee_de_donnees)[2] <- "nombre_annees_totales"
print(bilan_annee_de_donnees)



## Représentation graphique de stations sélectionnées (et types de pêches associées) ----

annee_de_donnees %>% 
  filter(pop_id %in% mes_pop_id) %>% 
  mef_ajouter_libelle_site() %>% 
  ggplot(aes(x = as.character(pop_libelle),
             y = annee, 
             fill= pro_libelle)) + 
  scale_fill_manual(values= pal) +
  geom_tile() +
  labs(title = "Les stations sélectionnées et les types de pêches associées",
       subtitle = "Région Bretagne",
       x = "Station", 
       y = "Années",
       fill = "Type de pêche") + 
  theme(legend.background = element_rect(fill="lightgray")) +
  coord_flip()



## Sélection d'une seule pêche par années, par station ----
# (cas des sites avec plusieurs pêches la même année)
annee_de_donnees <- annee_de_donnees %>% 
  group_by(pop_id, annee) %>% 
  filter(ope_date == max(ope_date))



## Sélection des stations ayant un nombre d'années maximum de suivis consécutifs manquant par station (paramètres) ----

series <- annee_de_donnees %>% 
  borner_series(var_id_site = pop_id,
                var_temp = annee,
                max_nb_obs_manquantes = n_max_manquant) # PARAMETRE


series <- series %>% 
  filter(n_opes > n_mini_annees) %>% # PARAMETRE
  select(-annee_mini)



## Construction d'un nouveau dataframe avec toutes les opérations de pêches
# présentes dans les stations retenues

ope_selection <- annee_de_donnees %>% 
  left_join(series) %>% 
  filter(!is.na(debut),
         annee >= debut,
         annee <= fin) %>% 
  select(-debut, -fin, -n_opes) %>% 
  ungroup()

# --- Enregistrement des opérations de pêches retenues dans la passerelle
passerelle <- passerelle %>% 
  filter(ope_id %in%ope_selection$ope_id)



#_______________________________________________________________________________
######################## VERIFICATION DU JEU DE DONNEES  #######################
#_______________________________________________________________________________

### Recherche erreur : 1 point de prélèvement = 1 seule station ----

df1 <- mes_ope %>% 
  mef_ajouter_libelle_site(origine_libelle = "station_sandre") %>% 
  rename(sta_libelle = sta_libelle_sandre) %>%  
  mef_ajouter_libelle_site(origine_libelle = "auto") %>% 
  select(-pro_libelle, -ope_id) %>% 
  distinct()

pop_plusieurs_sta <- df1 %>% 
  group_by(pop_id) %>% 
  summarise(n_sta = n_distinct(sta_id)) %>% 
  filter(n_sta>1, !is.na(pop_id)) %>% 
  pull(pop_id)

 
pop_plusieurs_sta <- df1 %>% 
  filter(pop_id %in% pop_plusieurs_sta) %>% 
  arrange(pop_id)

# On s'attend à ce que le dataframe pop_plusieurs_sta contienne 0 ligne.



### Recherche erreur : 1 station = ? opération(s) (vérification doublons) ----

df <- mes_ope %>% 
  mef_ajouter_libelle_site(origine_libelle = "station_sandre") %>% 
  rename(sta_libelle = sta_libelle_sandre) %>%  
  mef_ajouter_libelle_site(origine_libelle = "auto") %>% 
  select(-pro_libelle, -ope_id) %>% 
  distinct()

sta_plusieurs_pop <- df %>% 
  group_by(sta_id) %>% 
  summarise(n_pop = n_distinct(pop_id)) %>% 
  filter(n_pop>1, !is.na(sta_id)) %>% 
  pull(sta_id)

sta_plusieurs_pop <- df %>% 
  filter(sta_id %in% sta_plusieurs_pop) %>% 
  arrange(sta_id)

# REMARQUE : sta_plusieurs_pop = 3 stations à 2 points de prélèvement (doublons)




# SAUVEGARDE ----
save(series,
     ope_selection,
     passerelle,
     file = "processed_data/selection_pop_ope.rda")
