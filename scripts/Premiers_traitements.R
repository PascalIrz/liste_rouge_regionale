
#### PREMIERS TRAITEMENTS ----

# Objectif : Il s'agit d'un premier scirpt de prise en main du package ASPE. 


# --- Chargement des packages : 
library(aspe)
library(tidyverse)
library(mapview)
library(COGiter)
library(sf)
source(file = "R/borner_series.R")

# --- Récupération du fichier de données le plus récent du dépôt : 
rdata_tables <- misc_nom_dernier_fichier(
  repertoire = "../../../projets/ASPE/raw_data/rdata",
  pattern = "^tables")

# --- Chargement du fichier de données : 
load(rdata_tables)



####  PARAMETRES ----

# Séries de pêches avec au moins 9 années de données : 
n_mini_annee <- 9 

# Dans ce n_mini_annee : opérations de pêches successives éloignées d'un maximum 
# de (en années) : 
n_max_manquant <- 2


#### Création d'une parcelle : 

passerelle <- mef_creer_passerelle()
names(passerelle)


# Sélection de l'air géographique à traiter : La Bretagne ----
# --- Sélection des départements : 

passerelle <- passerelle %>% 
  mef_ajouter_dept() %>% 
  filter(dept %in% c(22, 29, 35, 56))


# Sélection des différents réseaux de pêches : RCS / RRP / RHP ----
# --- Sélection des réseaux : 

passerelle <- passerelle %>%
  mef_ajouter_objectif() %>% 
  filter(obj_libelle %in% c("RCS – Réseau de Contrôle de Surveillance",
                            "RRP – Réseau de Référence Pérenne",
                            "RHP – Réseau Hydrobiologique Piscicole"))


#### Réalisation des Opération graphiques : ----

station <- station %>%
  left_join(y = ref_type_projection,
            by = c("sta_typ_id" = "typ_id"))


coords_wgs84 <- geo_convertir_coords_df(df = station,
                                        var_x = sta_coordonnees_x,
                                        var_y = sta_coordonnees_y,
                                        var_id = sta_id,
                                        var_crs_initial = typ_code_epsg,
                                        crs_sortie = 4326) %>%
  rename(x_wgs84 = X, y_wgs84 = Y)

station <- station %>%
  left_join(y = coords_wgs84) %>%
  select(-(sta_geometrie:typ_code_epsg))

names(station)


station_geo <- station %>%
  sf::st_as_sf(coords = c("x_wgs84", "y_wgs84"),
               crs = 4326)

station_geo %>%
  sample_n(1000) %>% 
  mapview::mapview()



# --- Sélection des départements bretons :
mes_depts <- departements_metro_geo %>%
  filter(DEP %in% c("22", "29", "35", "56"))

# --- Attribution et filtres : 
station_bzh <- station_geo %>% 
  aspe::geo_attribuer(regions_metro_geo) %>% 
  filter(REG == "53")

# Visualisation des départements bretons et de leurs stations

station_bzh %>%
  mapview::mapview()



# Opération de pêche sur le secteur Bretagne

mes_ope <- mef_creer_passerelle() %>%
  select(sta_id:ope_id) %>%
  distinct() %>%
  mef_ajouter_type_protocole() %>%
  filter(str_detect(pattern = "Pêche",
                    pro_libelle)) %>%
  mef_ajouter_dept() %>%
  filter(dept %in% c("22", "29", "35", "56"))


### Détection des erreurs dans le jeu de données : ----
# Compter le nombre de points de prélèvements par station (vérification des doublons)

df <- mes_ope %>% 
  mef_ajouter_libelle_site(origine_libelle = "station_sandre") %>% 
  rename(sta_libelle = sta_libelle_sandre) %>%  
  mef_ajouter_libelle_site(origine_libelle = "auto") %>% 
  select(-dept, -pro_libelle, -ope_id) %>% 
  distinct()


sta_plusieurs_pop <- df %>% 
  group_by(sta_id) %>% 
  summarise(n_pop = n_distinct(pop_id)) %>% 
  filter(n_pop>1, !is.na(sta_id)) %>% 
  pull(sta_id)


sta_plusieurs_pop <- df %>% 
  filter(sta_id %in% sta_plusieurs_pop) %>% 
  arrange(sta_id)

# Dans sta_plusieurs_pop : 3 stations possèdent 2 points de prélèvements.
# Nous souhaitons vérifier qu'il s'agit de points regroupables pour allonger 
# au maximum nos séries temporelles.  


# Recherche du nombre d'années de données disponibles par station : ----
annee_de_donnee <- mes_ope %>%
  mef_ajouter_ope_date()

resultat <- annee_de_donnee %>% 
  group_by (pop_id) %>% 
  summarise(n_annee = n_distinct(annee), 
            premier_annee=min(annee),dernier_annee=max(annee),duree=dernier_annee-premier_annee)

mes_pop_id <- resultat %>% 
  filter(n_annee > n_mini_annee) %>% 
  pull(pop_id)

colnames(resultat)[2] <- "nombre_annees_totales"
print(resultat)


# Représentation graphique des données : 
annee_de_donnee %>% 
  filter(pop_id %in% mes_pop_id) %>% 
  ggplot(aes(x = as.character(pop_id),
           y = annee, 
           col= pro_libelle)) + 
  geom_point() + coord_flip()


annee_de_donnee %>% 
  filter(pop_id %in% mes_pop_id) %>% 
  mef_ajouter_libelle_site() %>% 
  ggplot(aes(x = as.character(pop_libelle),
             y = annee, 
             fill= pro_libelle)) + 
  geom_tile() + coord_flip()



# Nouveau paramètre : on fixe le nombre d'années sans données maximum consécutif
# sur une série pour une station


test <- annee_de_donnee %>% 
  group_by (pop_id) %>% 
  arrange (annee, .by_group = TRUE) %>% 
  mutate(annee_precedente = lag(annee),
         annees_manquantes = annee - annee_precedente - 1)%>% 
  filter(annees_manquantes > n_max_manquant)



### Affinage des stations sélectionnées du jeu de données : ----

# Choix de la pêche d'automne (la plus tardive) en cas de 2 pêches ou plus par an. 
annee_de_donnee <- annee_de_donnee %>% 
  group_by(pop_id, annee) %>% 
  filter(ope_date == max(ope_date))


# Création d'un df avec le nombre d'années d'opération total
series <- annee_de_donnee %>% 
  borner_series(var_id_site = pop_id,
                var_temp = annee)

# Filtre pour le nombre minimum d'années d'opération total (fixé dans les paramètres)
series <- series %>% 
  filter(n_opes > n_mini_annee)

# On obtient 44 stations ! 