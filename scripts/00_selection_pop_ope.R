################################################################################
############################  PREMIERS TRAITEMENTS #############################
################################################################################

## CHARGEMENT DES PACKAGES

library(aspe)
library(tidyverse)
library(mapview)
library(COGiter)
library(sf)
library(ggplot2)
library(rmapshaper)
library(dplyr)


source(file = "R/borner_series.R")

## CHARGEMENT DES DONNEES

rdata_tables <- misc_nom_dernier_fichier(
  repertoire = "../../../projets/ASPE/raw_data/rdata",
  pattern = "^tables")

load(rdata_tables)


################################################################################
##############################  PARAMETRES #####################################

# Le nombre minimum d'années d'échantillonnage pour mon traitement de données
n_mini_annee <- 9 

# Le nombre de d'années "trou" minimum et maximum dans les données
n_max_manquant <- 2

# La taille du buffer (en mètres)
taille_buffer <- 1000

################################################################################
################################################################################


## SELECTION DES DONNEES

passerelle <- mef_creer_passerelle()


# SELECTION GEOGRAHIQUE DEPARTEMENTALE : Bretagne

mes_depts <- departements_metro_geo %>% 
  filter (DEP %in% c("22", "29", "35", "56"))

mes_depts %>%
  mapview::mapview()

pop_geo <- point_prelevement %>%
  sf::st_as_sf(coords = c("pop_coordonnees_x","pop_coordonnees_y"),
               crs = 2154)

pop_geo %>% sample_n(100) %>% mapview()



# CREATION D'UN BUFFER DE 1 km :

bzh_buff_1km <- st_buffer(mes_depts, taille_buffer)


mapview(
  list(bzh_buff_1km, mes_depts),
  layer.name = c("Bretagne avec un buffer de 1 km", "Bretagne"),
  col.regions = list("#440154FF", "#FDE725FF")
)

pop_bzh <- pop_geo %>% 
  aspe::geo_attribuer(bzh_buff_1km) %>% 
  filter(!is.na(DEP))

mapview(
  list(bzh_buff_1km, mes_depts),
  layer.name = c("Bretagne avec un buffer de 1 km", "Bretagne"),
  col.regions = list("#440154FF", "#FDE725FF")
) + mapview (pop_bzh, color = "red", lwd =3)



passerelle <- passerelle %>%
  filter(pop_id %in% pop_bzh$pop_id)


## CREATION D'UN DF AVEC SEULEMENT LES PECHES TYPE INVENTAIRES EN BRETAGNE ----

mes_ope <- passerelle %>% 
  mef_ajouter_type_protocole() %>%
  filter(str_detect(pattern = "Pêche",
                    pro_libelle)) %>% 
  select(sta_id:ope_id, pro_libelle) %>% 
  distinct()

################################################################################
######################## VERIFICATION DU JEU DE DONNEES  #######################
################################################################################

# RECHERCHE ERREUR : 1 point de prélèvement = 1 seule station : 


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


# On s'attend à ce que le dataframe ci-dessus contienne 0 ligne. 
pop_plusieurs_sta <- df1 %>% 
  filter(pop_id %in% pop_plusieurs_sta) %>% 
  arrange(pop_id)


# RECHERCHE ERREUR : 1 stations = ? opérations (vérification doublons / ...)

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


#------------------------------------------------------------------------------
## ANALYSE DU JEU DE DONNEES:
#------------------------------------------------------------------------------

# NOMBRE D'ANNEES DE DONNEES PAR STATION :

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


#-------------------------------------------------------------------------------
# REPRESENTATION GRAPHIQUE PAR BRIQUE AVEC INTITULE :
#-------------------------------------------------------------------------------

annee_de_donnee %>% 
  filter(pop_id %in% mes_pop_id) %>% 
  mef_ajouter_libelle_site() %>% 
  ggplot(aes(x = as.character(pop_libelle),
             y = annee, 
             fill= pro_libelle)) + 
  geom_tile() +
  coord_flip()




# Identification des séries d'opérations séparées maxi de 2 ans

# cas des sites avec plusieurs pêches la même année
annee_de_donnee <- annee_de_donnee %>% 
  group_by(pop_id, annee) %>% 
  filter(ope_date == max(ope_date))

series <- annee_de_donnee %>% 
  borner_series(var_id_site = pop_id,
                var_temp = annee)

series <- series %>% 
  filter(n_opes > 9)

ope_selection <- annee_de_donnee %>% 
  left_join(series) %>% 
  filter(!is.na(debut),
         annee >= debut,
         annee <= fin) %>% 
  select(-debut, -fin, -n_opes) %>% 
  ungroup()
  



ope_selection %>% 
  mef_ajouter_libelle_site() %>% 
  ggplot(aes(x = as.character(pop_libelle),
             y = annee, 
             fill= pro_libelle)) + 
  geom_tile() +
  coord_flip()


#######################
save(series,
     ope_selection,
     passerelle,
     file = "processed_data/selection_pop_ope.rda")

################################################################################
#################### CREATION JEU DE DONNEES FINAL #############################
################################################################################



#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#----------------------- REPRESENTATION GRAPHIQUE ------------------------------
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------


# # SELECTION ZONE GEOGRAPHIQUE NATIONALE : France métropolitaine
# 
# station <- station %>%
#   left_join(y = ref_type_projection,
#             by = c("sta_typ_id" = "typ_id"))
# 
# 
# coords_wgs84 <- geo_convertir_coords_df(df = station,
#                                         var_x = sta_coordonnees_x,
#                                         var_y = sta_coordonnees_y,
#                                         var_id = sta_id,
#                                         var_crs_initial = typ_code_epsg,
#                                         crs_sortie = 4326) %>%
#   rename(x_wgs84 = X, y_wgs84 = Y)
# 
# station <- station %>%
#   left_join(y = coords_wgs84) %>%
#   select(-(sta_geometrie:typ_code_epsg))
# 
# 
# ggplot(regions_metro_geo) +
#   geom_sf(aes(fill=REG)) +
#   geom_sf(data = departements_metro_geo,
#           alpha=0)
# 
# sf::st_crs(regions_metro_geo)
# 
# 
# 
# 
# # SELECTION GEOGRAHIQUE DEPARTEMENTALE : Bretagne
# 
# mes_depts <- departements_metro_geo %>% 
#   filter (DEP %in% c("22", "29", "35", "56"))
# 
# mes_depts %>%
#   mapview::mapview()
# 
# station_geo <- station %>%
#   sf::st_as_sf(coords = c("x_wgs84", "y_wgs84"),
#                crs = 4326)
# 
# 
# station_bzh <- station_geo %>% 
#   aspe::geo_attribuer(regions_metro_geo) %>% 
#   filter(REG == "53")
# 
# 
# palette <- mapviewPalette("mapviewTopoColors")
# 
# mapview::mapview(mes_depts,
#                  zcol = "DEP",
#                  col.regions = palette(4),
#                  alpha.regions = 0.3,
#                  map.types = c("OpenStreetMap",
#                                "CartoDB.Positron",
#                                "CartoDB.DarkMatter",
#                                "Esri.WorldImagery",
#                                "OpenTopoMap")) +
#   mapview::mapview(station_bzh,
#                    alpha = 0.9,
#                    cex = 0.1,
#                    col.regions = 'blue')
# 
# 
# 
# 
# # CREATION D'UN BUFFER DE 2 km :
# 
# station_bzh_buff_2km <- st_buffer(mes_depts, 2000)
# 
# 
# mapview(
#   list(station_bzh_buff_2km, mes_depts),
#   layer.name = c("Bretagne avec un buffer de 2 km", "Bretagne"),
#   col.regions = list("#440154FF", "#FDE725FF")
# ) + mapview (station_bzh, color = "red", lwd =3)
# 
# 
#
# palette <- mapviewPalette("mapviewTopoColors")
# 
# mapview::mapview(mes_depts,
#                  zcol = "DEP",
#                  col.regions = palette(4),
#                  alpha.regions = 0.3,
#                  map.types = c("OpenStreetMap",
#                                "CartoDB.Positron",
#                                "CartoDB.DarkMatter",
#                                "Esri.WorldImagery",
#                                "OpenTopoMap")) +
#   mapview::mapview(station_bzh,
#                    alpha = 0.9,
#                    cex = 0.1,
#                    col.regions = 'blue')






  