################################################################
###################  TUTO ASPE - ¨Prise en main ###############
###############################################################

library(aspe)
library(tidyverse)

# retrieve most recent data file from repo 

rdata_tables <- misc_nom_dernier_fichier(
  repertoire = "../../../../projets/ASPE/raw_data/rdata",
  pattern = "^tables")

# load it
load(rdata_tables)

# PARAMETRES
# Le nombre minimum danneee pour mon traitrement de donnee
n_mini_annee <- 9 

passerelle <- mef_creer_passerelle()

names(passerelle)


passerelle <- passerelle %>% 
  mef_ajouter_dept() %>% 
  filter(dept %in% c(22, 29, 35, 56))


passerelle <- passerelle %>%
  mef_ajouter_objectif() %>% 
  filter(obj_libelle %in% c("RCS – Réseau de Contrôle de Surveillance",
                            "RRP – Réseau de Référence Pérenne",
                            "RHP – Réseau Hydrobiologique Piscicole"))

ipr <- passerelle %>% 
  mef_ajouter_ipr() %>% 
  mef_ajouter_ope_date() %>% 
  filter(ope_date > lubridate::dmy("01/01/2005")) %>% 
  mef_ajouter_libelle() %>% 
  droplevels()

ipr %>% head() %>% DT::datatable()


#############################################################
# Réalisation des Opération graphiques : 


# Création des opérations géographiques 
library(aspe)
library(tidyverse)
library(mapview)


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



# remotes::install_github("MaelTheuliere/COGiter")

library(COGiter)

ggplot(regions_metro_geo) +
  geom_sf(aes(fill = REG)) +
  geom_sf(data = departements_metro_geo,
          alpha = 0)

sf::st_crs(regions_metro_geo)



# Maintien que des départements bretons
mes_depts <- departements_metro_geo %>%
  filter(DEP %in% c("22", "29", "35", "56"))

# attribution et filtre
station_bzh <- station_geo %>% 
  aspe::geo_attribuer(regions_metro_geo) %>% 
  filter(REG == "53")


############################################################################

library(sf)


# Ici je souhaite visualiser mes départements bretons et leurs stations

station_bzh %>%
  mapview::mapview()

# Je souhaite maintennat les organiser en bassin versants 
bassin_simp %>%
  st_as_sf() %>% 
  mapview::mapview()

class(bassin_simp)


# ------------------------------------------------

# Reduction des données recueillies sur le secteur d'étude Bretagne (et opération de pêche)

mes_ope <- mef_creer_passerelle() %>%
  select(sta_id:ope_id) %>%
  distinct() %>%
  mef_ajouter_type_protocole() %>%
  filter(str_detect(pattern = "Pêche",
                    pro_libelle)) %>%
  mef_ajouter_dept() %>%
  filter(dept %in% c("22", "29", "35", "56"))



# Compter le nombre de points par stations après avoir trié le type d'opération de pêches souhaité

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

# Dans sta_plusieurs_pop on remarque 3 stations qui possèdent 2 points de prélèvements.
# Nous souhaitons vérifier qu'il s'agit de points regroupables pour allonger au maximum nos séries chronologiques. 
# J'attend le retour de Vincent


#--------------------------------------------------------------------------------------------------------

# Maintenant je cherche à savoir par station, combien il y a eu d'années de données : 

# Je sais si c'est très utile mais je recreer une nouvelle passerelle :
mes_nvl_ope <- mef_creer_passerelle() %>%
  select(sta_id:ope_id) %>%
  distinct() %>%
  mef_ajouter_type_protocole() %>%
  filter(str_detect(pattern = "Pêche",
                    pro_libelle)) %>%
  mef_ajouter_dept() %>%
  filter(dept %in% c("22", "29", "35", "56"))


# je creer un tableau qui me résume le nombre d'années de suivis  :
annee_de_donnee <- mes_nvl_ope %>%
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


# Représentation graphique des années de prélèvements et des types de pêches correspondant
# Indication sur les trou dans les séries d'inventaire
annee_de_donnee %>% 
  filter(pop_id %in% mes_pop_id) %>% 
  ggplot(aes(x = as.character(pop_id),
           y = annee, 
           col= pro_libelle)) + 
  geom_point() + coord_flip()


annee_de_donnee %>% 
  filter(pop_id %in% mes_pop_id) %>% 
  ggplot(aes(x = as.character(pop_id),
             y = annee, 
             fill= pro_libelle)) + 
  geom_tile() + coord_flip()



# On cherche maitenant à définir un nouveau paramètre : le nombre d'années de trou maximum consécutif
# sur une série pour une station


test <- annee_de_donnee %>% 
  group_by (pop_id) %>% 
  arrange (annee, .by_group = TRUE) %>% 
  mutate(annee_precedente = lag(annee))



