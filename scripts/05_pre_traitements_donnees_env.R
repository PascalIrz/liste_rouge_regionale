#_______________________________________________________________________________
################ PRE TRAITEMENTS DES DONNEES ENVIRONNEMENTALES #################
#_______________________________________________________________________________


# Objectif : Ce script propose les pré-traitements des données environnementales
# utilisées pour l'étude. Elle permet notamment d'observer les erreurs de 
# saisies de manière graphique. 



## Chargement des packages ----

library(tidyverse)
library(aspe)
library(ggplot2)
library(zoo)
library(lemon)


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


########################### DONNEES ENVIRONNEMENTALES #########################

# Je créer un tableau avec les variables environnementales relatives à mes 
# opérations contenues dans la passerelle (cf OO_selection_pop_ope) : 

ope_selection_param_env <- passerelle %>% 
  select(-lop_id,
         -pre_id) %>% 
  mef_ajouter_ope_env() %>% 
  mef_ajouter_ope_date() %>% 
  select(-ope_date,
         -sta_id,
         -distance_mer,
         -obj_libelle,
         -obj_id)%>%
  distinct() %>% 
  pivot_longer(altitude:temp_janvier,
               names_to = "parametre",
               values_to= "valeur") %>% 
  distinct()
  

# GESTION DES NA DANS LE JEU DE DONNEES ENV ---- 

# Je fais apparaître mes NA dans un nouveau jeu de données : 
rows_with_missing <- which(apply(is.na(ope_selection_param_env), 1, any))
print(ope_selection_param_env[rows_with_missing, ])

# Une fois que mes NA sont apparants : je remplace ces NA par les médianes :
# Trier les données par paramètre et par année et Remplacer les valeurs 
# manquantes par les médianes des autres données relatives aux points de prélèvements
#ope_selection_param_env <- ope_selection_param_env %>% 
 # arrange(parametre, annee)%>%
  #group_by(parametre,pop_id) %>%
  #mutate(valeur = ifelse(is.na(valeur), zoo::na.aggregate(valeur, median), valeur))


# Représentation de mes variables environnements (avec facet_wrap) : 
mes_id <- sample(unique(ope_selection_param_env$pop_id), 2)
ope_selection_param_env %>% 
  filter(pop_id%in% mes_id) %>%
  ggplot(aes(x=annee, y=valeur)) +
  geom_bar(stat="identity", fill = "darkcyan") +
  facet_wrap(vars(pop_id, parametre),
             ncol = 4,
             scales="free_y") +
  labs(title = "Diagrammes en baton des variables environnementales", 
       x = "Années", 
       y = "Valeurs") +
  theme(
    axis.title.y = element_text (color = "black"),
    axis.text.y.left = element_text (color = "black"),
    strip.text = element_text(size = 8),
    strip.background.x =element_rect(fill="lightgrey"))



# Représentation de mes variables environnements (avec facet_grid) : mais problème d'échelle
mes_id <- sample(unique(ope_selection_param_env$pop_id), 2) 
ope_selection_param_env %>% 
  filter(pop_id%in% mes_id) %>% 
  ggplot(mapping = aes(x = annee, y = valeur)) +
  geom_bar(stat="identity", fill = "darkcyan") +
  facet_rep_grid(pop_id ~ parametre, 
                 scales = "free",
                 repeat.tick.labels = TRUE) +
  labs(title = "Diagrammes en baton des variables environnementales", 
       x = "Années", 
       y = "Valeurs") +
  theme_bw()



# SAUVEGARDE ----
save(ope_selection_param_env,
     file = "processed_data/pre_traitements_donnees_env.rda")
