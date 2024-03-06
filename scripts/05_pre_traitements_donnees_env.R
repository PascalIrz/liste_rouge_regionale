#_______________________________________________________________________________
################ PRE TRAITEMENTS DES DONNEES ENVIRONNEMENTALES #################
#_______________________________________________________________________________


# Objectif : Ce script propose les pré-traitements des données environnementales
# utilisées pour l'étude. Elle permet notamment d'observer les erreurs de 
# saisies de manière graphique. 



## Chargement des packages ----

#install.packages("khroma")
#install.packages("lemon")
#install.packages("ggthemes")

library(lemon)
library(ggthemes)
library(tidyverse)
library(aspe)
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




########################### DONNEES ENVIRONNEMENTALES #########################


# Je crée un tableau avec mes variables environnement : 

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

env_median_par_pop <- ope_selection_param_env2 %>% 
  group_by(pop_id, parametre) %>% 
    summarise(mediane = median(valeur, na.rm = TRUE)) %>% 
  ungroup()

ope_selection_param_env2 <- ope_selection_param_env2 %>% 
  left_join(env_median_par_pop) %>% 
  mutate(valeur = ifelse(is.na(valeur), mediane, valeur)) %>% 
  select(-mediane)



# ope_selection_param_env3 <- left_join(ope_selection_param_env, 
#                                       ope_selection_param_env2,
#                                       by = "annee")


# Une fois que mes NA seront apparants : je remplace ces NA par les médianes :

# ope_selection_param_env3 %>% 
#   group_by(parametre, pop_id.x) %>% 
#   mutate (valeur = (ifelse (is.na (valeur), 
#                             yes = median (valeur, na.rm = TRUE), 
#                             no = valeur)))




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
  ggplot(mapping = aes(x = annee, y = valeur), scales = "free") +
  geom_bar(stat="identity", fill = "darkcyan") +
  facet_rep_grid(pop_id ~ parametre, 
                 scales= "free", 
                 repeat.tick.labels = TRUE) +
  labs(title = "Diagrammes en baton des variables environnementales", 
       x = "Années", 
       y = "Valeurs") +
  theme_bw()

ope_selection_param_env <- ope_selection_param_env2



save(ope_selection_param_env,
     file = "processed_data/ope_selection_param_env.rda")

