## Chargement des packages et des données ----


#install.packages("khroma")
#install.packages("lemon")
#install.packages("ggthemes")

library(lemon)
library(wesanderson)
library(ggthemes)
library(tidyverse)
library(aspe)
library(ggplot2)
library (khroma)
library(classInt)
source(file = "R/calcul_biomasse.R")


# --- Chargement du fichier de données : 
load(file = "processed_data/selection_pop_ope.rda")

rdata_tables <- misc_nom_dernier_fichier(
  repertoire = "../../../projets/ASPE/raw_data/rdata",
  pattern = "^tables")

load(rdata_tables)

mei_table <- misc_nom_dernier_fichier(
  repertoire = "../../../projets/ASPE/raw_data/rdata",
  pattern = "^mei")

load(mei_table)


# Chargement de la palette de couleur utilisée : 
pal <- wes_palette("AsteroidCity1")



################ Elaboration classe de tailles #####################

## Je complète mon df pour selectionner mes mesures individuelles de poissons d'eau douce de Bretagne (dans mes stations sélectionnées)

esp_selection <- passerelle %>% 
  left_join(y=lot_poissons %>% 
              select(lop_id,
                     esp_id = lop_esp_id,
                     lop_effectif)) %>% 
  left_join (y=  ref_espece %>% 
               select(esp_id,
                      esp_code_alternatif)) %>% 
  left_join (y= mesure_individuelle %>% 
               select (mei_taille,
                       lop_id = mei_lop_id))

esp_selection <- esp_selection %>% 
  select(-sta_id,
         -pop_id,
         -ope_id,
         -pre_id,
         -esp_id,
         -lop_id,
         -lop_effectif)


# Selection des codes alternatifs correspondant aux espèces ciblées (liste rouge regionale) : 

especes_a_garder <- c("ABL","ALF","ANG","BRB","BRO",
                      "BRE","CCO","CHA","CHE","EPI","EPT","FLE",
                      "GAR","GOU","ALA","LPP","LPR","LPM","LOF","MUP",
                      "PER","VAR","ROT","SAT",
                      "TAN","TRF","VAI"
                      )

esp_selection_filtrer <- esp_selection %>%
  filter(esp_code_alternatif %in% especes_a_garder)
  
  


# Graphique global avec les courbes de tailles de toutes les espèces (en fonction des effectifs)

limites_x <- c(0,500) # Mise en place d'une limite pour "zoomer" sur le graphique


graphique <- esp_selection_filtrer %>%  # Mettre juste esp_selection si je veux toutes les espèces
  ggplot(aes (x = mei_taille, color = esp_code_alternatif)) + 
  geom_density(aes(y=..count..)) + 
  labs (x = "Taille (en mm)", y = "Nombre d'individus", title = "Courbes de tailles des espèces en fonction du nombre d'individus") +
  theme_minimal() +
  scale_fill_manual(values= pal) +
  scale_x_continuous(limits = limites_x)

print(graphique)



#### Représentation graphique en facette
mes_do <- sample(unique(esp_selection_filtrer$esp_code_alternatif), 2)

graphique2 <- esp_selection_filtrer %>% 
  filter(esp_code_alternatif%in% mes_do) %>% 
  ggplot(aes(mei_taille)) + 
  geom_density(aes(y=..count..)) + 
  facet_wrap(.~esp_code_alternatif,
             scales = "free") + 
  scale_x_continuous(breaks = seq(0, 3000, by = 20)) + 
  labs (x= "Taille (en mm)") + 
  scale_fill_manual(values= pal)


print(graphique2)






######################### JENKS NATURAL BREAKS CLASSIFICATION #################
# Ca ne marche pas ! 

#library(classInt)

#tailles <- as.numeric(as.character(esp_selection_filtrer$mei_taille))

#donnees_par_espece <- split(esp_selection_filtrer, esp_selection_filtrer$esp_code_alternatif)

#nb_classes <- 3 

#jenks_seuils_par_espece <- list()

#for (esp_code_alternatif in names(donnees_par_espece)) {
#  tailles_espece <- as.numeric(as.character(donnees_par_espece[[esp_code_alternatif]]$tailles))
#  
#  jenks_seuils_espece <- classIntervals (tailles_espece, nb_classes, style = "jenks")$brks
#  
#  jenks_seuils_par_espece[[esp_code_alternatif]] <- jenks_seuils_espece
#}
     

#print (jenks_seuils_par_espece)







################# Pour le cas spécifique de l'anguille ########################


esp_ANG <- esp_selection %>% 
  filter(esp_code_alternatif == "ANG") %>% 
  select(-esp_code_alternatif)


effectifs <- table(esp_ANG)

donnees_effectifs_ANG <- data.frame(taille = as.numeric(names(effectifs)),
                                effectif = as.numeric (effectifs))




# Création d'une représentation graphique de taille par espèce

ggplot(data = donnees_effectifs_ANG, aes(x=taille, y = effectif)) +
  geom_line() + # geom_smooth(method= "loess", se = FALSE, color= "red") +
  labs (x = "Tailles", y = "Effectifs", title = "Distribution des tailles d'Anguilles")




#################### Essai avec des points d'inflexion ###############

deriv <- diff(donnees_effectifs_ANG$effectif) / diff(donnees_effectifs_ANG$taille)
indices_inflexion <- which(diff(sign(deriv)) !=0)

point_inflexion <- donnees_effectifs_ANG[indices_inflexion,]

print(point_inflexion)



















################# Pour le cas spécifique de GAR ########################


esp_GAR <- esp_selection %>% 
  filter(esp_code_alternatif == "GAR") %>% 
  select(-esp_code_alternatif)


effectifs <- table(esp_GAR)

donnees_effectifs_GAR <- data.frame(taille = as.numeric(names(effectifs)),
                                    effectif = as.numeric (effectifs))




# Création d'une représentation graphique de taille par espèce

ggplot(data = donnees_effectifs_GAR, aes(x=taille, y = effectif)) +
  geom_line() + 
  labs (x = "Tailles", y = "Effectifs", title = "Distribution des tailles GAR")








################# Pour le cas spécifique de CHA ########################


esp_CHA <- esp_selection %>% 
  filter(esp_code_alternatif == "CHA") %>% 
  select(-esp_code_alternatif)


effectifs <- table(esp_CHA)

donnees_effectifs_CHA <- data.frame(taille = as.numeric(names(effectifs)),
                                    effectif = as.numeric (effectifs))




# Création d'une représentation graphique de taille par espèce

ggplot(data = donnees_effectifs_CHA, aes(x=taille, y = effectif)) +
  geom_line() +
  labs (x = "Tailles", y = "Effectifs", title = "Distribution des tailles CHA")




################# Pour le cas spécifique de GOU ########################


esp_GOU <- esp_selection %>% 
  filter(esp_code_alternatif == "GOU") %>% 
  select(-esp_code_alternatif)


effectifs <- table(esp_GOU)

donnees_effectifs_GOU <- data.frame(taille = as.numeric(names(effectifs)),
                                    effectif = as.numeric (effectifs))




# Création d'une représentation graphique de taille par espèce

ggplot(data = donnees_effectifs_GOU, aes(x=taille, y = effectif)) +
  geom_line() +
  labs (x = "Tailles", y = "Effectifs", title = "Distribution des tailles GOU")





################# Pour le cas spécifique de LOF ########################


esp_LOF <- esp_selection %>% 
  filter(esp_code_alternatif == "LOF") %>% 
  select(-esp_code_alternatif)


effectifs <- table(esp_LOF)

donnees_effectifs_LOF <- data.frame(taille = as.numeric(names(effectifs)),
                                    effectif = as.numeric (effectifs))




# Création d'une représentation graphique de taille par espèce

ggplot(data = donnees_effectifs_LOF, aes(x=taille, y = effectif)) +
  geom_line() +
  labs (x = "Tailles", y = "Effectifs", title = "Distribution des tailles LOF")




################# Pour le cas spécifique de LOF ########################


esp_IDE <- esp_selection %>% 
  filter(esp_code_alternatif == "IDE") %>% 
  select(-esp_code_alternatif)


effectifs <- table(esp_IDE)

donnees_effectifs_IDE <- data.frame(taille = as.numeric(names(effectifs)),
                                    effectif = as.numeric (effectifs))




# Création d'une représentation graphique de taille par espèce

ggplot(data = donnees_effectifs_IDE, aes(x=taille, y = effectif)) +
  geom_line() +
  labs (x = "Tailles", y = "Effectifs", title = "Distribution des tailles IDE")








################# Pour le cas spécifique de MUP ########################


esp_MUP <- esp_selection %>% 
  filter(esp_code_alternatif == "MUP") %>% 
  select(-esp_code_alternatif)


effectifs <- table(esp_MUP)

donnees_effectifs_MUP <- data.frame(taille = as.numeric(names(effectifs)),
                                    effectif = as.numeric (effectifs))




# Création d'une représentation graphique de taille par espèce

ggplot(data = donnees_effectifs_MUP, aes(x=taille, y = effectif)) +
  geom_line() +
  labs (x = "Tailles", y = "Effectifs", title = "Distribution des tailles MUP")






################# Pour le cas spécifique de OCL ########################


esp_OCL <- esp_selection %>% 
  filter(esp_code_alternatif == "OCL") %>% 
  select(-esp_code_alternatif)


effectifs <- table(esp_OCL)

donnees_effectifs_OCL <- data.frame(taille = as.numeric(names(effectifs)),
                                    effectif = as.numeric (effectifs))




# Création d'une représentation graphique de taille par espèce

ggplot(data = donnees_effectifs_OCL, aes(x=taille, y = effectif)) +
  geom_line() +
  labs (x = "Tailles", y = "Effectifs", title = "Distribution des tailles OCL")



