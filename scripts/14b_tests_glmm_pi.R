load(file = "processed_data/14_ope_effectif_glm.rda")

mon_espece <- "TRF"
mon_stade <- "ind"
nb_annees <- 10

debut <- 2023 - nb_annees


library(tidyverse)
library(lme4)

ope_sp_ind <- ope_effectif_glm %>% 
  filter(espece == mon_espece,
         stade == mon_stade,
         indicateur == "effectif_total",
         annee >= debut) %>% 
 # select(-obj_libelle) %>% 
  distinct() %>% 
  mutate(pop_id = as.character(pop_id),
         annee_cr = scale(annee),
         ope_surface_calculee_cr = scale(ope_surface_calculee)
         )

# Sur le Guyoult à mont-Dol, l'anguille avait disparu à cause de pollution dans les années 90
# Ca ne concerne pas les autres espèces car la durée de l'évaluation LRR fait que cette période n'est pas couverte
if(mon_espece == "ANG") {ope_sp_ind <- ope_sp_ind %>% filter(pop_id != "41893")}
  

ggplot(data = ope_sp_ind,
       aes(x = valeur)) +
  geom_density()

ggplot(data = ope_sp_ind,
       aes(x = valeur)) +
  geom_density() +
  scale_x_log10()


ggplot(data = ope_sp_ind,
       aes(x = annee,
           y = valeur)) +
  geom_point() +
  geom_smooth()

ggplot(data = ope_sp_ind,
       aes(x = annee,
           y = valeur)) +
  geom_point() +
  geom_smooth() +
  scale_y_log10()

mod <- lmerTest::lmer(valeur ~ annee_cr + (1|pop_id) + ope_surface_calculee_cr +
                       pro_libelle #+
                        #poly(julian, 2)
                        ,
  data = ope_sp_ind)

summary(mod)
anova(mod)
plot(mod)
plot(mod, type=c("p","smooth"), col.line=1)
plot(mod,
     sqrt(abs(resid(.)))~fitted(.),
     type=c("p","smooth"), col.line=1)
lattice::qqmath(mod)
plot(mod, rstudent(.) ~ hatvalues(.))

res <- summary(mod)$coefficients %>%
  as.data.frame() %>%
  rename(p_value = `Pr(>|t|)`) %>%
  mutate(sig = case_when(
    p_value < 0.001 ~ "***",
    p_value < 0.01 ~ "**",
    p_value < 0.05 ~ "*",
    TRUE ~ ""
  ))

res

predicted <- predict(mod) %>% 
  as.data.frame() %>% 
  set_names("predicted")


ope_sp_ind <- ope_sp_ind %>% 
  bind_cols(predicted)

ggplot(data = ope_sp_ind,
       aes(x = valeur,
           y = predicted)) +
  geom_point() +
 # geom_smooth(method = "lm") +
  geom_abline(linetype = "dashed",
              col = "red",
              linewidth = 1) +
  labs(x = "Effectif observé",
       y = "Effectif préduit",
       title = paste0(mon_espece, " (", mon_stade, "), période ", debut, "-2023"))

#######################################

# On veut calculer les valeurs prédites sur l'ensemble des stations en début et fin de période
# On le fait slt sur les sites où l'espèce a été contactée mini sur 50% des pêches

# calcul du taux d'occurrence sur chaque station
n_ope_par_pop <- ope_effectif_glm %>% 
  filter(annee >= debut) %>% 
  group_by(pop_id) %>% 
  summarise(n_ope = n_distinct(ope_id))

pc_contact <- ope_sp_ind %>% 
  filter(valeur > 0) %>% 
  group_by(pop_id) %>% 
  summarise(n_pres = n_distinct(ope_id))  %>% 
  left_join(n_ope_par_pop) %>% 
  mutate(pc_pres = n_pres / n_ope)

# identifiants des points avec pourcentage d'occurrence au-dessus de 50%
pop_id_select <- pc_contact %>% 
  filter(pc_pres >= 0.5) %>% 
  pull(pop_id)

# calcul des effectifs prédits en première et en dernière année
effectifs_debut_fin <- ope_sp_ind %>% 
  filter(pop_id %in% pop_id_select) %>% 
  group_by(pop_id) %>% 
  filter(annee == max(annee) | annee == min(annee)) %>% 
  mutate(periode = ifelse(annee == min(annee),
                         "Début",
                         "Fin")) %>% 
  ungroup() %>% 
  mutate(annee_cr2 = ifelse(periode == "Début", min(annee_cr), max(annee_cr))) 

# vérification années de début et de fin pour points échantillonnés une année sur deux
effectifs_debut_fin %>% filter(annee_cr2 != annee_cr) %>% View()

# modification du tableau de données afin de prédire, quel que soit le point, l'effectif pour la première et 
# la dernière des années de l'étude, yc pour les points qui n'ont pas été pêchés ces années-là (pb de la prospection bi-annuelle)
effectifs_debut_fin <- effectifs_debut_fin %>% 
  mutate(annee_cr = annee_cr2) %>% 
  select(-annee_cr2)
  
predicted2 <- predict(mod, newdata = effectifs_debut_fin)

effectifs_debut_fin <- effectifs_debut_fin %>% 
  select(-predicted) %>% 
  cbind(predicted = predicted2)

stats_effectifs_debut_fin <- effectifs_debut_fin %>% 
  group_by(periode) %>% 
  summarise(effectif_moy = mean(predicted),
            effectif_median = median(predicted))


# calcul des taux d'évolution
# sur l'effectif moyen
taux_evol_effectif_moy_lrr <- (stats_effectifs_debut_fin[2, 2] - 
                                 stats_effectifs_debut_fin[1, 2]) /
                                 stats_effectifs_debut_fin[1, 2]

# sur l'effectif médian
taux_evol_effectif_med_lrr <- (stats_effectifs_debut_fin[2, 3] -
                                 stats_effectifs_debut_fin[1, 3]) /
                                 stats_effectifs_debut_fin[1, 3]

