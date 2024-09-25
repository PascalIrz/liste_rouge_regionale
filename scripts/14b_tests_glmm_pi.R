mon_espece <- "TRF"
mon_stade <- "ind"
nb_annees <- 10

debut <- 2023-nb_annees


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
       #  valeur = log10(valeur)
         ) %>% 
  filter(pop_id != "41893")

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

mod <- lmerTest::lmer(valeur ~ annee + (1|pop_id) + ope_surface_calculee +
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
effectifs_debut_fin <- ope_sp_ind %>% 
  group_by(pop_id) %>% 
  filter(annee == max(annee) | annee == min(annee)) %>% 
  mutate(debut = (annee == min(annee))) %>% 
  group_by(debut) %>% 
  summarise(effectif_moy = mean(valeur),
            effectif_median = median(valeur))


taux_evol_effectif_moy_lrr <- ()


