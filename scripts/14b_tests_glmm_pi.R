mon_espece <- "ANG"
mon_stade <- "ad"


library(tidyverse)
library(lme4)

ope_ang_ind <- ope_indicateur_lmm %>% 
  filter(espece == mon_espece,
         stade == mon_stade,
         indicateur == "densite_surface") %>% 
  select(-obj_libelle) %>% 
  distinct() %>% 
  mutate(pop_id = as.character(pop_id)#,
       #  valeur = log10(valeur)
         ) %>% 
  filter(pop_id != "41893")

ggplot(data = ope_ang_ind,
       aes(x = valeur)) +
  geom_density()

ggplot(data = ope_ang_ind,
       aes(x = valeur)) +
  geom_density() +
  scale_x_log10()


ggplot(data = ope_ang_ind,
       aes(x = annee,
           y = valeur)) +
  geom_point() +
  geom_smooth()

ggplot(data = ope_ang_ind,
       aes(x = annee,
           y = valeur)) +
  geom_point() +
  geom_smooth() +
  scale_y_log10()

mod <- lmerTest::lmer(valeur ~ annee + (1|pop_id) + pro_libelle,
  data = ope_ang_ind)

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


ope_ang_ind <- ope_ang_ind %>% 
  bind_cols(predicted)

ggplot(data = ope_ang_ind,
       aes(x = valeur,
           y = predicted)) +
  geom_point() +
  geom_smooth(method = "lm") +
  geom_abline()


ope_ang_ind <- ope_ang_ind %>% 
  mutate(valeur = 10^valeur,
         predicted = 10^predicted)


ggplot(data = ope_ang_ind,
       aes(x = valeur,
           y = predicted)) +
  geom_point() +
  geom_smooth() +
  geom_abline(slope = 1) 

#######################################
ope_ang_ind <- ope_indicateur_lmm %>% 
  filter(espece == "ANG",
         stade == "ind",
         indicateur == "densite_surface") %>% 
  select(-obj_libelle) %>% 
  distinct() %>% 
  mutate(pop_id = as.character(pop_id))

mod1 <- glmer(log10(valeur) ~ annee + (1|pop_id) + pro_libelle,
                    family = gaussian,
                    data = ope_ang_ind)

res1 <- summary(mod1)

res1

anova(mod1)
res1$coefficients



  ###################################################

# Modèles linéaires généralisés




library(lme4)
library(popdynmodel)
library(aspe2)

load(file = "processed_data/assemblage_tab_par_ope.rda")

rdata_tables <- misc_nom_dernier_fichier(
  repertoire = "../../../projets/ASPE/raw_data/rdata",
  pattern = "^tables")
load(rdata_tables)

mei_table <- misc_nom_dernier_fichier(
  repertoire = "../../../projets/ASPE/raw_data/rdata",
  pattern = "^mei")
load(mei_table)










ope_effectif <- ope_effectif %>% 
  filter(esp_code_alternatif %in% c("ANG", "CHE", "LPP", "BRO", "LOF", "VAR", "GAR", "VAI", "PER", "SAT", "GOU", "TRF", "CHA"))

ope_effectif_glm <- ope_effectif %>%
  filter(stade =="ind") %>% 
  mef_ajouter_type_protocole() %>% 
  mef_ajouter_surf_calc() %>% 
  mef_ajouter_ope_date() %>% 
  select(ope_id,
         ope_surface_calculee,
         esp_code_alternatif,
         valeur, 
         pro_libelle,
         annee)


ope_effectif_glm <- ope_effectif_glm %>% 
  left_join(y=operation %>% 
              select(ope_id,
                     pop_id= ope_pop_id))

ope_proto <- ope_effectif_glm %>% 
  select(-esp_code_alternatif, -valeur) %>% 
  distinct()

# Création d'un dataframe avec toutes les combinaisons possibles d'années, indicateurs, stades et esp_code_alternatif
combinaisons_ope_glm <- expand.grid(esp_code_alternatif = unique(ope_effectif_glm$esp_code_alternatif),
                                    ope_id = unique(ope_effectif_glm$ope_id))

ope_glm_complet <- ope_proto %>% 
  left_join(y = combinaisons_ope_glm) %>% 
  left_join(y = ope_effectif_glm) %>% 
  mutate(valeur = ifelse(is.na(valeur), 0, valeur))


ope_glm_complet_ang <- ope_glm_complet %>%
  filter(esp_code_alternatif == "SAT") %>% 
  mutate(pop_id = as.factor(pop_id))

mod <- glmer(valeur ~
               scale(annee)  +
               scale(ope_surface_calculee) +
               (1| pop_id) +
               pro_libelle
             , 
             family = poisson(link = "log"), 
             data = ope_glm_complet_ang)

summary(mod)

predict(mod)

ranef(mod)
coef(mod)

ggplot(ope_glm_complet_ang, aes(x = annee, y = valeur, color = pop_id)) +
  geom_point() +
  geom_line(aes(y = fitted(mod)))







##### Evaluation et comparaison des modèles
## Distribution des résidus 

chi2 <- sum(residuals(mod, type = "pearson")^2)
chi2

1-pchisq(chi2, df = df.residual(mod))


chi2 / df.residual(mod)



library(DHARMa)
resid_sim <- simulateResiduals(mod)
plot(resid_sim)



re <- ranef(mod)$pop_id

qqnorm(re$`(Intercept)`)
qqline(re$`(Intercept)`)



library(jtools)
library(effects)
summ(mod, exp = T)

ope_glm_complet_ang <- ope_glm_complet_ang %>% 
  cbind(residus = residuals(mod)) %>% 
  cbind(valeur_predite = predict(mod))

ggplot(data = ope_glm_complet_ang,
       aes(x = residus)) +
  # geom_histogram() +
  geom_density() +
  geom_vline(xintercept = mean(ope_glm_complet_ang$residus),
             col = "red",
             linetype = "dotted")


ggplot(data = ope_glm_complet_ang,
       aes(x = valeur,
           y = exp(valeur_predite))) +
  geom_point() +
  scale_x_log10() +
  scale_y_log10() +
  geom_smooth(method = "lm") +
  abline(col = "red", a = 1, b = 0)



plot(allEffects(mod))



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


mod <- mod %>% 
  bind_cols(predicted)

ggplot(data = ope_ang_ind,
       aes(x = valeur,
           y = predicted)) +
  geom_point() +
  geom_smooth(method = "lm") +
  geom_abline()


ope_ang_ind <- ope_ang_ind %>% 
  mutate(valeur = 10^valeur,
         predicted = 10^predicted)


ggplot(data = ope_ang_ind,
       aes(x = valeur,
           y = predicted)) +
  geom_point() +
  geom_smooth() +
  geom_abline(slope = 1) 
