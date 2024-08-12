
ope_densite_surf_glm <- ope_densite_surf %>% 
  filter(stade == "ind")

ope_effectif_absences_glm <- ope_effectif_absences %>% 
  filter(valeur == 0) 

ope_densite_surf_glm_ok <- bind_rows(ope_effectif_absences_glm,
                              ope_densite_surf_glm) %>% 
  mutate(indicateur = "densite_surface")


ope_densite_surf_glm_ok1 <- ope_densite_surf_glm_ok %>%
  mef_ajouter_type_protocole() %>% 
  mef_ajouter_ope_date() %>% 
  select(ope_id,
         stade,
         esp_code_alternatif,
         indicateur,
         valeur, 
         ope_date,
         pro_libelle,
         annee)


ope_densite_surf_glm_ok1 <- ope_densite_surf_glm_ok1 %>% #Ajout des pop_id dans le dataframe
  left_join(y=operation %>% 
              select(ope_id,
                     pop_id= ope_pop_id)) %>% 
  mutate(pop_id = as.factor(pop_id))




ope_densite_surf_glm_ok1 <- ope_densite_surf_glm_ok1 %>%
  mutate(julian = lubridate::yday(ope_date)) %>%
  ungroup()


ope_densite_surf_glm_ok1 <- ope_densite_surf_glm_ok1 %>%
  rename(espece = esp_code_alternatif) %>%
  ungroup() %>%
  select(-ope_id,
         -indicateur,
         -stade)



ope_densite_surf_glm_ok1_ang <- ope_densite_surf_glm_ok1 %>% 
  mutate(pop_id = as.factor(pop_id)) %>% 
  filter(espece == "ANG")


# Modèle GLMM pour les densités d'anguilles
library(lme4)


model_glm_ang <- glmer(valeur ~ (annee | pop_id) + pro_libelle + julian,
                       data = ope_densite_surf_glm_ok1_ang, 
                       family = gaussian)



res_ang_densite <- summary(model_glm_ang)$coefficients %>%
  as.data.frame() 


%>%
  rename(p_value = `Pr(>|z|)`) %>%
  mutate(sig = case_when(
    p_value < 0.001 ~ "***",
    p_value < 0.01 ~ "**",
    p_value < 0.05 ~ "*",
    TRUE ~ ""))



# Modèle GLMM pour les densités de gardon
ope_densite_surf_glm_ok1_gar <- ope_densite_surf_glm_ok1 %>% 
  mutate(pop_id = as.factor(pop_id)) %>% 
  filter(espece == "GAR") %>% 
  select(- ope_date,
         - espece,
         - julian)


model_glm_gar <- glmer(valeur  ~  scale(annee) +
                         (1 | pop_id) +
                         pro_libelle,
                       data = ope_densite_surf_glm_ok1_gar,
                       family = "poisson")

res_gar <- summary(model_glm_gar)$coefficients %>%
  as.data.frame() %>%
  rename(p_value = `Pr(>|z|)`) %>%
  mutate(sig = case_when(
    p_value < 0.001 ~ "***",
    p_value < 0.01 ~ "**",
    p_value < 0.05 ~ "*",
    TRUE ~ ""))
















####### Pour les données d'effectifs avec effets ope_surface_calculee:
# Vu que valeurs d'effectifs j'utilise la loi de Poisson

ope_effectif_absences_glm <- ope_effectif_absences %>% 
  filter(stade == "ind")


ope_effectif_absences_glm <- ope_effectif_absences_glm %>%
  mef_ajouter_type_protocole() %>% 
  mef_ajouter_surf_calc() %>% 
  mef_ajouter_ope_date() %>% 
  select(ope_id,
         stade,
         esp_code_alternatif,
         ope_surface_calculee,
         indicateur,
         valeur, 
         ope_date,
         pro_libelle,
         annee)


ope_effectif_absences_glm <- ope_effectif_absences_glm %>% #Ajout des pop_id dans le dataframe
  left_join(y=operation %>% 
              select(ope_id,
                     pop_id= ope_pop_id)) %>% 
  mutate(pop_id = as.factor(pop_id))




ope_effectif_absences_glm <- ope_effectif_absences_glm %>%
  mutate(julian = lubridate::yday(ope_date)) %>%
  ungroup()


ope_effectif_absences_glm_ok <- ope_effectif_absences_glm %>%
  rename(espece = esp_code_alternatif)
  # ungroup() %>%
  # select(-ope_id,
  #        -indicateur,
  #        -stade)



ope_effectif_absences_glm_ok_ang <- ope_effectif_absences_glm_ok %>% 
  mutate(pop_id = as.factor(pop_id)) %>% 
  filter(espece == "ANG") 
# %>% 
#   select(- ope_date,
#          - espece,
#          - julian)


# Modèle GLMM pour les densités d'anguilles
library(lme4)



model_glm_ang <- glmer(valeur ~ (annee | pop_id) + pro_libelle,
                       data = ope_effectif_absences_glm_ok_ang,
                       family = "poisson")




res_ang <- summary(model_glm_ang)$coefficients %>%
  as.data.frame() %>%
  rename(p_value = `Pr(>|z|)`) %>%
  mutate(sig = case_when(
    p_value < 0.001 ~ "***",
    p_value < 0.01 ~ "**",
    p_value < 0.05 ~ "*",
    TRUE ~ ""))




model <- glmer(valeur ~ (scale (annee)| pop_id) +
                 pro_libelle +
                 scale(ope_surface_calculee) +
                     scale(julian) +
                     scale(I(julian^2)),
                   data = ope_effectif_glm,
                   family = poisson)

residuals <- resid(model1, type = "pearson")
# Plot des résidus
plot(residuals)
qqnorm(residuals)
qqline(residuals, col = "red")
hist(residuals, breaks = 30, main = "Histogram of Residuals", xlab = "Residuals")

plot(fitted(model), residuals)
abline(h = 0, col = "red")
acf(residuals)
