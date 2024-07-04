
############################
# PRE TRAITEMENT 
mes_especes <- c("ANG","BRO", "CHA", "CHE","PER", "GAR", "GOU", "LOF", "LPP","SAT", "TRF", "VAI", "VAR")

ope_effectif_esp <- ope_effectif %>% 
  filter(esp_code_alternatif %in% mes_especes,
         stade == "ind")  



mon_espece <- "VAI"
ma_premiere_annee <- 2011

data_ang <- ope_effectif_esp %>% 
  filter(esp_code_alternatif == mon_espece)

data_ang <- data_ang %>%
  mef_ajouter_type_protocole() %>% 
  mef_ajouter_surf_calc() %>% 
  mef_ajouter_ope_date() %>% 
  select(ope_id,
         stade,
         ope_surface_calculee,
         esp_code_alternatif,
         indicateur,
         pro_libelle,
         annee,
         ope_date,
         valeur)

data_ang <- data_ang %>% 
  left_join(y=operation %>% 
              select(ope_id,
                     pop_id= ope_pop_id)) %>% 
  mutate(pop_id = as.factor(pop_id))

data_ang_filter <- data_ang %>% 
  filter(annee  >= ma_premiere_annee) %>% 
  mutate(julian = lubridate::yday(ope_date)) %>%
  ungroup() 
# %>% 
#   filter(pop_id != "41893")

data_ang_filter$X <- 1:nrow(data_ang_filter)

###############################
## APPLICATION MODELE


mod <- glmer(valeur ~ scale(annee) + scale(ope_surface_calculee) + (1| pop_id),
               family = poisson,
               data = data_ang_filter)





### MODELE 1
mod_1 <- glmer(valeur ~ scale(annee) +
               pro_libelle +
               scale(ope_surface_calculee) +
               (scale(annee) | pop_id) +
               scale(julian) +
               scale(I(julian^2)),
             family = poisson,
             data = data_ang_filter)
summary(mod_1)



#### MODELE 2
mod_2 <- glmer(valeur ~ scale(annee) +
                 pro_libelle +
                 scale(ope_surface_calculee) +
                 (1|pop_id) +
                 scale(julian) +
                 scale(I(julian^2)),
               family = poisson,
               data = data_ang_filter)
summary(mod_2)


#### MODELE 3
mod_3 <- glmer(valeur ~
                scale(annee) +
                pro_libelle +
                scale(ope_surface_calculee) +
                (1| pop_id) +
                scale(julian) +
                scale(I(julian^2)) +
                (1|X),
              family = poisson, 
              data = data_ang_filter,
              control = glmerControl(optimizer = "bobyqa"))
summary(mod_3)

#### MODELE 4
mod_4 <- glmer(valeur ~
                 scale(annee) +
                 pro_libelle +
                 scale(ope_surface_calculee) +
                 (1| pop_id) +
                 (1|annee) +
                 scale(julian) +
                 scale(I(julian^2)),
               family = poisson, 
               data = data_ang_filter)
summary(mod_4)



#### MODELE 5
mod_5 <- glmer(valeur ~
                 scale(annee) +
                 pro_libelle +
                 scale(ope_surface_calculee) +
                 (scale(annee) | pop_id) +
                 scale(julian) +
                 scale(I(julian^2)) +
                 (1|X),
               family = poisson, 
               data = data_ang_filter,
               control = glmerControl(optimizer = "bobyqa"))
summary(mod_5)


#### MODELE 6
mod_6 <- glmer(valeur ~ (scale(annee) +
                 pro_libelle + (1| pop_id)) *
                 scale(ope_surface_calculee) 
                 # scale(julian) +
                 # scale(I(julian^2)) +
                 ,
               family = poisson, 
               data = data_ang_filter)
summary(mod_6)



#######################

ranef(mod_1)
ranef(mod_2)
ranef(mod_3)
ranef(mod_4)
ranef(mod_5)

##########################

ggplot(data_ang_filter, 
       aes(x = annee,
           y = valeur, 
           color = pop_id)) + 
  geom_point() +
  geom_line(aes(y = fitted(mod_6)))



#############################
## EVALUATION

chi2_mod_1 <- sum(residuals(mod_1, type ="pearson")^2) ## 4415
chi2_mod_2 <- sum(residuals(mod_2, type ="pearson")^2) ## 6489
chi2_mod_3 <- sum(residuals(mod_3, type ="pearson")^2) ## Le plus bas : 198
chi2_mod_4 <- sum(residuals(mod_4, type ="pearson")^2) ## 5405
chi2_mod_5 <- sum(residuals(mod_5, type ="pearson")^2) ## 224


1- pchisq(chi2_mod_1, df = df.residual(mod_1)) # 0
1- pchisq(chi2_mod_2, df = df.residual(mod_2)) # 0
1- pchisq(chi2_mod_3, df = df.residual(mod_3)) # 1
1- pchisq(chi2_mod_4, df = df.residual(mod_4)) # 0
1- pchisq(chi2_mod_5, df = df.residual(mod_5)) # 1

chi2_mod_1 / df.residual(mod_1) # 5.2
chi2_mod_2 / df.residual(mod_2) # 7.7
chi2_mod_3 / df.residual(mod_3) # 0.23
chi2_mod_4 / df.residual(mod_4) # 6.4
chi2_mod_5 / df.residual(mod_5) # 0.26


library(DHARMa)

resid_sim_mod_1 <- simulateResiduals(mod_1)
resid_sim_mod_2 <- simulateResiduals(mod_2)
resid_sim_mod_3 <- simulateResiduals(mod_3)
resid_sim_mod_4 <- simulateResiduals(mod_4)
resid_sim_mod_5 <- simulateResiduals(mod_5)
resid_sim_mod_6 <- simulateResiduals(mod_6)

plot(resid_sim_mod_1)
plot(resid_sim_mod_2)
plot(resid_sim_mod_3)
plot(resid_sim_mod_4)
plot(resid_sim_mod_5)
plot(resid_sim_mod_6)

###########################
## Distribution des effets aléatoires

re_mod_1 <- ranef(mod_1)$pop_id
re_mod_2 <- ranef(mod_2)$pop_id
re_mod_3 <- ranef(mod_3)$pop_id
re_mod_4 <- ranef(mod_4)$pop_id
re_mod_5 <- ranef(mod_5)$pop_id

qqnorm(re_mod_1$`(Intercept)`)
qqnorm(re_mod_2$`(Intercept)`)
qqnorm(re_mod_3$`(Intercept)`)
qqnorm(re_mod_4$`(Intercept)`)
qqnorm(re_mod_5$`(Intercept)`)

qqline(re_mod_1$`(Intercept)`)
qqline(re_mod_2$`(Intercept)`)
qqline(re_mod_3$`(Intercept)`)
qqline(re_mod_4$`(Intercept)`)
qqline(re_mod_5$`(Intercept)`)



##################
## COEF DE DETERMINATION

library(MuMIn)
r.squaredGLMM(mod_1)
r.squaredGLMM(mod_2)
r.squaredGLMM(mod_3)
r.squaredGLMM(mod_4)
r.squaredGLMM(mod_5)


#############################
## Comparaison des modèles

library(AICcmodavg)

aictab(list(mod_1,
            mod_2,
            mod_3, 
            mod_4,
            mod_5,
            mod_6))



plot(simulateResiduals(mod_6
                       ))

###############################################################################


mon_espece <- "ANG"
ma_premiere_annee <- 1990

data_ang <- ope_effectif_esp %>% 
  filter(esp_code_alternatif == mon_espece)

data_ang <- data_ang %>%
  mef_ajouter_type_protocole() %>% 
  mef_ajouter_surf_calc() %>% 
  mef_ajouter_ope_date() %>% 
  select(ope_id,
         stade,
         ope_surface_calculee,
         esp_code_alternatif,
         indicateur,
         pro_libelle,
         annee,
         ope_date,
         valeur)

data_ang <- data_ang %>% 
  left_join(y=operation %>% 
              select(ope_id,
                     pop_id= ope_pop_id)) %>% 
  mutate(pop_id = as.factor(pop_id))

data_ang_filter <- data_ang %>% 
  filter(annee  >= ma_premiere_annee) %>% 
  mutate(julian = lubridate::yday(ope_date)) %>%
  ungroup() %>% 
  filter(pop_id != "41893")

data_ang_filter$X <- 1:nrow(data_ang_filter)

################################################
d <- ggplot(data = data_ang_filter,
            aes(x = annee, 
                y = valeur, 
                col = pop_id)) +
  geom_point() +
  geom_smooth(method = "lm",
              se = FALSE) +
  scale_y_log10()


plotly :: ggplotly (d)

df <- data_ang %>% 
  mutate(code_proto = as.factor(pro_libelle),
         code_proto = as.integer(code_proto),
         code_serie = paste(pop_id, code_proto, sep = "_"))

d1 <- ggplot(data = df %>% filter(pop_id %in% c("42437", "41893", "42207", "47613", "47659")),
             aes(x = annee, 
                 y = valeur, 
                 col = pop_id,
                 group = code_serie)) +
  geom_point() +
  geom_smooth(method = "lm",
              se = FALSE) +
  scale_y_log10()


plotly :: ggplotly (d1)

df %>% 
  ungroup() %>% 
  select(pro_libelle, code_proto) %>% 
  unique()

###################################
mod2 <- glmer(valeur ~
                scale(annee) +
                pro_libelle +
                scale(ope_surface_calculee) +
                (1| pop_id) +
                scale(julian) +
                scale(I(julian^2)) +
                (1|X),
              
              family = poisson, 
              data = data_ang_filter,# %>% filter(pop_id != "45884")
              control = glmerControl(optimizer = "bobyqa"))

summary(mod2)




###################################

mod101 <- glmer(valeur ~
                  scale(annee) +
                  pro_libelle +
                  scale(ope_surface_calculee) +
                  (scale(annee) | pop_id) +
                  scale(julian) +
                  scale(I(julian^2)),
                #(1|X),
                family = poisson, 
                data = data_ang_filter)# %>% filter(pop_id != "45884")
#control = glmerControl(optimizer = "bobyqa"))

summary(mod101)





ggplot(data_ang_filter, 
       aes(x = annee,
           y = valeur, 
           color = pop_id)) + 
  geom_point() +
  geom_line(aes(y = fitted(mod101)))




data_ang_filter$predicted_valeur <- predict(mod_6, 
                                            newdata = data_ang_filter, 
                                            type = "response", 
                                            re.form = NA)





graph <- ggplot(data = data_ang_filter,
                aes(x = valeur,
                    y = predicted_valeur,
                    col = pop_id))  +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  # scale_y_log10() +
  # scale_x_log10() +
  geom_abline() +
  scale_x_continuous(limits = c(NA, NA)) +
  scale_y_continuous(limits = c(NA, NA))


plotly:: ggplotly(graph)



