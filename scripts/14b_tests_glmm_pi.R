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


