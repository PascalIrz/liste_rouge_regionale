mon_espece <- "CHA"
mes_stades <- c("juv", "ad", "ind")
mon_indicateur <- "biomasse"
mon_annee_depart <- 1990

# Filtrer les données
df <- reg_indicateur %>% 
  filter(!is.na(valeur),
         annee >= mon_annee_depart,
         esp_code_alternatif == mon_espece,
         indicateur == mon_indicateur,
         stade == mes_stades)

# Calculer les différences annuelles et les taux annuels
df1 <- df %>% 
  group_by(esp_code_alternatif, indicateur, stade) %>% 
  arrange(annee) %>% 
  mutate(diff_annee = annee - lag(annee, default = first(annee)),
         taux_annuel = ifelse(diff_annee > 0, (valeur / lag(valeur, default = first(valeur))) ^ (1 / diff_annee), NA_real_)) %>% 
  filter(!is.na(taux_annuel))

# Calculer la moyenne géométrique des taux annuels
mean_geom <- df1 %>%
  summarize(mean_geom = exp(mean(log(taux_annuel)))) %>%
  pull(mean_geom)

# Créer un dataframe avec les valeurs calculées
result_df <- data.frame(Espece = mon_espece,
                        Indicateur = mon_indicateur,
                        Annee_Depart = mon_annee_depart,
                        Moyenne_Geometrique = mean_geom,
                        Stade = mes_stades)

# Afficher le dataframe résultant
print(result_df)





# Partie faite avec pascal vendredi
mon_espece <- "ANG"
mon_indicateur <- "biomasse"
mon_annee_depart <- 1990

df <- reg_indicateur %>% 
  filter(esp_code_alternatif == mon_espece,
         indicateur == mon_indicateur,
         !annee %in% c(1991, 2000, 2001),
         annee >= mon_annee_depart)


test <- df %>% 
  arrange(annee) %>% 
  ungroup() %>% 
  mutate(diff_annee = annee - dplyr::lag(annee, n = 1),
         taux_annuel = (valeur / dplyr::lag(valeur, n = 1)) ^ (1 / diff_annee)) %>% 
  filter(!is.na(taux_annuel)) %>% 
  pull(taux_annuel)


exp(mean(log(test)))

