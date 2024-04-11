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
mon_espece <- "ABL"
mon_indicateur <- "densite_surf"
mon_annee_depart <- 1990

df <- reg_indicateur %>% 
  filter(esp_code_alternatif == mon_espece,
         indicateur == mon_indicateur,
         # !annee %in% c(1991, 2000, 2001),
         annee >= mon_annee_depart)


test <- df %>% 
  arrange(annee) %>% 
  ungroup() %>% 
  mutate(diff_annee = annee - dplyr::lag(annee, n = 1),
         taux_annuel = (valeur / dplyr::lag(valeur, n = 1)) ^ (1 / diff_annee)) %>% 
  filter(!is.na(taux_annuel)) %>% 
  pull(taux_annuel)


exp(mean(log(test)))



graph_taux_evol <- taux_evol_2005 %>%
  ggplot(aes(x = reorder(stade, mean_geom), y = mean_geom, color = stade)) +
  geom_point(shape = 19) +
  geom_line(aes(group = stade)) +
  facet_grid(~ esp_code_alternatif ~ indicateur, scales = "free") +
  labs(title = "Taux évolution", x = "Stade", y = "Moyenne Géométrique") +
  theme(strip.background = element_rect(color = "black", fill = "#faffff"),
        panel.grid.major = element_line(color = "#ffffff", size = 0.1),
        panel.grid.minor = element_line(color = "#ffffff"),
        panel.background = element_rect(fill = "#faf0e0"),
        legend.position = "bottom") + 
  scale_color_manual(values = c("juv" = "#92D080", "ad" = "#A97B30", "ind" = "black"),
                     name = "Stade", labels = c("Adulte", "Indeterminé", "Juvénile"))

print(graph_taux_evol)



combinaison <- expand.grid(esp_code_alternatif = unique(ope_indicateur$esp_code_alternatif),
                           ope_id = unique(ope_indicateur$ope_id),
                           stade = unique (ope_indicateur$stade))

ope_indicateur_2 <- combinaison %>% 
  left_join(ope_indicateur %>%
              filter (indicateur == "densite_surface")) %>%
  replace_na(list(valeur = 0 , y = "unknown")) %>% 
  group_by(ope_id,
           esp_code_alternatif) %>% 
  mutate (test = (valeur[stade == "ind"] == (valeur[stade == "ad"] + valeur[stade == "juv"])))

