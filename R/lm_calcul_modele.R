
lm_calcul_modele <- function(data,
                               mon_espece,
                               mon_stade,
                               mon_indicateur) {
  
  filtered_data <- data %>%
    filter(espece == mon_espece, 
           stade == mon_stade, 
           indicateur == mon_indicateur)
  
  model <- lm(valeur ~ annee, data = filtered_data)

  res <- summary(model)$coefficients %>%  # Obtenir les résultats résumés
    as.data.frame() %>%
    rename(p_value = `Pr(>|t|)`) %>%
    mutate(sig = case_when(
      p_value < 0.001 ~ "***",
      p_value < 0.01 ~ "**",
      p_value < 0.05 ~ "*",
      TRUE ~ ""
    )) %>%
    mutate(esp_code_alternatif = mon_espece,
           stade = mon_stade,
           indicateur = mon_indicateur)
  
  return(res)
}
