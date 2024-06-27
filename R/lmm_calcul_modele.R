lmm_calcul_modele <- function(data,
                              mon_espece,
                              mon_stade,
                              mon_indicateur) {
  
  filtered_data <- data %>%
    filter(espece == mon_espece, 
           stade == mon_stade, 
           indicateur == mon_indicateur)
  
  if (nrow(filtered_data) < 2 || 
      length(unique(filtered_data$pop_id)) < 2 || 
      length(unique(filtered_data$annee)) < 2 || 
      length(unique(filtered_data$pro_libelle)) < 2 || 
      length(unique(filtered_data$obj_libelle)) < 2) {
    return(NULL)
  }
  
 # model <- try(lmer(valeur ~ (1 | pop_id) + annee + pro_libelle + obj_libelle, data = filtered_data), silent = TRUE)
  model <- try(lmer(valeur ~ (1 | pop_id) + annee + pro_libelle, data = filtered_data), silent = TRUE)
  
  
  if (inherits(model, "try-error")) {
    return(NULL)
  }
  
  res <- summary(model)$coefficients %>%
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
