glmm_calcul_modele <- function(data,
                               mon_espece) {
  
  filtered_data <- data %>%
    filter(espece == mon_espece)
  
  if (nrow(filtered_data) < 2 || 
      length(unique(filtered_data$pop_id)) < 2 || 
      length(unique(filtered_data$annee)) < 2 || 
      length(unique(filtered_data$pro_libelle)) < 2 ) {
    return(NULL)
  }
  
  # model <- try(glmer(valeur ~  scale(annee) +
  #                      pro_libelle +
  #                    scale(ope_surface_calculee) +
  #                      (scale(annee) | pop_id) +
  #                      scale(julian) +
  #                      scale(I(julian^2)),
  #                    data = filtered_data,
  #                    family = poisson), 
  #                    silent = TRUE)
  
  model <- try(glmer(valeur ~  (annee | pop_id) +
                       annee +
                       pro_libelle +
                       ope_surface_calculee +
                       scale(julian) +
                       scale(I(julian^2)),
                     data = filtered_data,
                     family = poisson),
                     silent = TRUE)

  
  if (inherits(model, "try-error")) {
    return(NULL)
  }
  
  res <- summary(model)$coefficients %>%
    as.data.frame() %>%
    rename(p_value = `Pr(>|z|)`) %>%
    mutate(sig = case_when(
      p_value < 0.001 ~ "***",
      p_value < 0.01 ~ "**",
      p_value < 0.05 ~ "*",
      TRUE ~ ""
    )) %>%
    mutate(esp_code_alternatif = mon_espece)
  
  return(res)
}

