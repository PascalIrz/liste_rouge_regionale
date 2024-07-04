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
  
  model <- try(glmer(valeur ~  scale(annee) + 
                       scale(ope_surface_calculee) + 
                       (1 | pop_id) + 
                       pro_libelle, 
                     data = filtered_data, 
                     family = poisson(link = "log")), 
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

















# glmm_calcul_modele <- function(data,
#                               mon_espece,
#                               mon_stade,
#                               mon_indicateur,
#                               ma_surface,
#                               mon_protocole) {
#   
#   filtered_data <- data %>%
#     filter(espece == mon_espece, 
#            stade == mon_stade, 
#            indicateur == mon_indicateur,
#            surface == ma_surface,
#            protocole == mon_protocole)
#   
#   if (nrow(filtered_data) < 2 || 
#       length(unique(filtered_data$pop_id)) < 2 || 
#       length(unique(filtered_data$annee)) < 2 || 
#       length(unique(filtered_data$pro_libelle)) < 2 || 
#       length(unique(filtered_data$obj_libelle)) < 2) {
#     return(NULL)
#   }
#   
#  # model <- try(lmer(valeur ~ (1 | pop_id) + annee + pro_libelle + obj_libelle, data = filtered_data), silent = TRUE)
#   model <- try(glmer(valeur ~  scale(annee) + 
#                        scale(surface) + 
#                        (1 | pop_id) + 
#                        protocole, 
#                      data = filtered_data, 
#                      family = poisson(link = "log")), 
#                silent = TRUE)
#   
#   
#   if (inherits(model, "try-error")) {
#     return(NULL)
#   }
#   
#   res <- summary(model)$coefficients %>%
#     as.data.frame() %>%
#     rename(p_value = `Pr(>|z|)`) %>%
#     mutate(sig = case_when(
#       p_value < 0.001 ~ "***",
#       p_value < 0.01 ~ "**",
#       p_value < 0.05 ~ "*",
#       TRUE ~ ""
#     )) %>%
#     mutate(esp_code_alternatif = mon_espece,
#            stade = mon_stade,
#            indicateur = mon_indicateur)
#   
#   return(res)
# }
