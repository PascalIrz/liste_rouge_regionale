
application_modeles <- function(data) {
  
  combinations <- data %>%
    select(espece, stade, indicateur) %>%
    distinct()
  
  results_list <- combinations %>%
    pmap(function(espece, stade, indicateur) {
      calculate_model_glmm(data, espece, stade, indicateur)
    })
  
  results_df <- bind_rows(results_list, .id = "combination_id") %>%
    filter(!is.null(.))  # Filtrer les résultats nuls
  
  return(results_df)
}