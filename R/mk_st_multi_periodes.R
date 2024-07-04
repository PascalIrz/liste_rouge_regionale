# Définir une fonction pour itérer sur plusieurs périodes
mk_st_multi_periodes <- function(df,
                                 periods,
                                 valeur,
                                 annee,
                                 esp_code_alternatif,
                                 indicateur,
                                 stade,
                                 pop_id) {
  
  resultats <- list()
  
  for (period in periods) {
    start_year <- period[1]
    end_year <- period[2]
    
    # Filtrer les données pour la période spécifique
    period_data <- df %>%
      filter(!!sym(annee) >= start_year & !!sym(annee) <= end_year)
    
    # Appliquer mk_st_by_group pour cette période
    mk_resultats <- mk_st_by_group(period_data,
                                   valeur,
                                   annee,
                                   esp_code_alternatif,
                                   indicateur,
                                   stade,
                                   pop_id) # La je le rajoute pour la manip glm mais à enlever sinon
    
    # Ajouter la période comme colonne
    mk_resultats <- mk_resultats %>% 
      mutate(periode = paste0(start_year, "-", end_year))
    
    # Calculer 'trend' comme flèche montante, descendante ou point
    mk_resultats <- mk_resultats %>% 
      mutate(trend = case_when(
        sens_slope < 0 ~ "\U2198",   # flèche vers le bas
        sens_slope > 0 ~ "\U2197",   # flèche vers le haut
        TRUE ~ "."))                 # point si sens_slope == 0
    
    # Ajouter la colonne 'sig' en fonction de 'mk_pvalue'
    mk_resultats <- mk_resultats %>% 
      mutate(sig = case_when(
        mk_pvalue < 0.001 ~ "***",
        mk_pvalue < 0.01 ~ "**",
        mk_pvalue < 0.05 ~ "*",
        TRUE ~ ""))
    
    # Sélectionner les colonnes finales
    mk_resultats <- mk_resultats %>% 
      select(esp_code_alternatif, stade, indicateur, mk_pvalue, trend, sig, sens_slope)
    
    # Stocker les résultats pour cette période
    resultats[[paste0(start_year, "-", end_year)]] <- mk_resultats
  }
  
  return(resultats)
}
