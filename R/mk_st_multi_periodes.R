# Définir une fonction pour itérer sur plusieurs périodes
mk_st_multi_periodes <- function(df,
                                 periods,
                                 valeur,
                                 annee,
                                 esp_code_alternatif,
                                 indicateur,
                                 stade) 

  {
  resultats <- list()
  
  for (period in periods) {
    start_year <- period[1]
    end_year <- period[2]
    
    # Filtrer les données pour la période spécifique
    period_data <- df[df[[annee]] >= start_year & df[[annee]] <= end_year, ]
    
    # Appliquer mk_st_by_group pour cette période
    mk_resultats <- mk_st_by_group(period_data,
                                   valeur,
                                   annee,
                                   esp_code_alternatif,
                                   indicateur,
                                   stade)
    
    mk_resultats <- mk_resultats %>% 
      mutate (periode = paste0(start_year, "-", end_year)) %>% 
      mutate(sig = case_when(
        mk_pvalue < 0.001 ~ "***",
        mk_pvalue < 0.01 ~ "**",
        mk_pvalue < 0.05 ~ "*",
        TRUE ~ ""))
    
    # Stocker les résultats
    resultats[[paste0(start_year, "-", end_year)]] <- mk_resultats
  }
  
  return(resultats)
}