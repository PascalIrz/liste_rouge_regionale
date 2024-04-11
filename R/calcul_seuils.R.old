
calcul_seuils <- function(df,
                          var_taxon,
                          var_a_tester) {
  
  var_taxon <- enquo(var_taxon)
  var_a_tester <- enquo(var_a_tester)
  
  # Création d'une liste pour stocker les données des seuils pour chaque espèce
  seuils_list <- list()
  
  # Extraire les valeurs uniques de la variable spécifiée à partir du jeu de données
  codes <- df %>%
    pull(!!var_taxon) %>%
    unique()
  
  seuils <- map(
    .x = codes,
    .f = aspeQual::qtp_seuils,
    df = df,
    variable = var_a_tester,
    seuil_densite = 0.001
    )
  
  seuils

  
  # Boucle pour calculer les seuils min et max pour chaque espèce
  # for (code in codes) {
  #   # Filtrer les données pour l'espèce actuelle
  #   esp_data <- df %>%
  #     filter(!!var_taxon == code)
  # 
  #   # Calculer les seuils pour l'espèce actuelle
  #   seuils <- aspeQual::qtp_seuils(
  #     df = esp_data,
  #     code_espece = code,
  #     variable = !!var_a_tester,
  #     seuil_densite = 0.001
  #   )
  # 
  #   # Stocker les seuils min et max dans une liste
  #   seuils_list[[code]] <- data.frame(
  #     esp_code_alternatif = code,
  #     seuil_min = min(seuils),
  #     seuil_max = max(seuils)
  #   )
  # }

  # Combiner les données des seuils pour chaque espèce en un seul DataFrame
  # mei_df_seuils <- do.call(rbind, seuils_list)
  # 
  # # Joindre les seuils calculés avec le jeu de données principal
  # df <- left_join(df, mei_df_seuils)
  # 
  # return(df)
  
}
