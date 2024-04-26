
lrr_critere_a <- function(df,
                          var_espece,
                          var_indicateur,
                          var_stade,
                          var_temps_generation)
  
{
  var_espece <- enquo(var_espece)
  var_indicateur <- enquo(var_indicateur)
  var_stade <- enquo(var_stade)
  var_temps_generation <- enquo(var_temps_generation)
  
  # Filtrer le dataframe pour ne conserver que les lignes correspondant à var_espece, var_stade et var_temps_generation
  df_filtered <- df %>%
    filter(esp_code_alternatif == !!var_espece) %>%
    filter(stade == !!var_stade) %>%
    filter(indicateur == !!var_indicateur)
  
  # Sélectionner uniquement les lignes pour l'année la plus récente et l'année - temps de génération de l'espèce
  df_selected <- df_filtered %>%
    filter(annee %in% c(max(annee), max(annee) - !!var_temps_generation))
  
  # Trouver la valeur la plus récente et celle de -temps de génération
  latest_value <- df_selected[df_selected$annee == max(df_selected$annee), "valeur"]
  value_generation_years_ago <- df_selected[df_selected$annee == max(df_selected$annee) - !!var_temps_generation, "valeur"]

  # Calculer l'évolution de l'indicateur
  percentage_change <- ((latest_value - value_generation_years_ago) / value_generation_years_ago) * 100

  return(percentage_change)
}
