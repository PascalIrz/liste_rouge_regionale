#' Calculer les taux d'évolution des espèces par annees selon la variable souhaitée
#'
#' @param df Dataframe contenant les données
#' @return Dataframe avec les taux d'évolution par espèce de la variable souhaitée
#' @export
#' 
#' @importClassesFrom dplyr enquo group_by mutate filter summarize ungroup select unique
#'
#' @examples
#' \dontrun{
#' }

calcul_moyenne_geom <- function(data, 
                                var_x, 
                                var_y,
                                var_annee_depart,
                                ...) 

  
{
  var_x <- enquo(var_x)
  var_y <- enquo(var_y)
  var_annee_depart <- enquo(var_annee_depart)
  vars_group <- quos(...)

  
  # Filtrer les données
  df <- data %>% 
    filter(!is.na(!!var_y),
           !!var_x >= !!var_annee_depart,
           !!!vars_group)
  
  # Calculer les différences annuelles et les taux annuels
  df1 <- df %>% 
    group_by(!!!vars_group) %>% 
    arrange(!!var_x) %>% 
    mutate(diff_annee =  !!var_x  - lag(!!var_x , default = first( !!var_x )),
           taux_annuel = ifelse(diff_annee > 0, (!!var_y / lag( !!var_y , default = first( !!var_y ))) ^ (1 / diff_annee), NA_real_)) %>% 
    filter(!is.na(taux_annuel))
  
  # Calculer la moyenne géométrique des taux annuels
  mean_geom <- df1 %>%
    summarize(mean_geom = exp(mean(log(taux_annuel), na.rm = TRUE))) %>%
    pull(mean_geom)
  
  # Créer un dataframe avec les valeurs calculées
  result_df <- data.frame(Espece = unique(data$esp_code_alternatif),
                          Indicateur = unique(data$indicateur),
                          Annee_Depart = min(data[[!!var_x]]),
                          Moyenne_Geometrique = mean_geom,
                          Stade = unique(data$stade))
  
  return(result_df)
}
