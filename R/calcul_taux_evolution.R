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

calcul_taux_evol <- function(df,
                             var_x,
                             var_y,
                             debut,
                             ...) 

  
{
  var_x <- enquo(var_x)
  var_y <- enquo(var_y)
  vars_group <- quos(...)

  
  # Filtrer les données
  df1 <- df %>%
    filter(!is.na(!!var_y),
           !!var_x >= debut)
  
  # Calculer les différences annuelles et les taux annuels
  df2 <- df1 %>%
    group_by(!!!vars_group) %>%
    arrange(!!var_x) %>%
    mutate(diff_annee =  !!var_x  - lag(!!var_x , default = first( !!var_x )),
           taux_annuel = ifelse(diff_annee > 0, (!!var_y / lag( !!var_y , default = first( !!var_y ))) ^ (1 / diff_annee), NA_real_)) %>%
  filter(!is.na(taux_annuel))
  
 # Calculer la moyenne géométrique des taux annuels
  mean_geom <- df2 %>%
  summarize(mean_geom = exp(mean(log(taux_annuel)))) %>% 
    mutate (debut = debut)

  return(mean_geom)
}
