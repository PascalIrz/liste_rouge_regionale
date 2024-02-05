#' Déterminer les dates de début et de fin d'une série de données satisfaisant à une condition
#'     de proximité temporelle entre observations successives
#'
#' @param df Dataframe contenant les données
#' @param var_temp Variable temporelle
#' @param var_id_site Variable d'identification du site
#' @param max_nb_obs_manquantes Nombre maxi de données manquantes successives acceptées dans une série
#'
#' @return Dataframe avec une ligne par série et des colonnes indiquant les débuts, fin et nombre d'observations
#' @export
#' 
#' @importClassesFrom dplyr enquo group_by arrange mutate filter summarize ungroup select n
#'
#' @examples
#' \dontrun{
#' prov <- borner_series(df = annee_de_donnee,
#' var_id_site = pop_id,
#' var_temp = annee)
#' }
borner_series <- function(df,
                          var_temp,
                          var_id_site,
                          max_nb_obs_manquantes = 2)
  
{
  var_temp <- enquo(var_temp)
  var_id_site <- enquo(var_id_site)
  
  
  test <- df %>% 
    group_by(!!var_id_site) %>% 
    arrange (!!var_temp, .by_group = TRUE) %>% 
    mutate(obs_precedente := lag(!!var_temp),
           obs_manquantes := !!var_temp - obs_precedente - 1,
           obs_manquantes = ifelse(is.na(obs_manquantes), 0, obs_manquantes)) %>% 
    # #  group_by(id) %>% 
    mutate(boolean = obs_manquantes <= max_nb_obs_manquantes) 
  
  test %>%
    mutate(boolean = ifelse(is.na(boolean), 0, boolean)) %>%
    group_by(!!var_id_site, group = cumsum(c(0, diff(boolean) != 0))) %>%
    filter(boolean == 1 & n() > 1)  %>%
    summarize(debut = min(as.character(obs_precedente)),
              fin = max(as.character(!!var_temp)),
              n_opes= 1 + n()) %>%
    ungroup() %>%
    select(-group) %>% 
    mutate(n_opes = ifelse(is.na(debut), n_opes - 1, n_opes))
}

