#' Calculer les percentiles 25 des tailles des espèces par opération selon les stades des individus
#'
#' @param df Dataframe contenant les données
#' @param var_taille Variable de taille
#' @param var_id_site Variable d'identification du site
#' @param var_esp_code_alternatif Variable d'identification de l'espèce de poisson
#' @param var_stade Variable d'identification du stade de l'espèce (adulte / juvénile)
#'
#' @return 3 Dataframes avec une ligne par operation et par espèce et des colonnes indiquant le type d'indicateur "25_percentile", et la valeur associée
#' @export
#' 
#' @importClassesFrom dplyr enquo group_by mutate filter summarize ungroup select bind_rows
#'
#' @examples
#' \dontrun{
#' prov <- calcul_25_percentile(df = ope_selection,
#' var_taille = mei_taille,
#' var_id_site = ope_id,
#' var_esp_code_alternatif = esp_code_alternatif,
#' var_stade = stade)
#' }
calcul_25_percentile <- function(df,
                                 var_taille,
                                 var_id_site,
                                 var_esp_code_alternatif,
                                 var_stade)

  
{
  var_taille <- enquo(var_taille)
  var_id_site <- enquo(var_id_site)
  var_esp_code_alternatif <- enquo (var_esp_code_alternatif)
  var_stade <- enquo(var_stade)
  

 # Construction d'un df des 25 percentiles des tailles des différents stades des espèces (juvéniles / adultes) ----
  ope_25_percentile_stade <- df %>%
    group_by(!!var_id_site,
             !!var_esp_code_alternatif,
             !!var_stade) %>% 
    summarise(valeur=quantile(!!var_taille, probs = c(.25), na.rm=TRUE))
  
  ope_25_percentile_stade <- ope_25_percentile_stade %>% 
    mutate(indicateur="25_percentile") %>% 
    select(!!var_id_site,
           !!var_esp_code_alternatif,
           indicateur,
           valeur,
           !!var_stade)

  
# Construction d'un Df des 25 percentiles des espèces par opération toutes tailles confondues ----
  ope_25_percentile_esp <- df %>% 
    group_by(!!var_esp_code_alternatif, 
             !!var_id_site) %>% 
    summarise(valeur=quantile(!!var_taille, probs = c(.25), na.rm=TRUE))
  
  
  ope_25_percentile_esp <- ope_25_percentile_esp %>% 
    mutate(indicateur="25_percentile", stade = "ind") %>% 
    select(!!var_id_site,
           !!var_esp_code_alternatif,
           indicateur,
           valeur,
           !!var_stade)
  

  
  # Construction d'un Df avec les 25 percentiles des espèces par opération toutes tailles confondues + des différents stades ----
  ope_25_percentile <- bind_rows(ope_25_percentile_stade, ope_25_percentile_esp)
  
  list(df1 = ope_25_percentile, 
       df2 = ope_25_percentile_esp,
       df3 = ope_25_percentile_stade)
} 
  