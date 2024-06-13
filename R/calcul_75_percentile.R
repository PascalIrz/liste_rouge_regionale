#' Calculer les percentiles 75 des tailles des espèces par opération selon les statuts des individus
#'
#' @param df Dataframe contenant les données
#' @param var_taille Variable de taille
#' @param var_id_site Variable d'identification du site
#' @param var_esp_code_alternatif Variable d'identification de l'espèce de poisson
#' @param var_stade Variable d'identification du stade de l'espèce (adulte / juvénile)
#'
#' @return 1 dataframe avec une ligne par operation et par espèce et des colonnes indiquant le type d'indicateur "75_percentile", et la valeur associée
#' @export
#' 
#' @importClassesFrom dplyr enquo group_by mutate filter summarize ungroup select bind_rows
#'
#' @examples
#' \dontrun{
#' prov <- calcul_75_percentile(df = ope_selection,
#' var_taille = mei_taille,
#' var_id_site = ope_id,
#' var_esp_code_alternatif = esp_code_alternatif,
#' var_stade = stade)
#' }
calcul_p75 <- function(df,
                       var_taille,
                       var_id_site,
                       var_esp_code_alternatif,
                       var_stade)
  
  
{
  var_taille <- enquo(var_taille)
  var_id_site <- enquo(var_id_site)
  var_esp_code_alternatif <- enquo (var_esp_code_alternatif)
  var_stade <- enquo(var_stade)
  
  
  # Construction d'un df des 75 percentiles des tailles des différents stades des espèces (juvéniles / adultes) ----
  ope_75_percentile_stade <- df %>%
    group_by(!!var_id_site,
             !!var_esp_code_alternatif,
             !!var_stade) %>% 
    summarise(valeur=quantile(!!var_taille, probs = c(.75), na.rm=TRUE))
  
  ope_75_percentile_stade <- ope_75_percentile_stade %>% 
    mutate(indicateur="75_percentile") %>% 
    select(!!var_id_site,
           !!var_esp_code_alternatif,
           indicateur,
           valeur,
           !!var_stade)
  
  
  # Construction d'un Df des 25 percentiles des espèces par opération toutes tailles confondues ----
  ope_75_percentile_esp <- df %>% 
    group_by(!!var_esp_code_alternatif, 
             !!var_id_site) %>% 
    summarise(valeur=quantile(!!var_taille, probs = c(.75), na.rm=TRUE))
  
  
  ope_75_percentile_esp <- ope_75_percentile_esp %>% 
    mutate(indicateur="75_percentile", stade = "ind") %>% 
    select(!!var_id_site,
           !!var_esp_code_alternatif,
           indicateur,
           valeur,
           !!var_stade)
  
  
  
  # Construction d'un Df avec les 75 percentiles des espèces par opération toutes tailles confondues + des différents stades ----
  ope_p75 <- bind_rows(ope_75_percentile_stade, ope_75_percentile_esp)
  return(ope_p75)
} 
