#' Calculer les longueurs médianes (50e percentile)des espèces par opération selon les statuts des individus
#'
#' @param df Dataframe contenant les données
#' @param var_taille Variable de taille
#' @param var_id_site Variable d'identification du site
#' @param var_esp_code_alternatif Variable d'identification de l'espèce de poisson
#' @param var_statut Variable d'identification du statut de l'espèce (adulte / juvénile)
#'
#' @return 3 Dataframes avec une ligne par operation et par espèce et des colonnes indiquant le type d'indicateur "50_percentile", et la valeur associée
#' @export
#' 
#' @importClassesFrom dplyr enquo group_by mutate filter summarize ungroup select
#'
#' @examples
#' \dontrun{
#' prov <- calcul_50_percentile(df = ope_selection,
#' var_taille = mei_taille,
#' var_id_site = ope_id,
#' var_esp_code_alternatif = esp_code_alternatif,
#' var_statut = statut)
#' }

calcul_50_percentile <- function(df,
                                    var_taille,
                                    var_id_site,
                                    var_esp_code_alternatif,
                                    var_statut)
  {
  var_taille <- enquo(var_taille)
  var_id_site <- enquo(var_id_site)
  var_esp_code_alternatif <- enquo (var_esp_code_alternatif)
  var_statut <- enquo(var_statut)

  # Construction d'un Df avec les longueurs médianes des différents statuts des espèces (juvéniles / adultes) ----
  ope_50_percentile_statut <- df %>%
    group_by(!!var_esp_code_alternatif,
             !!var_id_site,
             !!var_statut) %>% 
    summarise (valeur = median(!!var_taille))

  ope_50_percentile_statut <- ope_50_percentile_statut %>%
    mutate(indicateur = "longueur_mediane") %>%
    select(!!var_id_site,
          !!var_esp_code_alternatif,
          indicateur,
          valeur,
          !!var_statut)
  
  # Construction d'un Df avec les longueurs médianes des espèces par opération toutes tailles confondues ----
  ope_50_percentile_esp <- df %>% 
    group_by(!! var_esp_code_alternatif, 
             !! var_id_site) %>% 
    summarise(valeur = median(!!var_taille))
  
  ope_50_percentile_esp <- ope_50_percentile_esp %>% 
    mutate(indicateur="longueur_mediane", statut = "toutes") %>% 
    select(!!var_id_site,
           !!var_esp_code_alternatif,
           indicateur,
           valeur,
           !!var_statut)

  # Construction d'un Df avec les longueurs médianes des espèces par opération toutes tailles confondues + des différents statuts ----
  ope_50_percentile <- bind_rows(ope_50_percentile_statut, ope_50_percentile_esp)
  
  list(df1 = ope_50_percentile, 
       df2 = ope_50_percentile_esp,
       df3 = ope_50_percentile_statut)
  } 
  
  