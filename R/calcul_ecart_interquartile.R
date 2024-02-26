#' Calculer les longueurs médianes des espèces par opération selon les statuts des individus
#'
#' @param df Dataframe contenant les données
#' @param var_taille Variable de taille
#' @param var_id_site Variable d'identification du site
#' @param var_esp_code_alternatif Variable d'identification de l'espèce de poisson
#' @param var_statut Variable d'identification du statut de l'espèce (adulte / juvénile)
#'
#' @return 3 Dataframes avec une ligne par operation et par espèce et des colonnes indiquant le type d'indicateur "ecart_interquatile", et la valeur associée
#' @export
#' 
#' @importClassesFrom dplyr enquo group_by mutate filter summarize ungroup select
#'
#' @examples
#' \dontrun{
#' prov <- calcul_ecart_interquartile(df = ope_selection,
#' var_taille = mei_taille,
#' var_id_site = ope_id,
#' var_esp_code_alternatif = esp_code_alternatif,
#' var_statut = statut)
#' }

calcul_ecart_interquartile <- function(df,
                                    var_taille,
                                    var_id_site,
                                    var_esp_code_alternatif,
                                    var_statut)
{
  var_taille <- enquo(var_taille)
  var_id_site <- enquo(var_id_site)
  var_esp_code_alternatif <- enquo (var_esp_code_alternatif)
  var_statut <- enquo(var_statut)
  
  # Construction d'un df de l'écart interquartile des tailles des différents statuts des espèces (juvéniles / adultes) ----
    ope_interqua_statut <- df %>%
    group_by(!!var_esp_code_alternatif,
             !!var_id_site,
             !!var_statut) %>% 
summarise(valeur=IQR(!!var_taille, na.rm=TRUE))
  
  ope_interqua_statut <- ope_interqua_statut %>% 
    mutate(indicateur="ecart_interquartile") %>% 
    select(!!var_id_site,
           !!var_esp_code_alternatif,
           indicateur,
           valeur,
           !!var_statut)

  
  # Construction d'un Df de l'écart interquartile des espèces par opération toutes tailles confondues ----
  ope_interqua_esp <- df %>% 
    group_by(!!var_esp_code_alternatif, 
             !!var_id_site) %>% 
    summarise(valeur = IQR(!!var_taille, na.rm=TRUE))
  
  
  ope_interqua_esp <- ope_interqua_esp %>% 
    mutate(indicateur="ecart_interquartile", statut = "toutes") %>% 
    select(!!var_id_site,
           !!var_esp_code_alternatif,
           indicateur,
           valeur,
           !!var_statut)

  # Construction d'un Df avec les longueurs médianes des espèces par opération toutes tailles confondues + des différents statuts ----
  ope_interqua <- bind_rows(ope_interqua_statut, ope_interqua_esp)

  list(df1 = ope_interqua, 
       df2 = ope_interqua_esp,
       df3 = ope_interqua_statut)
} 
