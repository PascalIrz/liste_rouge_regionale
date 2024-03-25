#' Calculer les densites de surface des espèces par opération selon les différents stades des individus
#'
#' @param df Dataframe contenant les données
#' @param var_surface_calculee Variable des surfaces calculees à l'échelle de l'opération
#' @param var_id_site Variable d'identification du site
#' @param var_esp_code_alternatif Variable d'identification de l'espèce de poisson
#' @param var_stade Variable d'identification du stade de l'espèce (ad / juv)
#' @param var_id_taille Variable d'identifiant de la mesure individuelle
#' @param var_profondeur Variable environnementale sur la profondeur relative à l'opération
#'
#' @return 3 Dataframes avec une ligne par operation et par espèce et des colonnes indiquant le type d'indicateur "ecart_interquatile", et la valeur associée
#' @export
#' 
#' @importClassesFrom dplyr enquo group_by mutate filter summarize ungroup select bind_rows
#'
#' @examples
#' \dontrun{
#' prov <- calcul_densite_surface(df = ope_selection,
#' var_surface_calculee = ope_surface_calculee,
#' var_id_site = ope_id,
#' var_esp_code_alternatif = esp_code_alternatif,
#' var_stade = stade,
#' var_id_taille = mei_id)
#' }
calcul_densite_surface <- function(df,
                                   var_surface_calculee,
                                   var_id_site,
                                   var_esp_code_alternatif,
                                   var_stade,
                                   var_id_taille)

{
  var_surface_calculee <- enquo(var_surface_calculee)
  var_id_site <- enquo(var_id_site)
  var_esp_code_alternatif <- enquo (var_esp_code_alternatif)
  var_stade <- enquo(var_stade)
  var_id_taille <- enquo(var_id_taille)

  
  ope_densite_surface_stade <- df %>%
    group_by(!!var_id_site,
             !!var_esp_code_alternatif,
             !!var_surface_calculee,
             !!var_stade) %>% 
    summarise(effectif=sum(length(!!var_id_taille))) %>% 
    ungroup() %>% 
    mutate(indicateur= "densite_surface") %>% 
    mutate (valeur = (effectif / !!var_surface_calculee)) %>%
    select(!!var_id_site,
           !!var_esp_code_alternatif,
           indicateur,
           valeur,
           !!var_stade)
  
  
  ope_densite_surface_esp <- df %>% 
    group_by(!!var_id_site,
             !!var_esp_code_alternatif,
             !!var_surface_calculee) %>% 
    summarise(effectif=sum(length(!!var_id_taille))) %>% 
    ungroup() %>% 
    mutate(indicateur="densite_surface", stade = "ind")  %>% 
    group_by(!!var_esp_code_alternatif, 
             !!var_id_site,
             !!var_stade, 
             indicateur) %>% 
    summarise(valeur = (effectif / !!var_surface_calculee)) %>% 
    select(!!var_id_site,
           !!var_esp_code_alternatif,
           indicateur,
           valeur,
           !!var_stade)
  
  
  # Construction d'un Df avec les densités des espèces par opération toutes tailles confondues + des différents statuts ----
  ope_densite_surface <- bind_rows(ope_densite_surface_stade, ope_densite_surface_esp)

  
  list(df1 = ope_densite_surface, 
       df2 = ope_densite_surface_esp,
       df3 = ope_densite_surface_stade)
} 
  