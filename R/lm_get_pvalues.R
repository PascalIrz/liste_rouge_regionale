#' Extraire les p.values issues de regression linéaires
#' 
#' @param data Dataframe contenant les données
#' @param strat_year Variable temporelle - Année de départ considérée
#' @param end_year Variable temporelle - Année de fin considérée
#'
#' @return Dataframe avec les p.values par espèces, indicateurs et périodes
#' @export
#' 
#' @importClassesFrom dplyr filter group_by do tidy lm select
#'
#' @examples
#' \dontrun{
#' prov <- borner_series(df = annee_de_donnee,
#' var_id_site = pop_id,
#' var_temp = annee)
#' }




lm_get_pvalues <- function(data, 
                        start_year, 
                        end_year,
                        significativite) {
  
  # Filtrage des données selon les années spécifiées
  data_filtered <- data %>% 
    filter(annee >= start_year & annee <= end_year)
  
  # Calcul des p-values et estimate
  results <- data_filtered %>%
    group_by(stade, 
             indicateur, 
             esp_code_alternatif) %>%
    do(tidy(lm(valeur ~ annee, data = .)))
  
  # Filtrage du terme "annee" et sélection des colonnes pertinentes
  results <- results %>%
    filter(term == "annee") %>%
    select(indicateur, 
           esp_code_alternatif, 
           estimate, 
           p.value) %>% 
    mutate(
      significance = ifelse(p.value < significativite, "significant", "non_significant"),
      periode = paste0(start_year, "_", end_year)  # Ajout de la colonne période
    ) %>%
    mutate( # Ajout des colonnes trend et sig
      trend = case_when(
        estimate > 0 ~ "\U2197",
        estimate < 0 ~ "\U2198",
        TRUE ~ "."
      ),
      sig = case_when(
        p.value < 0.001 ~ "***",
        p.value < 0.01 ~ "**",
        p.value < 0.05 ~ "*",
        TRUE ~ ""
      )
    ) %>%
    select(periode, everything())  # Placer la colonne "période" en premier
  
  return(results)
}
