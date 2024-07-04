glmm_application_modele <- function(data, 
                                    liste_periodes) {
  
  results_list <- map(liste_periodes, function(period) {
    mon_annee_depart <- period[1]
    mon_annee_fin <- period[2]
    
    # Filtrer les données pour la période courante
    period_data <- data %>%
      filter(annee >= mon_annee_depart & annee <= mon_annee_fin)
    
    combinations <- period_data %>%
      select(espece) %>%
      distinct()
    
    period_label <- paste0(mon_annee_depart, "-", mon_annee_fin)
    
    results <- combinations %>%
      pmap(function(espece, stade, indicateur, surface, protocole) 
    {
        glmm_calcul_modele(data = period_data,
                           mon_espece = espece)
      }) %>%
      keep(~ !is.null(.)) %>%
      map_dfr(~ mutate(.x, row_name = rownames(.x), periode = period_label))
    
    return(results)
  }) %>%
    bind_rows()
  
  return(results_list)
}

















# glmm_application_modele <- function(data, 
#                                    liste_periodes) {
#   
#   results_list <- map(liste_periodes, function(period) {
#     mon_annee_depart <- period[1]
#     mon_annee_fin <- period[2]
#     
#     # Filtrer les données pour la période courante
#     period_data <- data %>%
#       filter(annee >= mon_annee_depart & annee <= mon_annee_fin)
#     
#     combinations <- period_data %>%
#       select(espece, stade, indicateur) %>%
#       distinct()
#     
#     period_label <- paste0(mon_annee_depart, "-", mon_annee_fin)
#     
#     combinations %>%
#       pmap(function(espece, stade, indicateur) {
#         results <- glmm_calcul_modele(data = period_data,
#                                      mon_espece = espece,
#                                      mon_stade = stade,
#                                      mon_indicateur = indicateur,
#                                      ma_surface = ope_surface_calculee,
#                                      mon_protocole = pro_libelle)
#         if (!is.null(results)) {
#           # Ajouter les noms des lignes comme colonne
#           results <- results %>%
#             mutate(row_name = rownames(results),
#                    periode = period_label)
#         }
#         return(results)
#       })
#   }) %>%
#     flatten() %>%
#     bind_rows()
#   
#   return(results_list)
# }
