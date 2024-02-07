## Création d'une fonction de calcul de densité à partir des longueurs et des 
# poids_estimées par opération. 


calcul_biomasse <- function(df,
                          var_longueur,
                          var_type_longueur,
                          var_taxon)
  
{
  var_longueur <- enquo(mei_taille, mei_poids_estime)
  var_type_longueur <- enquo(tlo_libelle)
  var_taxon <- enquo(esp_code_alternatif, esp_nom_commun)

  
# Incorporation des mesures individuelles ----
  
  data_ind <- df %>%
    mef_ajouter_libelle() %>% 
    mef_ajouter_ope_date() %>% 
    mef_ajouter_passage() %>% 
    mef_ajouter_lots() %>% 
    mef_ajouter_esp() %>%
    mef_ajouter_mei() %>%
    mef_ajouter_type_longueur() %>%
    select(mei_id,
           sta_id,
           pop_id,
           dept,
           annee,
           ope_date,
           ope_id,
           lop_id,
           esp_code_alternatif,
           esp_nom_commun,
           mei_taille,
           mei_poids,
           mei_poids_estime,
           mei_mesure_reelle,
           tlo_libelle,
           -mei_mep_id,
           -tyl_id,
           -tlo_id) %>%
    mutate(esp_code_alternatif2 = paste0(esp_nom_commun, "(", esp_code_alternatif, ")"))
  
  
  # Exemple : Selection de la station l'AER le Croisty dans le 56 - constitution jeu de donnée réduit
  # data_ind_aer <- data_ind %>%
  # filter(sta_id %in% c("11601"))
  
  
  
  # Utilisation des relations taille - poids du package aspe
  
  # data("taille_poids")
  
  taille_poids <- data_taille_poids %>%
    group_by(esp_code_alternatif,
             tlo_libelle) %>%
    arrange(source) %>%
    slice(1)
  
  
# nb vérifier qu'on a bien les tailles pour tous les poissons avant d'appliquer la taille-poids
# combinaisons espèces - type de longueur absentes de la table de conversion
  tp_manquantes <- data_ind %>% 
    select(esp_code_alternatif, tlo_libelle) %>% 
    distinct() %>% 
    left_join(y = taille_poids) %>% 
    filter(is.na(a))
  
  
  # on voit que le pb est que pas mal d'espèces n'ont pas de relation en longueur fourche
  # on approxime par la longueur totale
  esp_tp_manquantes <- tp_manquantes %>% 
    pull(esp_code_alternatif)
  
  
  data_ind <- data_ind %>%
    mutate(tlo_libelle = ifelse(esp_code_alternatif %in% esp_tp_manquantes &
                                  tlo_libelle == "Fourche",
                                "Totale",
                                tlo_libelle))
  
  
  data_ind <- data_ind %>% 
    left_join(y = taille_poids,
              by = c("esp_code_alternatif", "tlo_libelle")) %>%
    mutate(mei_taille = mei_taille / 10,   # passage en cm
           poids_tp = a * (mei_taille ^ b),
           erreur_abs = poids_tp - mei_poids,
           erreur_rel = erreur_abs / poids_tp)
  
  
  
  data_ind_poids <- data_ind %>% 
    group_by(ope_id, esp_code_alternatif) %>%
    summarise(poids = sum(poids_tp)) %>% 
    ungroup()
  
  
  data_dept_poids <- data_ind %>% 
    group_by(esp_nom_commun,
             annee,
             dept) %>% 
    summarise(poids = sum(poids_tp, na.rm = TRUE),
              .groups = "drop")
}
  