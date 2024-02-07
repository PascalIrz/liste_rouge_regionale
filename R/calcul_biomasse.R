## Création d'une fonction de calcul de biomasse à partir des tailles et des 
# poids_estimées par opération. 


calcul_biomasse <- function(df,
                            erreurs = FALSE)
  
{

  # Utilisation des relations taille - poids du package aspe
  # data("taille_poids")
taille_poids <- aspe::data_taille_poids %>%
    group_by(esp_code_alternatif,
             tlo_libelle) %>%
    arrange(source) %>%
    slice(1)
  
  
# nb : vérifier qu'on a bien les tailles pour tous les poissons avant d'appliquer la taille-poids
# combinaisons espèces - type de longueur absentes de la table de conversion
 
   tp_manquantes <- df %>% 
    select(esp_code_alternatif, tlo_libelle) %>% 
    distinct() %>% 
    left_join(y = taille_poids) %>% 
    filter(is.na(a))
  
  # nb : certaines espèces n'ont pas de relation en longueur fourche
  # on approxime par la longueur totale
 
    esp_tp_manquantes <- tp_manquantes %>% 
      pull(esp_code_alternatif)
  
  
  df <- df %>%
    mutate(tlo_libelle = ifelse(esp_code_alternatif %in% esp_tp_manquantes &
                                tlo_libelle == "Fourche",
                                "Totale",
                                tlo_libelle))
  
  
  df <- df %>% 
    left_join(y = taille_poids,
              by = c("esp_code_alternatif", "tlo_libelle")) %>%
    mutate(poids_tp = a * ((mei_taille / 10) ^ b)) # passage en cm de mei_taille => /10
  
  if(erreurs == TRUE)
  {
    df <- df %>% 
      mutate(
        erreur_abs = poids_tp - mei_poids,
        erreur_rel = erreur_abs / poids_tp)
  }
  
  df

}



