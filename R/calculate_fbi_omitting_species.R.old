#' Calculate the Fish-based Index after removing the taxa one-by-one
#' 
#' Based on the function calc_indic() of R package SEEEapi (see https://github.com/CedricMondy/SEEEapi),
#'     itself iminig at providing an easy way to access the indicator calculation services from the
#'     French system for evaluating water status (see http://seee.eaufrance.fr/).
#'
#' @param species_codes Character vector of the taxa 3-letters codes.
#' @param df_fauna Dataframe containing the data with variables CODE_OPERATION (survey id),
#'     CODE_TAXON (taxa code) and RESULTAT (nb of individuals caught). The variable names are
#'     fixed because the function calls pre-existing tools requiring this format. For more info,
#'     see the header of script "scripts/IPR_v1.0.3_calc_consult.r" (sorry, in French).
#' @param file_fauna Character. File path used to store a text file with the data. The directory has
#'     to pre-exist.
#' @param dir_algorithm Character. Directory path indicating where the script "IPR_v1.0.3_calc_consult.r"
#'     is stored.
#'
#' @return A dataframe with one row per survey and the contribution of each species to each survey as variables.
#'     This contribution is calculated as actual_FBI - FBI_excluding_my_species.
#' @export
#'
#' @examples
#' \dontrun{
#' test2_sp <-
#'   calculate_fbi_omitting_species(species_codes = c("GAR", "TRF"),
#'                                  df_fauna = data_faune,
#'                                  file_fauna = file_faune,
#'                                  dir_algorithm = "scripts")
#' }
calculate_fbi_omitting_species <- function(species_codes,
                                           df_fauna,
                                           file_fauna,
                                           dir_algorithm)
  
  # function for a single species ---
{
  calculate_fbi_omitting_1_species <- function(species_code,
                                               df_fauna,
                                               file_fauna,
                                               dir_algorithm)
    
  {
    sub_data_fauna <- df_fauna %>%
      filter(CODE_TAXON != species_code)
    
    write_delim(x = sub_data_fauna,
                path = file_fauna,
                delim = "\t")
    
    
    result <- calc_indic(
      indic      = "IPR",
      version    = "1.0.3",
      file_paths = c(file_envir, file_fauna),
      locally = TRUE,
      dir_algo = dir_algorithm
    )
    
    data <- result$result %>%
      mutate(omitted_species = species_code)
    
    assign(species_code,
           data)
    
  }
  
  # test <-
  #   calculate_fbi_omitting_1_species(species_code = "GAR",
  #                                   df_fauna = data_faune,
  #                                   file_fauna = file_faune,
  #                                   dir_algorithm = "scripts")
  
  # END function for a single species
  
  
  # Apply to all the species  ---
  
  map(
    .x = species_codes,
    .f = calculate_fbi_omitting_1_species,
    df_fauna = df_fauna,
    file_fauna = file_fauna,
    dir_algorithm = dir_algorithm
  ) %>%
    reduce(rbind)
  
}

# test2_sp <-
#   calculate_fbi_omitting_species(species_codes = c("GAR", "TRF"),
#                                  df_fauna = data_faune,
#                                  file_fauna = file_faune,
#                                  dir_algorithm = "scripts")
