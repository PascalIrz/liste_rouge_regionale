
# d'après https://stackoverflow.com/questions/64845777/compute-statistical-tests-groupwise-in-r-using-dplyr


#' Mann-Kendall test combined to Sen-Theil slope estimation on a vector
#'
#' @param x A vector of class "numeric" or a time series object of class "ts".
#' @param ... Arguments to be passed to the function trend::mk.test()
#'
#' @return A dataframe with the test statistics.
#' @export
#' 
#' @importFrom trend mk.test sens.slope
#'
#' @examples
#' vector <- c(0, 3, 2, 5, 7, 6, 9, 8, 13, 16, 12)
#' mann_kendall_sen(vector)
mann_kendall_sen <- function(x, ...) 
{
  mk_pvalue <- mk.test(x, ...)
  sens_slope <- sens.slope(x, ...)
  
  # output
  data.frame(mk_pvalue = mk_pvalue$p.value,
             sens_slope = sens_slope$estimates)
}
