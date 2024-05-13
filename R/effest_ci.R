#' Get formatted character with effect estimate and 95\% CI
#' 
#' @import tidyverse
#' 
#' @export  
#' 
#' @param eff_est Numeric Vector
#' @param lcl Numeric Vector
#' @param ucl Numeric Vector
#' @param n.digits number of digits after the decimal point. Default is 2.
effest_ci <- function(eff_est, lcl, ucl, n.digits = 2){
  
  eff_est <- signif(eff_est, n.digits)  |>
    formatC(format="f", digits=n.digits)
  lcl <- signif(lcl, n.digits) |>
    formatC(format = "f", digits=n.digits)
  ucl <- signif(ucl, n.digits) |>
    formatC(format="f", digits=n.digits)
  
  eff_est_ci <- paste(eff_est, " (", lcl, ", ", ucl, ")", sep = "")
  
}
