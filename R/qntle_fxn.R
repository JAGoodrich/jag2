#' Get quantiles for exposure concentrations. Returns a charachter vector.
#' 
#' @import tidyverse janitor
#' @importFrom stats quantile
#' @export
#' 
#' @param x Vector of exposure concentrations
#' @param pct percentile cutoff
qntle_fxn <- function(x, pct){ 
  pct <- quantile(x, pct) |> signif(digits = 3)
  
  pct2 <- if_else(pct == 0, 
                  "<LOD", 
                  as.character(pct))
  return(pct2)
}