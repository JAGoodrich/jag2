#' Get formatted character vector of the Mean (IQR). 
#' 
#' @import tidyverse
#' @importFrom stats IQR median
#' 
#' @export
#' 
#' @param x Numeric Vector
#' @param n.digits number of digits after the decimal point. Default is 1.
median_iqr_fxn <- function(x, n.digits = 1) {
  out <- paste0(round(median(x, na.rm = T), n.digits) |>
                 formatC(format="f", digits=n.digits),
               " (",
               round(IQR(x, na.rm = T), n.digits) |>
                 formatC(format="f", digits=n.digits), 
               ")") 
  return(out)
}
