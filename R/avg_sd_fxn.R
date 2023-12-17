#' Get formatted character vector of the average +/- SD. 
#' 
#' @import tidyverse
#' 
#' @export
#' 
#' @param x Numeric Vector
#' @param n.digits number of digits after the decimal point. Default is 1.
#' @param include.n include sample size? Default is false.  
avg_sd_fxn <- function(x, n.digits = 1, include.n = FALSE) {
  out <- paste(round(mean(x, na.rm = T), n.digits) %>% 
                 formatC(. , format="f", digits=n.digits),
               round(sd(x, na.rm = T), n.digits) %>% 
                 formatC(. , format="f", digits=n.digits), sep = " ± ") 
  if(include.n == TRUE) {
    out <- paste(out, 
                 " (",
                 paste(length(which(!is.na(x))), ")", sep = ""), 
                 sep = "") }
  return(out)
}