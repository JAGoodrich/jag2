#' Get formatted character with geometric mean and sd
#' 
#' @import DescTools tidyverse
#' 
#' @export
#' 
#' @param x Numeric Vector
#' @param n.digits number of digits after the decimal point. Default is 1.
#' @param na.rm Remove NAs from calculation? Default is FALSE.
fungsd <- function(x, n.digits = 1, na.rm = FALSE){
  if( any(is.na(x))){
    if(na.rm == TRUE){warning("Data contains NA's")}  
    if(na.rm == FALSE){stop("Data contains NA, and na.rm is FALSE")}
  } else{ 
    if(any(x==0)){stop("Data contains 0's")}     
  }
  
  gm <- Gmean(x, na.rm = na.rm) %>% 
    round(., n.digits) %>% 
    formatC(. , format="f", digits=n.digits)
  gs <- Gsd(x, na.rm = na.rm) %>% 
    round(., n.digits) %>% 
    formatC(. , format="f", digits=n.digits)
  return(paste(gm[1], " (", gs,")", sep = ""))
}