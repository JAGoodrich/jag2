#' Get formatted character with geometric mean and 95\% CI
#' 
#' @import DescTools tidyverse
#' 
#' @export  
#' 
#' @param x Numeric Vector
#' @param n.digits number of digits after the decimal point. Default is 3.
#' @param na.rm Remove NAs from calculation? Default is FALSE.
fungm <- function(x, n.digits = 3, na.rm = FALSE, CI_or_SD = "CI"){
  if( any(is.na(x))){
    if(na.rm == TRUE){warning("Data contains NA's")}  
    if(na.rm == FALSE){stop("Data contains NA, and na.rm is FALSE")}
  } else{ 
    if(any(x==0)){stop("Data contains 0's")}     
  }
  
  gm <- Gmean(x, conf.level = .95, na.rm = na.rm) %>% 
    signif(., n.digits) %>% 
    formatC(. , format="g", digits=n.digits)
  
  g_sd <- Gsd(x, na.rm = na.rm) %>% 
    signif(., n.digits) %>% 
    formatC(. , format="g", digits=n.digits)
  
  
  if(CI_or_SD == "CI"){
    gm2 <- paste(gm[1], " [", gm[2], ", ", gm[3], "]", sep = "")
  }
  if(CI_or_SD == "SD"){
    gm2 <- paste(gm[1], " (", g_sd, ")", sep = "")
  }
  
  return(gm2)
}