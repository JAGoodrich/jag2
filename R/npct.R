#' Get formatted character with n and percent from a factor, character, or numeric
#' 
#' @import tidyverse
#' 
#' @export
#' 
#' @param x Values
#' @param level_of_interest Value of interest, for example, "Male"
#' @param n.digits number of digits after the decimal point. Default is 1.
npct <- function(x, level_of_interest, n.digits = 1){ 
  number_meeting_criteria = sum(x == level_of_interest)
  pct_of_pop = 100*round(number_meeting_criteria/length(x),
                         digits = n.digits)
  return(paste(number_meeting_criteria, 
               " (", 
               pct_of_pop,
               "%)", sep = ""))
}