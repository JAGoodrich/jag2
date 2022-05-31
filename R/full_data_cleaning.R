#' Clean LC-MS Data
#' 
#' @import tidyverse
#' 
full_data_cleaning <- function(data, pct_metab_na_allowed, pct_obs_na_allowed){
  metabolites <- data %>%
    select((match("lg_pfda",names(data)) + 1):ncol(data))
  
  data <- data %>% 
    select("id":"lg_pfda")
  
  # Specify Fraction Metabolite Missing Allowed
  mv <- pct_metab_na_allowed
  # Drop metabolites with more missing than allowed
  metabolites <- metabolites[, which(colMeans(is.na(metabolites)) < mv)]
  
  data <- cbind(data, metabolites)
  
  # Specify Fraction Missing Metabolies allowed for each observation
  mv_id <- pct_obs_na_allowed
  
  data <- data %>% 
    filter(rowMeans(is.na(metabolites)) < mv_id)
  
  return(data)
}
