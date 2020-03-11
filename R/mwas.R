#' MWAS Function
#'
#' @import tidyverse
#' 
mwas <- function(data, name_of_exposure){
  MWAS_output_lm <- data %>%
    dplyr::select( (match("lg_pfda",names(data)) + 1):ncol(data)) %>%  # exclude outcome, leave only predictors
    scale(.) %>% as_tibble() %>%
    map(~lm(data[[name_of_exposure]] ~ .x + data$og_age + data$sex + data$og_bmi, data = data)) %>%
    map(summary)
  
  MWAS_output <- tibble(name = names(MWAS_output_lm),
                        exposure = name_of_exposure,
                        beta = map_dbl(MWAS_output_lm, function(x){coefficients(x)[2,1]}),
                        se = map_dbl(MWAS_output_lm, function(x){coefficients(x)[2,2]}),
                        pvalue = map_dbl(MWAS_output_lm, function(x){coefficients(x)[2,4]}),
                        `t-score` = map_dbl(MWAS_output_lm, function(x){coefficients(x)[2,3]})) 
  
  # Calculate FDR Adjustments
  MWAS_output <- MWAS_output %>% 
    mutate(qvalue = p.adjust(pvalue, method = "fdr"), # Calculate FDR Adjustment  
           Significance = ifelse(pvalue < 0.05, "P<0.05", "Not Sig"), 
           SignificanceFDR = ifelse(qvalue < 0.05, "P<0.05", "Not Sig"), 
           names = if_else(pvalue < 0.05, name, ""), 
           namesFDR = if_else(qvalue < 0.05, name, ""), ) 
  return(MWAS_output)
}

