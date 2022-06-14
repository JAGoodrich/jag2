#' MWAS Function (WORK IN PROGRESS)
#'
#' @import tidyverse
#' @export
#' 
#' @param metab_dat Data frame, where the first column is the unique id, and the rest of the columns are metabolites 
#' @param exp_cov_dat data frame which contains one column with the unique id as well as all of the covariates
#' @param covariates vector containing the covariates for 


## Run mode specific MWAS for all outcomes -------------------------
# Vars to test mwas_all_outcomes
# metab_dat = fts %>% modify(~.x %>% select(1:100))
# exp_cov_dat = tl_cov_out %>% 
#     filter(visit == 1)
# exposure_names = outcomes_for_analysis
# i = exposure_names[1]


# Function
mwas_all_outcomes <- function(metab_dat, 
                              exp_cov_dat, 
                              exposure_names){ 
  # Initialize list
  list_mwas_results <- vector("list", length(exposure_names))
  names(list_mwas_results) <- exposure_names
  
  # Run MWAS 
  for(i in exposure_names){
    print(paste(i))
    mwas_results <- mwas_e_m(metab_dat = metab_dat, 
                             exp_cov_dat = exp_cov_dat, 
                             name_of_outcome = i) %>% 
      list()
    
    list_mwas_results[i] <- mwas_results
  }
  
  list_mwas_results <- bind_rows(list_mwas_results, .id = "outcome") 
  
  return(list_mwas_results)
}



# MWAS Exposure --> Metabolite -----------------------------
mwas_e_m <- function(metab_dat, exp_cov_dat, name_of_outcome){
  # merge data to ensure matching keys
  data <- left_join(exp_cov_dat , 
                    metab_dat, 
                    by = c("id"))
  
  # only run analysis if outcome var has more than 1 value
  if(length(unique(data[[name_of_outcome]])) >1){
    
    # Run MWAS
    MWAS_output_lm <- data %>%
      dplyr::select(colnames(metab_dat)[2]:ncol(data)) %>%  # exclude outcome
      as_tibble() %>%
      map(~lm(data[[name_of_outcome]]~ .x  +
                data$q1_age_cohent + data$sex + data$q1_eth + 
                data$area + data$cc, 
              data = data)) %>%
      map(~tidy(.x, conf.int = TRUE)) %>% 
      bind_rows(.id = "name")
    
    
    # Add new variables, filter only the outcome var
    MWAS_output <- MWAS_output_lm %>% 
      mutate(outcome = name_of_outcome) %>% 
      filter(term == ".x") %>%
      select(outcome, name, everything(), -term) %>%
      janitor::clean_names()
    
    nfeat <- length(unique(MWAS_output$name))
    
    # Calculate FDR Adjustments
    MWAS_output <- MWAS_output %>% 
      group_by(outcome) %>%
      mutate(q_value = p.adjust(p_value, 
                                method = "fdr", 
                                n = nfeat), # Calculate FDR Adjustment  
             significance = ifelse(p_value < 0.05, "P<0.05", "Not Sig"), 
             significancefdr = ifelse(q_value < 0.05, "P<0.05", "Not Sig"), 
             names = if_else(p_value < 0.05, name, ""), 
             namesfdr = if_else(q_value < 0.05, name, "")) %>% 
      ungroup()
    
    return(MWAS_output)
  }
}