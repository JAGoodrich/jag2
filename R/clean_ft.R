#' Clean Raw Feature Table
#' 
#' @import tidyverse janitor
#' 
#' @export
#' 
#' @param raw_ft Raw LC-MS Feature Table
#' @param samples Which to select? One of: "all", "sol", "q3". In future: "duplicates" 
clean_ft <- function(raw_ft, samples){
  if(samples == "sol"){
    data <- raw_ft %>% 
    clean_names() %>%
    select(mz, time, contains("sol"))
  }
  
  if(samples == "all"){
    data <- raw_ft %>% 
      clean_names() 
  }
    
  if(samples == "q3"){
    data <- raw_ft %>% 
      clean_names() %>%
      select(mz, time, contains("q3"))
  }
  
  

    # Identify unique metabolites
    data <- data %>%
      mutate(feature = str_c(mz, time, sep = "_") %>% as.factor) %>%
      select(mz, time, feature, everything()) %>% 
      as.data.frame()
    
    # Enter Row Names Prior to Transpose
    rownames(data) <- data$feature
    
    # Transpose
    data <- data %>%
      select(-mz, -time, -feature) %>%
      t() %>%
      as_tibble(rownames = "id")
    
    # Select only first instance of each id
    data <- data %>%
      filter(str_length(id) == 15) %>%
      mutate(id = str_sub(id, start = 1, end = 13))
    
    return(data)

}