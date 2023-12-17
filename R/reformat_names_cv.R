#' Reformat author names for CV
#' 
#' @import stringr purrr
#' @export 
#' 
#' @param name_string String of author names, in the format "first last#, ..."
reformat_names <- function(name_string) {
  # Step 1: Remove all digits and spaces from the name string
  name_string <- str_remove_all(name_string, "\\d+")
  
  # Step 2: Split into names
  names <- str_split(name_string, ", ")[[1]]
  
  # Steps 3-5
  reformatted_names <- names %>%
    map(function(name) {
      # Split into first name, middle initial (if exists) and last name
      first_middle_last <- str_split(name, " ")[[1]]
      
      # Concatenate first name's first letter and middle initial (if exists)
      first_middle_initial <- paste0(substring(first_middle_last[1], 1, 1),
                                     ifelse(length(first_middle_last) == 3, 
                                            str_remove(substring(first_middle_last[2], 1, 2), "\\."), 
                                            ""))
      
      # Combine with last name
      reformatted_name <- paste0(ifelse(length(first_middle_last) == 3, 
                                        first_middle_last[3], 
                                        first_middle_last[2]), 
                                 " ", first_middle_initial)
      
      return(reformatted_name)
    }) %>%
    paste(collapse = ", ")
  
  return(reformatted_names)
}