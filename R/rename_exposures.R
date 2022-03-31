#' Rename exposures, including PFAS and lipophilic pops.
#' 
#' @import tibble dplyr
#' 
#' @export
#' 
#' @param exposure_names character vector of lowercase PFAS or lipophilic POPs names
#' @param include_asterisk Include asterisk for exposures which will likely be dichotomous? May need to be altered for different projects based on percent below lod. 
#' @param arrange_by_class Order factor levels by chemical class?
rename_exposures <- function(exposure_names, 
                             include_asterisk = FALSE, 
                             arrange_by_class = FALSE){
  
  x <- tibble::tibble(exposure = exposure_names)
  
  suppressWarnings(
    exposure2 <-  x %>%
      dplyr::mutate(exposure2 = dplyr::case_when(
        exposure == "pfhxs" ~ "PFHxS",
        exposure == "pfhps" ~ "PFHpS",
        exposure == "pfpes" ~ "PFPeS",
        exposure == "pfhpa" ~ "PFHpA",
        exposure == "nmefosaab" ~ "N-MeFOSAA-b", 
        exposure == "pfuda" ~ "PFUnDA",
        exposure == "pfds" ~ "PFDS",
        exposure == "netfosaa" ~ "N-EtFOSAA",
        exposure == "pfns" ~ "PFNS",
        exposure == "pfbs" ~ "PFBS",
        exposure == "x82fts" ~ "8:2 FTS", 
        exposure == "pfhxa" ~ "PFHxA", 
        exposure == "pfdoa" ~ "PFDoDA",
        exposure == "Mixture effect" ~ "Mixture effect",
        TRUE ~ toupper(exposure)) %>% 
          as.factor() %>% 
          forcats::fct_relevel(., 
                               "PFOS", "PFOA", "PFHxS", "PFNA", "PFHpS","PFDA", "PFPeS", 
                               "PFHpA","N-MeFOSAA-b","N-EtFOSAA","PFDS","PFBS", 
                               "8:2 FTS", "PFDoDA", "PFUnDA","PFNS","PFHxA",
                               "Mixture effect", 
                               "HCB", "DDE", "Sum of OC Compounds",
                               "PBDE-154", "PBDE-47","PBDE-100", "PBDE-153","PBDE-85", 
                               "PCB-118","PCB-138", "PCB-153","PCB-180",
                               "Number of detected PCB's", 
                               "Number of detected PBDE's"))  
  )
  
  if(include_asterisk == TRUE){ 
    suppressWarnings(
      exposure2 <-  x %>%
        dplyr::mutate(exposure2 = dplyr::case_when(
          exposure == "pfhxs" ~ "PFHxS",
          exposure == "pfhps" ~ "PFHpS",
          exposure == "pfpes" ~ "PFPeS",
          exposure == "pfhpa" ~ "PFHpA",
          exposure == "nmefosaab" ~ "N-MeFOSAA-b*", 
          exposure == "pfuda" ~ "PFUnDA*",
          exposure == "pfds" ~ "PFDS*",
          exposure == "netfosaa" ~ "N-EtFOSAA*",
          exposure == "pfns" ~ "PFNS*",
          exposure == "pfbs" ~ "PFBS*",
          exposure == "x82fts" ~ "8:2 FTS*", 
          exposure == "pfhxa" ~ "PFHxA*", 
          exposure == "pfdoa" ~ "PFDoDA*",
          exposure == "hexachlorobenzene" ~ "HCB",
          exposure == "dde" ~ "DDE",
          exposure == "ocs" ~ "Sum of OC Compounds",
          exposure == "pbde_154" ~  "PBDE-154", 
          exposure == "pbde_47" ~   "PBDE-47" ,
          exposure == "pbde_100" ~  "PBDE-100*", 
          exposure == "pbde_153" ~  "PBDE-153*",
          exposure == "pbde_85" ~   "PBDE-85*" , 
          exposure == "pcb_118" ~   "PCB-118*" ,
          exposure == "pcb_138" ~   "PCB-138*" , 
          exposure == "pcb_153" ~   "PCB-153*" ,
          exposure == "pcb_180" ~   "PCB-180*" ,
          exposure == "pcb_num" ~   "Number of detected PCB's"   , 
          exposure == "pbde_num" ~    "Number of detected PBDE's",
          TRUE ~ toupper(exposure)) %>% 
            as.factor() %>% 
            forcats::fct_relevel(., 
                                 "PFOS", "PFOA", "PFHxS", 
                                 "PFNA", "PFHpS","PFDA", 
                                 "PFPeS", "PFHpA",
                                 "N-MeFOSAA-b*", "N-EtFOSAA*",
                                 "PFDS*", "PFBS*", 
                                 "8:2 FTS*", 
                                 "PFDoDA*", 
                                 "PFUnDA*",
                                 "PFNS*",
                                 "PFHxA*", 
                                 "HCB","DDE",
                                 "Sum of OC Compounds",
                                 "PBDE-154", "PBDE-47","PBDE-100*", "PBDE-153*","PBDE-85*", 
                                 "PCB-118*","PCB-138*", "PCB-153*","PCB-180*",
                                 "Number of detected PCB's", "Number of detected PBDE's"))
    ) 
  }
  
  if(arrange_by_class == TRUE){ 
    suppressWarnings(
      
      exposure2 <-  exposure2 %>% 
        dplyr::mutate(exposure2 = 
                        forcats::fct_relevel(
                          exposure2, 
                          #PFSA's
                          "PFOS", 
                          "PFHxS",
                          "PFHpS",
                          "PFPeS",
                          "PFDS*",
                          "PFBS*", 
                          "PFNS*",
                          "8:2 FTS*", 
                          # PFCA's
                          "PFOA", 
                          "PFNA", 
                          "PFDA", 
                          "PFHpA", 
                          "PFDoDA*", 
                          "PFUnDA*",
                          "PFHxA*", 
                          "N-MeFOSAA-b*",
                          "N-EtFOSAA*",
                          # OCs
                          "HCB","DDE",
                          "Sum of OC Compounds",
                          #PCBs
                          "PCB-118*","PCB-138*", "PCB-153*","PCB-180*",
                          # PBDEs
                          "PBDE-154", "PBDE-47","PBDE-100*", "PBDE-153*","PBDE-85*", 
                          "Number of detected PCB's", "Number of detected PBDE's")))
  }
  
  return(exposure2$exposure2)
}

rename_exposures(c("pfoa", "pfos"))  
