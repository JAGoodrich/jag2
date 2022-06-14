#' FAST OWAS Function
#'
#' @import data.table
#' @export 
#' 
#' @param df Dataset
#' @param var Name of the variable of interest- this is usually either an exposure 
#' variable or an outcome variable 
#' @param omics Names of all omics features in the dataset 
#' @param covars Names of covariates (can be NULL)
#' @param var_exposure_or_outcome Is the variable of interest an exposure 
#' (independent variable) or outcome (dependent variable)? Must be either
#' "exposure" or "outcome"
#' @param model_type "linear" for linear models (via lm) or "logistic" for 
#' logistic (via glm) 
#' 
# Function
owas <- compiler::cmpfun(
  function(df, 
           var,
           omics, 
           covars,
           var_exposure_or_outcome, 
           model_type = "linear"){
    
    # Change data frame to data table for speed
    df <- data.table(df)
    
    # Pivot longer  
      dt_l = melt.data.table(
        df,
        id.vars = c(var, covars),
        measure.vars = omics,
        variable.name = "feature_name",
        value.name = "feature_value"
      )
    
    # Set formula for model ------------------
    # depending on whether variable of interest is the exposure or the outcome 
    if(var_exposure_or_outcome == "exposure"){
      # If variable is exposure: 
      if(is.null(covars)){
        mod_formula <- paste0("feature_value~", var)
      } else {
        mod_formula <- paste0("feature_value~", 
                              paste0(covars, collapse = "+"), "+",
                              var)
      } 
      
    } else if(var_exposure_or_outcome == "outcome"){
      # If variable is outcome: 
      if(is.null(covars)){
        mod_formula <- paste0(var, "~ feature_value")
      } else{
        mod_formula <- paste0(var, "~", 
                              paste0(covars, collapse = "+"),
                              "+feature_value")
      }
      
    } else {
      stop("var_exposure_or_outcome must be either \"exposure\" or \"outcome\" ")
    }
    
    
    # Run models -------------------------
    if(model_type == "linear"){
      # Linear models:
      res <- dt_l[, 
                  {fit <- lm(mod_formula, data = .SD) 
                  coef(summary(fit))[nrow(coef(summary(fit))), # Select last row
                                     c(1, 2, 4)] # Select Estimate, Std Error, and p_val
                  }, 
                  by = feature_name]
      
      # Add column for estimate 
      res <- cbind(res, c("estimate", "se", "p_value"))
      
      # Pivot wider
      final_results <- dcast(data = res, 
                             feature_name ~ V2, 
                             value.var = "V1")[,c(1, 2, 4, 3)]
      
    } else if(model_type == "logistic"){
      # Logistic models
      res <- dt_l[, 
                  {fit <- glm(mod_formula, data = .SD, family=binomial(link='logit')) 
                  coef(summary(fit))[nrow(coef(summary(fit))), # Select last row
                                     c(1, 2, 4)] # Select Estimate, Std Error, and p_val
                  }, 
                  by = feature_name]
      
      # Add column for estimate 
      res <- cbind(res, c("estimate", "se", "p_value"))
      
      # Pivot wider
      final_results <- dcast(data = res, 
                             feature_name ~ V2, 
                             value.var = "V1")[,c(1, 2, 4, 3)]
      
    } else {
      stop("model_type must be either \"linear\" or \"logistic\" ")
    }
    
    # Calculate adjusted p value
    final_results$adjusted_pval = p.adjust(final_results$p_value, method = "fdr")
    
    final_results$threshold = ifelse(final_results$adjusted_pval < 0.05, 
                                     "Significant",
                                     "Non-significant")
    
    return(final_results)
    
  }
)
