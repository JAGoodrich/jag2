#' Generalized P-Act Function
#' 
#' P_act was designed for correction of GWAS analysis. Here, 
#' the code has been modified for the following generalized scenario:          
#' dpdt_var ~ ind_var + covariates      
#' Where:          
#'      dpdt_vars is a set of correlated outcomes/traits;       
#'      ind_vars is a set of potentially correlated exposures/metabolites/genes/etc..;        
#'      and covariates is a set of covariates for the analysis.           
#' This function can not handle more than 1,000 corrections. 
#' 
#' @import tidyverse MASS mvtnorm
#' @export
#' @param p_values Data frame with 3 columns. File should contain one row for 
#' each test. Missingness is not allowed. 
#'  
#'   Column 1: dpdt_var names (outcomes)   
#'    
#'   Column 2: ind_var names   
#'   
#'   Column 3: The p-value from each test    
#'   
#' @param  dpdt_vars traits.txt: 
#'   Not necessary if only one trait is considered.
#' One column for each trait. 
#' One row for each individual plus header row of trait names.  
#' If covariates are used, need trait values residualized on covariates.
#' Missing values coded as NA.  
#' Must have same # of individuals as metabolite.txt.
#' 
#' @param ind_vars metabolite.txt/outcomes.txt: 
#'   One column for each metabolite score. 
#' One row for each individual plus a header row of metabolite labels.  
#' Missing values must be coded as NA.
#' Not necessary if only one marker is considered.
#' 
#' @param covariates  Data frame with one column for each covariate, 
#' and one row for each individual. Column names should be the names of the 
#' covariates. Not necessary if no covariates used; Not necessary if only 
#' one independent variable is considered. Missing values are ok.
#' Must have same # of individuals as @ind_vars.
#' 
#' @param alpha Used to determine when to stop sequential testing. 
#' If not specified, default value of .05 is used.
#'  
#' @returns a data frame containing 4 columns: y_vars, x_vars, original p values, 
#' and p-act corrected p values.  
#' 
p_act <- function(p_values, x_vars, y_vars = NA, covariates, alpha = 0.05){ 
  err=0  #Parameter to keep track of error in input files
  
  # These parameters affect precision of p-values and speed of estimates.  The initial settings request
  # increased precision for values of p_ACT <.05, <.01, and <.00001 but may be altered as desired.
  # Use 25000 for quick, reasonably precise p-values.  Use 25000000 for ultra-precise but slower p-values.
  level1=25000; cutoff2=.05; level2=25000; cutoff3=.05; level3=25000; cutoff4=.05; level4=25000
  
  # Step 1: Get residuals of y_vars ~ covariates for the y_vars data-------
  covariate_temp <- covariates
  data = data.frame()
  # Run loop to residualilze each y_var
  for(i in names(y_vars)){
    covariate_temp <- covariates %>% 
      mutate(outcome = y_vars[[i]])
    
    temp_resid = lm(as.formula(paste0("outcome ~ ", 
                                      paste(names(covariates),collapse = " + "))), 
                    data = covariate_temp) %>% 
      resid()
    
    data[names(temp_resid), i] <- temp_resid 
  }
  
  y_vars = as.data.frame(data)
  
  # Step 2: run original p-act code with modifications where needed. 
  pvals <- p_values %>% as.data.frame()
  colnames(pvals) <- c("y_vars", "x_vars", "p_values")
  L=dim(pvals)[1]
  
  # mutate x_vars
  x_var_list <- pvals[1,2]
  M=corg=1  
  # Assign "G" (originally for gene) to the x-vars
  G<-x_vars %>% data.matrix()
  
  # N = sample size
  N=dim(G)[1]
  # M = Number of x_vars (exposures/metabolites/genes etc)
  M=dim(G)[2] 
  
  if (M>1) {
    #For covariance estimation, fill in missing values with mean metabolite code
    for (i in 1:M) { G[is.na(G[,i]),i]=mean(G[,i],na.rm=TRUE) }
    
    
    X=matrix(1,N)
    
    # Get covariate info
    covar <- covariates %>% data.matrix()
    
    if (dim(covar)[1]!=N) {
      cat("Error: Different # of individuals in x_vars and covariates\n")
      err=1
    } else{
      
      X=cbind(X,covar)
      #For covariance estimation, fill in missing values with variable mean
      for (i in 1:dim(X)[2]) { X[is.na(X[,i]),i]=mean(X[,i],na.rm=TRUE) }
    }
    
    #Computation of variance matrix of G, conditional on X
    x_var_list <- colnames(x_vars) %>% t() 
    kg=order(x_var_list)
    G=G[,kg]
    corg=cov2cor(t(G)%*%G - t(G)%*%X %*% ginv(t(X)%*%X) %*% t(X)%*%G)
  }
  
  
  
  # y_vars contains a row of K trait values for each individual, plus a header row containing trait
  # names, with missing values coded as NA.  If the model includes environmental covariates, trait
  # values should be residualized on covariates (see README.txt for more details).
  # If there is no y_vars file, it will be assumed that only one trait is considered
  y_var_list=pvals[1,1]
  K=cory=1
  
  if(!is.null(ncol(y_vars)) ) {
    Y <- y_vars
    if (exists("N")) {
      if (dim(Y)[1]!=N) {
        cat("Error: Different # of individuals in x_vars and y_vars\n")
        err=1
      }
    }
    if (!exists("N")) {N=dim(Y)[1]}
    # K = number of y variables/outcomes
    K <- dim(Y)[2]
    if (K>1) {
      #For covariance estimation, fill in missing values with trait mean
      for (i in 1:K) { Y[is.na(Y[,i]),i]=mean(Y[,i],na.rm=TRUE) }
      #Sort columns of Y into order by trait name, compute correlation matrix
      y_var_list <- colnames(y_vars) %>% t()
      kl=order(y_var_list)
      Y=Y[,kl]
      cory=cor(Y)
    }
  }
  
  if (K*M<L)  {
    cat("Error: More p-values than trait*metabolite combinations\n")
    err=1
  }
  
  
  # Now, actually run the correction!!
  
  if (err==0) {    #Condition on err==0
    
    #Computation of correlation matrix between K x M tests
    v=kronecker(cory,corg)
    
    #Removal of any missing tests from correlation matrix and alignment of p-values and covariance matrix
    glist=cbind(1,t(x_var_list))
    tlist=cbind(1,t(y_var_list))
    tests_km=merge(tlist,glist,by.x=1,by.y=1)[,2:3]
    all=merge(tests_km,pvals,by.x=c(1,2),by.y=c(1,2),all=TRUE)
    v=v[!is.na(all[,3]),
        !is.na(all[,3])]
    pvals=all[!is.na(all[,3]),]
    
    #Ordering of p-values, covariance matrix
    rank=order(pvals[,3])
    v_orig=v[rank,rank]
    ordered=pvals[rank,]
    #Oneside flag for presence of one-sided tests
    oneside=0
    if (dim(ordered)[2]>3) { oneside=1 }
    
    #Computation of p_ACT
    stop=0
    i=1
    v=v_orig
    while (stop==0) {
      minp=ordered[i,3]
      if (minp==0) {p_ACT=0}
      if (minp>=.5) {p_ACT=1}
      
      if (minp>0 & minp < .5) {
        lower=rep(qnorm(minp/2),(L+1-i))
        upper=rep(qnorm(1-minp/2),(L+1-i))
        if (oneside==1) {
          p4=ordered[(i+1):L,4]
          lower=rep(-Inf,(L+1-i))
          upper=rep(Inf,(L+1-i))
          lower[p4==2]=qnorm(minp/2)
          lower[p4==-1]=qnorm(minp)
          upper[p4==2]=qnorm(1-minp/2)
          upper[p4==1]=qnorm(1-minp)
        }
        
        # Use pmvnorm() function from mvtnorm package to compute multivariate normal probabilities
        # For more information, see:
        # Genz A, Bretz F, Hothorn T (2006) mvtnorm: Multivariate normal and t distribution. R package version 0.8-0
        # Genz A (1992) Numerical computation of multivariate normal probabilities.  J Comput Graph Stat 1:141-150
        # Genz A (1993) Comparison of methods for the computation of multivariate normal probabilities.  Comput Sci Stat 25:400-405
        
        # if (oneside==0) {ordered=cbind(ordered,0) }
        p_ACT=1-pmvnorm(lower=lower,upper=upper,sigma=v,maxpts=level1,abseps=.0000000000001)
        if (p_ACT<cutoff2) {
          p_ACT=1-pmvnorm(lower=lower,upper=upper,sigma=v,maxpts=level2,abseps=.0000000000001)
          if (p_ACT<cutoff3) {
            p_ACT=1-pmvnorm(lower=lower,upper=upper,sigma=v,maxpts=level3,abseps=.0000000000001)
            if (p_ACT<cutoff4) {
              p_ACT=1-pmvnorm(lower=lower,upper=upper,sigma=v,maxpts=level4,abseps=.0000000000001)
            }
          }
        } 
      }
      ordered[i,4]=format(p_ACT,digits=5)
      if (p_ACT >= alpha | i>=L) {
        stop=1
      }
      if (p_ACT < alpha & i<L) {
        v=v[-1,-1]
        i=i+1
      }
    }
    if (i<L) {ordered[(i+1):L,4]=NA}
    
    colnames(ordered) <- c("y_vars","x_vars","p","p_act")
  }
  return(ordered)
}
