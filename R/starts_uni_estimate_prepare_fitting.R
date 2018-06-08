## File Name: starts_uni_estimate_prepare_fitting.R
## File Version: 0.12


starts_uni_estimate_prepare_fitting <- function( pars_est, constraints, estimator,
     prior_var_trait, prior_var_ar, prior_var_state, prior_a , sd0, pars_inits, W)
{
    if ( estimator != "ML"){
        constraints <- TRUE
    }        
    
    par_names <- names(pars_est)
    
    #---- parameter constraints
    if (constraints){
        pars_lower <- starts_vector_with_names( c( rep(-Inf,W), rep(1e-4,4)), par_names )            
        pars_upper <- starts_vector_with_names( c( rep(Inf, W+3), 1), par_names )    
    } else {
        pars_lower <- pars_upper <- NULL
    }
    pars_lower <- pars_lower[ pars_est ]
    pars_upper <- pars_upper[ pars_est ]
    
    #--- define prior distributions
    prior_model <- starts_uni_estimate_prepare_fitting_prior_model( pars_est=pars_est, 
                        prior_var_trait=prior_var_trait, prior_var_ar=prior_var_ar, 
                        prior_var_state=prior_var_state, prior_a=prior_a, sd0=sd0, 
                        estimator=estimator ) 
                        
    #--- proposal SDs
    sd_prop <- .1
    pars0 <- ifelse( abs(pars_inits) < 1 , sd_prop, sd_prop*abs(pars_inits) )
    proposal_sd <- pars0 * rep(1, length(pars_inits) )    
                        
    #--------------------------------
    #--- output
    res <- list( pars_lower=pars_lower, pars_upper=pars_upper, constraints=constraints,
                    prior_model=prior_model, proposal_sd=proposal_sd )
    return(res)
}    
