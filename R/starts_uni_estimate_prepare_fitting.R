## File Name: starts_uni_estimate_prepare_fitting.R
## File Version: 0.03
## File Last Change: 2017-08-27 13:39:18


starts_uni_estimate_prepare_fitting <- function( pars_est, constraints, estimator,
	 prior_var_trait, prior_var_ar, prior_var_state, prior_a , sd0)
{
	if ( estimator != "ML"){
		constraints <- TRUE
	}

	#---- parameter constraints
	if (constraints){
		pars_lower <- starts_vector_with_names( rep(1e-4,4) , names(pars_est) )			
		pars_upper <- starts_vector_with_names( c(Inf, Inf, Inf, 1) , names(pars_est) )	
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

	#--------------------------------
	#--- output
	res <- list( pars_lower=pars_lower, pars_upper=pars_upper, constraints=constraints,
					prior_model=prior_model )
	return(res)
}	
