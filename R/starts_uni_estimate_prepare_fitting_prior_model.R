## File Name: starts_uni_estimate_prepare_fitting_prior_model.R
## File Version: 0.04
## File Last Change: 2017-08-27 13:57:25


starts_uni_estimate_prepare_fitting_prior_model <- function( pars_est, 
	prior_var_trait, prior_var_ar, prior_var_state, prior_a , sd0, estimator)
{
	prior_model <- NULL
	if ( estimator != "ML"){
		prior_model <- list()
		prior_model[[ "var_trait" ]] <- starts_uni_estimate_prepare_fitting_prior_variance( 
											prior_var=prior_var_trait, sd0=sd0)
		prior_model[[ "var_ar" ]] <- starts_uni_estimate_prepare_fitting_prior_variance( 
											prior_var=prior_var_ar, sd0=sd0)
		prior_model[[ "var_state" ]] <- starts_uni_estimate_prepare_fitting_prior_variance( 
											prior_var=prior_var_state, sd0=sd0)		
		vec <- round( c(NA, (prior_a[1]+1)*prior_a[2] , (prior_a[1]+1)*(1-prior_a[2] )  ), 4 )
		prior_model[[ "a" ]] <- list( "dbeta" , as.list(vec) )
		pars_remove <- names(pars_est)[ ! pars_est ]
		if ( length(pars_remove) > 0 ){
			for (pp in pars_remove){
				prior_model[[pp]] <- NULL
			}
		}
	}
	return(prior_model)
}
