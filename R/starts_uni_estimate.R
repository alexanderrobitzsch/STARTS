## File Name: starts_uni_estimate.R
## File Version: 0.17
## File Last Change: 2017-08-27 15:10:34


starts_uni_estimate <- function( data=NULL, covmat=NULL, nobs=NULL, estimator="ML", 
        pars_inits = NULL , prior_var_trait = c(3,.33), prior_var_ar=c(3,.33), prior_var_state=c(3,.33), 
		prior_a=c(3,.5), est_var_trait=TRUE, est_var_ar=TRUE, est_var_state=TRUE, var_meas_error = 0,
		constraints=TRUE, time_index=NULL, type="stationary" )
{
	CALL <- match.call()
	time <- list( "start" = Sys.time() )

	#--- data input processing
	res <- starts_uni_estimate_proc( data=data, time_index=time_index, covmat=covmat, 
				pars_inits=pars_inits, nobs=nobs, est_var_trait=est_var_trait, 
				est_var_ar=est_var_ar, est_var_state=est_var_state, 
				var_meas_error=var_meas_error ) 
	W <- res$W
	pars_inits <- res$pars_inits
	time_index <- res$time_index
	covmat <- res$covmat
	data0 <- res$data0
	par_names <- res$par_names
	nobs <- res$nobs
	pars_est <- res$pars_est
	df_sem <- res$df_sem
	par_names_est <- res$par_names_est
	sd0 <- res$sd0
	
	#--- define input data for the STARTS model
	data <- list( "S" = covmat , "M" = rep(0,W) , "n" = nobs , "W" = W  )
	#--- define likelihood function for the STARTS model
	ll_model <- function( pars , data ){ 
		Sigma <- starts_uni_cov_pars(W=W, pars=pars, pars_est=pars_est, time_index=time_index)
		ll <- LAM::loglike_mvnorm( S = data$S , Sigma = Sigma , M = data$M , mu = data$M ,
					n = data$n , lambda = 1E-10)
		return(ll)
	}

	#--- lower and upper bounds for parameters
	res <- starts_uni_estimate_prepare_fitting( pars_est, constraints, estimator,
	               prior_var_trait, prior_var_ar, prior_var_state, prior_a ,sd0)
	pars_lower <- res$pars_lower
	pars_upper <- res$pars_upper	
	constraints <- res$constraints
	prior_model <- res$prior_model
	
	#--- estimate model	
	LAM_fct <- LAM::pmle
	LAM_args <- list( data=data, nobs= data$n, pars=pars_inits, model = ll_model ,  prior=prior_model , 
							pars_lower = pars_lower, pars_upper = pars_upper , 
							method = "L-BFGS-B", verbose=FALSE ) 	
	fit_LAM <- res <- do.call( what=LAM_fct, args = LAM_args )

	deviance <- fit_LAM$deviance
	coef <- res$coef
	vcov <- res$vcov
	
	#--- saturated fit
	ll_saturated <- LAM::loglike_mvnorm( S = data$S , Sigma = data$S , M = data$M , mu = data$M ,
						n = data$n , lambda = 1E-10)	
	deviance_saturated <- -2*ll_saturated				
	
	#--- fitted covariance matrix					
	covmat_fitted <- starts_uni_cov_pars(W=W, pars=coef, pars_est=pars_est, time_index=time_index)					
					
	#--- model fit
	model_fit <- starts_estimate_model_fit( covmat=covmat, covmat_fitted=covmat_fitted, deviance=deviance, 
						deviance_saturated=deviance_saturated, df_sem=df_sem, nobs=nobs ) 

	#--- description
	res <- starts_estimate_description(estimator=estimator)
	description <- res$description
	used_function <- res$used_function
	
	#--- output management
	deviance <- fit_LAM$deviance
	ic <- fit_LAM$ic		
	time$end <- Sys.time()
	
	#--- output
	res <- list( coef=coef, vcov=vcov, deviance=deviance, ic = ic ,
					model_fit=model_fit, covmat_fitted=covmat_fitted, fit_LAM=fit_LAM, data0=data0, 
					pars_inits=pars_inits, pars_lower=pars_lower, pars_upper=pars_upper,
					estimator=estimator, description=description, used_function=used_function,
					constraints=constraints,
					time=time, CALL=CALL)
	class(res) <- "starts_uni"
	return(res)
}
