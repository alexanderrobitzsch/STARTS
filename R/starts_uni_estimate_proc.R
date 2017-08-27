## File Name: starts_uni_estimate_proc.R
## File Version: 0.03
## File Last Change: 2017-08-26 22:55:53

starts_uni_estimate_proc <- function( data, time_index, covmat, pars_inits , nobs, 
		est_var_trait,	est_var_ar, est_var_state, var_meas_error )
{
	#--- data processing
	data0 <- data
	# handle data input
	if ( ! is.null(data) ){
		if ( sum( is.na(data) ) > 0 ){
			warning("Function 'starts_uni_estimate' uses listwise deletion to handle missing data.\n")	
		}
		data <- na.omit(data)
		covmat <- stats::cov(data)			
	}
	diag(covmat) <- diag(covmat) - var_meas_error
	
	
	if (! is.null(time_index)){
		time_index <- cumsum( c(0, diff(time_index)) ) + 1
	}
	#--- average standard deviation
	sd0 <- mean( sqrt( diag(covmat) ) )	
	
	#--- estimated parameters
	par_names <- c("var_trait", "var_ar", "var_state", "a")	
	pars_est <- starts_vector_with_names( c( est_var_trait,	est_var_ar, est_var_state , TRUE ) , par_names )	
	if ( ! est_var_ar ){
		pars_est["a"] <- FALSE
	}
	
	#--- define initial parameters	
	if ( is.null(pars_inits) ){
		pars_inits <- starts_vector_with_names( c( sd0*c( .33, .33, .33 ),.5), par_names )				
	}
	pars_inits <- pars_inits[ pars_est ]
	par_names_est <- par_names[ pars_est ]
	
	W <- ncol(covmat)
	#-- degrees of freedom SEM
	covmat_np <- W*(W-1)/2 + W
	estimated_np <- est_var_trait + est_var_ar + est_var_state + 1
	df_sem <- covmat_np - estimated_np
	
	
	#----- output
	res <- list(W=W, pars_inits=pars_inits, time_index=time_index, covmat=covmat, data0=data0,
				par_names=par_names, nobs = nobs, sd0 = sd0, pars_est=pars_est, df_sem=df_sem,
				par_names_est=par_names_est)
	return(res)
}
