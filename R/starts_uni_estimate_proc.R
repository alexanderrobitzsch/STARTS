## File Name: starts_uni_estimate_proc.R
## File Version: 0.11

starts_uni_estimate_proc <- function( data, time_index, covmat, pars_inits , nobs, 
		est_var_trait,	est_var_ar, est_var_state, var_meas_error )
{
	#--- data processing
	data0 <- data
	some_missings <- FALSE
	suff_stat <- NULL
	# handle data input
	if ( ! is.null(data) ){		
		nobs <- nrow(data)
		if ( sum( is.na(data) ) > 0 ){
			some_missings <- TRUE
			suff_stat <- LAM::suff_stat_NA_pattern(data) 
		}
		data <- stats::na.omit(data)
		covmat <- stats::cov(data)		
		M <- colMeans(data0, na.rm=TRUE)
	} else {
		M <- rep(0, ncol(covmat) )
	}
	diag(covmat) <- diag(covmat) - var_meas_error
	W <- ncol(covmat)	
	
	if (! is.null(time_index)){
		time_index <- cumsum( c(0, diff(time_index)) ) + 1
	}
	#--- average standard deviation
	sd0 <- mean( sqrt( diag(covmat) ) )	
	
	#--- estimated parameters
	par_names <- c( paste0("mu",1:W), c("var_trait", "var_ar", "var_state", "a") )
	est_pars <- c( rep(TRUE, W) , c( est_var_trait,	est_var_ar, est_var_state , TRUE )	)
	pars_est <- starts_vector_with_names( est_pars , par_names )	
	if ( ! est_var_ar ){
		pars_est["a"] <- FALSE
	}
	
	#--- define initial parameters	
	if ( is.null(pars_inits) ){
		par1 <- c( rep(0,W), c( sd0*c( .33, .33, .33 ),.5) )
		pars_inits <- starts_vector_with_names( par1 , par_names )				
	}
	pars_inits <- pars_inits[ pars_est ]
	par_names_est <- par_names[ pars_est ]
		
	#-- degrees of freedom SEM
	covmat_np <- W*(W-1)/2 + W
	estimated_np <- est_var_trait + est_var_ar + est_var_state + 1
	df_sem <- covmat_np - estimated_np
		
	#----- output
	res <- list(W=W, pars_inits=pars_inits, time_index=time_index, covmat=covmat, M=M, data0=data0,
				par_names=par_names, nobs = nobs, sd0 = sd0, pars_est=pars_est, df_sem=df_sem,
				par_names_est=par_names_est, some_missings=some_missings, suff_stat=suff_stat)
	return(res)
}
