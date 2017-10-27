## File Name: starts_estimate_model_fit.R
## File Version: 0.04

starts_estimate_model_fit <- function( covmat, covmat_fitted, deviance, deviance_saturated, df_sem, nobs )
{
	#--- SRMR
	srmr <- starts_estimate_model_fit_srmr( covmat=covmat, covmat_fitted=covmat_fitted ) 
	#--- RMSEA	
	res <- starts_estimate_model_fit_rmsea( deviance=deviance, deviance_saturated=deviance_saturated, 
					df_sem=df_sem, nobs=nobs ) 
	chisq <- res$chisq
	p_chisq <- res$p_chisq
	df_sem <- res$df_sem
	rmsea <- res$rmsea
	#--- output
	res <- list( SRMR = srmr , chisq = chisq , df_sem = df_sem, p_chisq=p_chisq, RMSEA=rmsea  )
	return(res)
}
