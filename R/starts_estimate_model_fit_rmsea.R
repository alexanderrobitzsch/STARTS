## File Name: starts_estimate_model_fit_rmsea.R
## File Version: 0.01
## File Last Change: 2017-08-26 22:14:00


starts_estimate_model_fit_rmsea <- function( deviance, deviance_saturated, df_sem, nobs )
{
	chisq <- deviance - deviance_saturated
	p_chisq <- stats::pchisq( chisq, df = df_sem , lower.tail=FALSE )	
	rmsea <- sqrt( max( chisq - df_sem , 0) / df_sem / ( nobs - 1 ) )
	#--- output
	res <- list(chisq=chisq, p_chisq=p_chisq, df_sem=df_sem, rmsea=rmsea)
	return(res)
}
	
