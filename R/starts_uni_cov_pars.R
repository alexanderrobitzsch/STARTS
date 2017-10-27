## File Name: starts_uni_cov_pars.R
## File Version: 0.01

starts_uni_cov_pars <- function(W, pars, pars_est, time_index)
{
	pars0 <- pars
	pars <- 0 + 0*pars_est
	pars[ pars_est ] <- pars0 	
	res <- starts_uni_cov( W=W , var_trait=pars["var_trait"], var_ar=pars["var_ar"], 
					var_state=pars["var_state"] , a=pars["a"], 
					time_index=time_index )
	return(res)
}
