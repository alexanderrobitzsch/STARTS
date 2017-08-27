## File Name: starts_uni_estimate_prepare_fitting_prior_variance.R
## File Version: 0.04
## File Last Change: 2017-08-27 13:51:46


starts_uni_estimate_prepare_fitting_prior_variance <- function( prior_var, sd0)
{
	vec <- round( c(NA, prior_var[1], prior_var[2] * sd0 ) , 4 )
	vec <- as.list(vec)
	res <- list( "dinvgamma2" , vec )
	return(res)
}
