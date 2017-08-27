## File Name: starts_estimate_description.R
## File Version: 0.02
## File Last Change: 2017-08-27 13:09:54

starts_estimate_description <- function(estimator)
{
	if (estimator=="PML"){
		description <- paste0( "Penalized Maximum Likelihood Estimation \n "	,
			  "   (Maximum Posterior Estimation, MAP)" )
	}
	if (estimator=="ML"){
		description <- paste0( "Maximum Likelihood Estimation" )
	}
	if (estimator %in% c("ML","PML")){
		used_function <- "LAM::pmle"
	}
	#--- output
	res <- list(description=description, used_function=used_function)
	return(res)
}
