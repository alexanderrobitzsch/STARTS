## File Name: starts_cov.R
## File Version: 1.01
## File Last Change: 2017-08-25 11:25:00


starts_cov <- function(W , var_trait , var_ar , var_state , a )
{
	starts_deprecated( old="starts_cov", new="starts_uni_cov" )	
	res <- starts_uni_cov(W=W, var_trait=var_trait, var_ar=var_ar, var_state=var_state, 
				a=a)
	return(res)
}
