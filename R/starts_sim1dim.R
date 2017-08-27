## File Name: starts_sim1dim.R
## File Version: 0.06
## File Last Change: 2017-08-26 20:25:12

#######################################################
# STARTS unidimensional model: simulation 
starts_sim1dim <- function( N , W , var_trait , 
	   var_ar , var_state , a )
{
	starts_deprecated( old="starts_sim1dim", new="starts_uni_sim" )	
	res <- starts_uni_sim( N=N , W=W , var_trait=var_trait , var_ar=var_ar , 
				var_state=var_state , a=a )
	return(res)
}
