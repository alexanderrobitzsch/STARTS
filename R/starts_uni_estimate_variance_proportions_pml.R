## File Name: starts_uni_estimate_variance_proportions_pml.R
## File Version: 0.02

starts_uni_estimate_variance_proportions_pml <- function( coef, vcov, vars )
{	
	NV <- length(vars)
	parm_fct <- function(x){		
		tot <- sum( x[vars] )
		res <- x[vars] / tot
		return(res)
	}
	# define point for evaluating partial derivatives            
	par <- coef
	val <- parm_fct(x=par)

	#--- compute gradient 
	NC <- length(par)
	A <- matrix( 0, nrow=NV , ncol=NC)
	rownames(A) <- vars
	colnames(A) <- names(par)
	indices <- match( vars, colnames(A) )
	for (cc in indices){
		res <- CDM::numerical_Hessian_partial( par = par , FUN = parm_fct , coordinate = cc )
		A[,cc] <- res$grad
	}
	vcov_prop <- A %*% vcov %*% t(A)
	var_prop <- data.frame( parm = vars, est = val, se = sqrt( diag(vcov_prop) ) )
	var_prop$t <- var_prop$est / var_prop$se
	var_prop$p <- 2* stats::pnorm( - abs( var_prop$t ) )
	return(var_prop)
}
