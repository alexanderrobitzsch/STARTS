## File Name: starts_uni_estimate_variance_proportions_pml.R
## File Version: 0.23

starts_uni_estimate_variance_proportions_pml <- function( coef, vcov, vars )
{
    NV <- length(vars)
    parm_fct <- function(x){
        tot <- sum(x)
        res <- x / tot
        return(res)
    }
    # define point for evaluating partial derivatives
    par <- coef[vars]
    val <- parm_fct(x=par)

    #--- compute gradient
    indices1 <- match( vars, names(coef) )
    vcov <- vcov[indices1, indices1]
    A <- CDM::numerical_gradient(par=par, FUN=parm_fct)
    vcov_prop <- A %*% vcov %*% t(A)
    var_prop <- data.frame( parm=vars, est=val, se=sqrt( diag(vcov_prop) ) )
    var_prop$t <- var_prop$est / var_prop$se
    var_prop$p <- 2*stats::pnorm( - abs( var_prop$t ) )
    return(var_prop)
}
