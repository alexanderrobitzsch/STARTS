## File Name: starts_uni_estimate_prepare_fitting_prior_variance.R
## File Version: 0.071


starts_uni_estimate_prepare_fitting_prior_variance <- function( prior_var, sd0)
{
    vec <- round( c(NA, prior_var[1], prior_var[2] * sd0 ), 4 )
    vec <- as.list(vec)
    res <- list( 'digamma2', vec )
    return(res)
}
