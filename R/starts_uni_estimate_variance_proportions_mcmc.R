## File Name: starts_uni_estimate_variance_proportions_mcmc.R
## File Version: 0.05

starts_uni_estimate_variance_proportions_mcmc <- function( fit_LAM, vars )
{
    mcmcobj <- fit_LAM$mcmcobj
    mcmcobj2 <- mcmcobj[, vars ] / rowSums( mcmcobj[,vars] )
    var_prop <- sirt::mcmc_summary(mcmcobj2)
    return(var_prop)
}
