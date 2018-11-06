## File Name: starts_estimate_description.R
## File Version: 0.04

starts_estimate_description <- function(estimator)
{
    if (estimator=="PML"){
        description <- paste0( "Penalized Maximum Likelihood Estimation \n ",
                "   (Maximum Posterior Estimation, MAP)" )
    }
    if (estimator=="ML"){
        description <- paste0( "Maximum Likelihood Estimation" )
    }
    if (estimator=="MCMC"){
        description <- paste0( "Markov Chain Monte Carlo Estimation" )
    }
    if (estimator %in% c("ML","PML")){
        used_function <- "LAM::pmle"
    }
    if (estimator %in% c("MCMC")){
        used_function <- "LAM::amh"
    }

    #--- output
    res <- list(description=description, used_function=used_function)
    return(res)
}
