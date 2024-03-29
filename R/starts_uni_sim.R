## File Name: starts_uni_sim.R
## File Version: 0.142


#-- STARTS unidimensional model: simulation
starts_uni_sim <- function( N, W, var_trait, var_ar, var_state, a, time_index=NULL )
{
    # var_state <- var_disturbance / ( 1 - a^2 )
    #  Var(S)=a^2 Var(S) + Var(U)
    #=> Var(U)=(1-a^2)*Var(S)
    #--- renaming input arguments
    var_error <- var_state
    var_state <- var_ar
    #---
    var_disturbance <- (1-a^2)*var_state
    matr <- matrix( NA, nrow=N, ncol=W )
    colnames(matr) <- paste0('W', 1L:W)
    states <- matr
    trait <- stats::rnorm( N, sd=sqrt( var_trait ))
    ww <- 1
    states[,ww] <- stats::rnorm( N, sd=sqrt( var_state ))
    for ( ww in 2L:W){
        states[,ww] <- a*states[,ww-1] + stats::rnorm( N, sd=sqrt(var_disturbance))
    }
    matr <- trait + states + stats::rnorm( N*W, sd=sqrt( var_error ))
    if ( ! is.null(time_index) ){
        matr <- matr[, time_index ]
    }
    #--- output
    return(matr)
}
