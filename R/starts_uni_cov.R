## File Name: starts_uni_cov.R
## File Version: 0.15

starts_uni_cov <- function(W , var_trait , var_ar , var_state , a , time_index = NULL ,
    add_meas_error = NULL )
{
    if ( ! is.null(time_index) ){
        W <- max(time_index)
    }

    covmat <- matrix( var_trait , nrow=W , ncol=W )          
    for (ii in 1:W){
        covmat[ii,ii] <- covmat[ii,ii] + var_state
    }
    matr <- 0*covmat
    for (ii in 1:W){
        for (jj in 1:W){
            matr[ii,jj] <- a^( abs(ii-jj) ) * var_ar 
        }
    }
    covmat <- covmat + matr
    
    if ( ! is.null(time_index) ){
        covmat <- covmat[ time_index , time_index ]
    }    
    
    if ( ! is.null(add_meas_error) ){
        diag(covmat) <- diag(covmat) + add_meas_error
    }        
    return(covmat)
}

