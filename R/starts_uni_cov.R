## File Name: starts_uni_cov.R
## File Version: 0.35

starts_uni_cov <- function(W , var_trait , var_ar , var_state , a , time_index = NULL ,
    add_meas_error = NULL )
{
    if ( is.null(time_index) ){
        time_index <- 1:W
    }
    covmat <- starts_rcpp_starts_uni_cov( var_trait=var_trait, var_ar=var_ar, 
                    var_state=var_state, a=a, time_index=time_index )
    if ( ! is.null(add_meas_error) ){
        if (is.vector(add_meas_error)){
            diag(covmat) <- diag(covmat) + add_meas_error                        
        } else {
            covmat <- covmat + add_meas_error
        }
    }                    
    return(covmat)
}

