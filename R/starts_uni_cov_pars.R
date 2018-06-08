## File Name: starts_uni_cov_pars.R
## File Version: 0.06

starts_uni_cov_pars <- function(W, pars, pars_est, time_index, add_meas_error=NULL)
{
    pars0 <- pars
    pars <- 0*pars_est
    pars[ pars_est ] <- pars0     
    res <- starts_uni_cov( W=W , var_trait=pars["var_trait"], var_ar=pars["var_ar"], 
                    var_state=pars["var_state"] , a=pars["a"], 
                    time_index=time_index, add_meas_error=add_meas_error )
    return(res)
}
