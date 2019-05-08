## File Name: starts_uni_estimate_proc.R
## File Version: 0.443

starts_uni_estimate_proc <- function( data, time_index, covmat, pars_inits, nobs,
        est_var_trait,    est_var_ar, est_var_state, var_meas_error )
{
    #--- data processing
    data0 <- data
    some_missings <- FALSE
    suff_stat <- NULL

    #-- clean time_index
    if ( ! is.null(time_index) ){
        time_index <- time_index - time_index[1] + 1
    }
    # handle data input
    if ( ! is.null(data) ){
        nobs <- nrow(data)
        if ( sum( is.na(data) ) > 0 ){
            some_missings <- TRUE
            suff_stat <- LAM::suff_stat_NA_pattern(dat=data)
            # suff_stat <- starts_uni_estimate_proc_modify_suff_stat( suff_stat, var_meas_error)
        }
        data <- stats::na.omit(data)
        covmat <- stats::cov(data)
        M <- colMeans(data0, na.rm=TRUE)
        W <- ncol(data)
    } else {
        M <- rep(0, ncol(covmat) )
        W <- ncol(covmat)
    }
    if ( sum(var_meas_error)==0 ){
        if ( is.matrix(var_meas_error) | is.data.frame(var_meas_error) ){
            var_meas_error <- as.numeric( var_meas_error[1,,drop=TRUE] )
        }
        if ( length(var_meas_error)    ==1 ){
            var_meas_error <- rep( var_meas_error, W)
        }
    }
    if ( is.matrix(var_meas_error) | is.data.frame(var_meas_error) ){
        if ( ( nrow(var_meas_error)==1) & (ncol(var_meas_error)> 1) ){
            var_meas_error <- as.vector(as.numeric(var_meas_error[1,]))
        }
    }

    if (! is.null(time_index)){
        time_index <- cumsum( c(0, diff(time_index)) ) + 1
    }
    #--- average standard deviation
    if ( ! some_missings){
        if (is.data.frame(covmat)){
            covmat <- as.matrix(covmat)
        }
        sd0 <- mean( sqrt( diag(covmat) - var_meas_error )  )
    } else {
        sd0 <- mean( apply( data0, 2, stats::sd, na.rm=TRUE ) )
    }
    #--- estimated parameters
    par_names <- c( paste0("mu",1:W), c("var_trait", "var_ar", "var_state", "a") )
    est_pars <- c( rep(TRUE, W), c( est_var_trait,    est_var_ar, est_var_state, TRUE )    )
    pars_est <- starts_vector_with_names( est_pars, par_names )
    if ( ! est_var_ar ){
        pars_est["a"] <- FALSE
    }

    #--- define initial parameters
    if ( is.null(pars_inits) ){
        par1 <- c( M, c( sd0*c( .33, .33, .33 ),.5) )
        pars_inits <- starts_vector_with_names( par1, par_names )
    }
    pars_inits <- pars_inits[ pars_est ]
    par_names_est <- par_names[ pars_est ]

    #-- degrees of freedom SEM
    covmat_np <- W*(W-1)/2 + W
    estimated_np <- est_var_trait + est_var_ar + est_var_state + 1
    df_sem <- covmat_np - estimated_np

    if ( is.null(time_index) ){
        time_index <- 1:W
    }
    time_index_lags <- sort(unique( as.vector( abs( outer( time_index, time_index, "-" ) ) ) ))

    #----- output
    res <- list(W=W, pars_inits=pars_inits, time_index=time_index, covmat=covmat, M=M, data0=data0,
                par_names=par_names, nobs=nobs, sd0=sd0, pars_est=pars_est, df_sem=df_sem,
                par_names_est=par_names_est, some_missings=some_missings, suff_stat=suff_stat,
                var_meas_error=var_meas_error, time_index_lags=time_index_lags)
    return(res)
}
