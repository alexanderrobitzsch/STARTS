## File Name: starts_uni_estimate.R
## File Version: 0.897


starts_uni_estimate <- function( data=NULL, covmat=NULL, nobs=NULL, estimator="ML",
        pars_inits=NULL, prior_var_trait=c(3,.33), prior_var_ar=c(3,.33), prior_var_state=c(3,.33),
        prior_a=c(3,.5), est_var_trait=TRUE, est_var_ar=TRUE, est_var_state=TRUE, var_meas_error=0,
        constraints=TRUE, time_index=NULL, type="stationary",
        n.burnin=5000, n.iter=20000, verbose=FALSE, optim_fct="optim", use_rcpp=TRUE )
{
    CALL <- match.call()
    time <- list( "start"=Sys.time() )

    #--- data input processing
    res <- starts_uni_estimate_proc( data=data, time_index=time_index, covmat=covmat,
                pars_inits=pars_inits, nobs=nobs, est_var_trait=est_var_trait,
                est_var_ar=est_var_ar, est_var_state=est_var_state,
                var_meas_error=var_meas_error )
    W <- res$W
    pars_inits <- res$pars_inits
    time_index <- res$time_index
    covmat <- res$covmat
    M <- res$M
    data0 <- res$data0
    par_names <- res$par_names
    nobs <- res$nobs
    pars_est <- res$pars_est
    df_sem <- res$df_sem
    par_names_est <- res$par_names_est
    sd0 <- res$sd0
    some_missings <- res$some_missings
    suff_stat <- res$suff_stat
    var_meas_error <- res$var_meas_error
    time_index_lags <- res$time_index_lags

    #--- define input data for the STARTS model
    data <- list( "S"=covmat, "M"=M, "n"=nobs, "W"=W, suff_stat=suff_stat )

    #--- define likelihood function for the STARTS model
    ind_M <- 1:W
    ind_S <- seq(W+1,W+4)

    if (some_missings){
        loglike_fct <- LAM::loglike_mvnorm_NA_pattern
    } else {
        loglike_fct <- LAM::loglike_mvnorm
    }

    add_meas_error <- var_meas_error

    ll_model <- function( pars, data ){
        pars_M <- pars[ ind_M ]
        pars_S <- pars[ ind_S ]
        Sigma <- starts_uni_cov_pars(W=W, pars=pars_S, pars_est=pars_est[ind_S],
                        time_index=time_index, add_meas_error=add_meas_error)
        loglike_args <- list( Sigma=Sigma, mu=pars_M, lambda=1E-10, use_rcpp=use_rcpp)
        if ( ! some_missings ){
            loglike_args$S <- data$S
            loglike_args$M <- data$M
            loglike_args$n <- data$n
        }
        if ( some_missings ){
            loglike_args$suff_stat <- data$suff_stat
        }
        ll <- do.call( what=loglike_fct, args=loglike_args)
        return(ll)
    }

    #--- lower and upper bounds for parameters
    res <- starts_uni_estimate_prepare_fitting( pars_est=pars_est,
                constraints=constraints, estimator=estimator, prior_var_trait=prior_var_trait,
                prior_var_ar=prior_var_ar, prior_var_state=prior_var_state, prior_a=prior_a, sd0=sd0,
                pars_inits=pars_inits, W=W)
    pars_lower <- res$pars_lower
    pars_upper <- res$pars_upper
    constraints <- res$constraints
    prior_model <- res$prior_model
    proposal_sd <- res$proposal_sd

    #--- estimate model
    #- function and arguments
    use_pmle <- use_amh <- FALSE
    if (estimator %in% c("ML", "PML")){
        LAM_fct <- LAM::pmle
        use_pmle <- TRUE
    }
    if (estimator %in% c("MCMC") ){
        LAM_fct <- LAM::amh
        use_amh <- TRUE
    }

    LAM_args <- list( data=data, nobs=data$n, pars=pars_inits, model=ll_model, prior=prior_model,
                            pars_lower=pars_lower, pars_upper=pars_upper  )

    if (use_pmle){
        LAM_args$method <- "L-BFGS-B"
        LAM_args$verbose <- verbose
        LAM_args$optim_fct <- optim_fct
    }
    if (use_amh){
        LAM_args$n.burnin <- n.burnin
        LAM_args$n.iter <- n.iter
        LAM_args$proposal_sd <- proposal_sd
    }

    #- call estimation function
    fit_LAM <- res <- do.call( what=LAM_fct, args=LAM_args )
    deviance <- fit_LAM$deviance
    coef <- res$coef
    vcov <- res$vcov

    #--- saturated fit
    if ( ! some_missings ){
        loglike_args <- list( S=data$S, Sigma=data$S, M=data$M, mu=data$M,
                            n=data$n, lambda=1E-10)
        ll_saturated <- do.call( what=loglike_fct, args=loglike_args)
        deviance_saturated <- -2*ll_saturated
    }

    #--- fitted covariance matrix
    covmat_fitted <- starts_uni_cov_pars(W=W, pars=coef, pars_est=pars_est, time_index=time_index)

    #--- model fit
    model_fit <- NULL
    if (! some_missings ){
        model_fit <- starts_estimate_model_fit( covmat=covmat, covmat_fitted=covmat_fitted, deviance=deviance,
                        deviance_saturated=deviance_saturated, df_sem=df_sem, nobs=nobs,
                        some_missings=some_missings)
    }

    #--- variance proportions inference
    vars <- c("var_trait", "var_ar", "var_state")
    if ( estimator %in% c("ML","PML")){
        var_prop <- starts_uni_estimate_variance_proportions_pml( coef=coef, vcov=vcov, vars=vars )
    }
    if ( estimator %in% c("MCMC")){
        var_prop <- starts_uni_estimate_variance_proportions_mcmc( fit_LAM=fit_LAM, vars=vars )
    }

    #--- description
    res <- starts_estimate_description(estimator=estimator)
    description <- res$description
    used_function <- res$used_function

    #--- output management
    deviance <- fit_LAM$deviance

    #-- adapt information criteria
    ic <- fit_LAM$ic

    time$end <- Sys.time()
    #--- output
    res <- list( coef=coef, vcov=vcov, deviance=deviance, ic=ic,
                    model_fit=model_fit, covmat_fitted=covmat_fitted, fit_LAM=fit_LAM, data0=data0,
                    pars_inits=pars_inits, pars_lower=pars_lower, pars_upper=pars_upper,
                    var_prop=var_prop,
                    estimator=estimator, description=description, used_function=used_function,
                    constraints=constraints, use_pmle=use_pmle, use_amh=use_amh,
                    some_missings=some_missings, time=time, CALL=CALL)
    class(res) <- "starts_uni"
    return(res)
}
