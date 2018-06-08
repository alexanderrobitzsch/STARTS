## File Name: starts_uni_estimate_proc_modify_suff_stat.R
## File Version: 0.02


starts_uni_estimate_proc_modify_suff_stat <- function( suff_stat, var_meas_error)
{
    NP <- suff_stat$NP
    var_index <- suff_stat$varindex
    S <- suff_stat[["S"]]
    if ( ! is.null(var_meas_error) ){
        for (pp in 1:NP){    
            var_pp <- var_meas_error[ var_index[[pp]] ]        
            if ( is.matrix(var_pp) | is.data.frame(var_pp) ){
                var_pp <- var_pp[1,]
            }
            var_pp <- as.numeric(var_pp)
            S_pp <- S[[pp]]
            names(var_pp) <- colnames(S_pp)
            w1 <- diag(S_pp) - var_pp
            w1 <- ifelse( w1 > 0 , w1 , 0 )
            diag(S_pp) <- w1
        }
    }
    return(suff_stat)
}
