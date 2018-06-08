## File Name: starts_ic_add.R
## File Version: 0.01

starts_ic_add <- function(ic)
{
    dev <- ic$deviance
    ic$AIC <- dev + 2*ic$np
    ic$BIC <- dev + ( log(ic$n) )*ic$np
    ic$CAIC <- dev + ( log(ic$n) + 1 )*ic$np
    ic$AICc <- ic$AIC + 2*ic$np * ( ic$np + 1 ) / ( ic$n - ic$np - 1 )            
    return(ic)    
}    
