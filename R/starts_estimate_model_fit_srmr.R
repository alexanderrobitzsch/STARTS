## File Name: starts_estimate_model_fit_srmr.R
## File Version: 0.03

starts_estimate_model_fit_srmr <- function(covmat, covmat_fitted)
{
    # sd_diag <- sqrt( diag(covmat) )
    sd_diag <- sqrt( diag(covmat_fitted) )    
    #-- Mplus version; include different versions, see lavaan package
    srmr <- ( ( covmat - covmat_fitted )/ outer( sd_diag , sd_diag ) )^2 
    srmr <- sqrt( mean( srmr ) )
    return(srmr)
}
