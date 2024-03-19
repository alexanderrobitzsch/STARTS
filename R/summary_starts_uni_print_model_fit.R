## File Name: summary_starts_uni_print_model_fit.R
## File Version: 0.101

summary_starts_uni_print_model_fit <- function(object, digits_fit=3)
{
    if ( ! object$some_missings ){
        cat('-----------------------------------------------------------------\n')
        cat('Model Fit \n\n')
        cat( paste0('Chi square test of model fit: \nChi2(df=', object$model_fit$df_sem,
                    ')', ' ', '=', ' ', round( object$model_fit$chisq, digits_fit ),
                    ', p', ' ', '=', ' ', round( object$model_fit$p_chisq, digits_fit),
                        '\n\n') )
        cat( 'SRMR', '=', round( object$model_fit$SRMR, digits_fit ), '\n' )
        cat( 'RMSEA', '=', round( object$model_fit$RMSEA, digits_fit ), '\n' )
    }
}
