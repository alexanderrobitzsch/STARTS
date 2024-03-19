## File Name: starts_summary_print_computation_time.R
## File Version: 0.04

starts_summary_print_computation_time <- function(object)
{
    cat('Date of Analysis:', '\n')
    cat('   Start:', paste( object$time$start ), '\n')
    cat('   End  :', paste( object$time$end ), '\n')
    cat('Computation time:', print(object$time$end - object$time$start), '\n\n')
}
