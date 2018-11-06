## File Name: plot.starts_uni.R
## File Version: 0.03

plot.starts_uni <- function(x, ...)
{
    if (x$use_amh){
        plot(x=x$fit_LAM, ...)
    }
}
