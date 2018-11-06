## File Name: logLik.starts_uni.R
## File Version: 0.02


logLik.starts_uni <- function (object, ...)
{
    out <- - object$ic$deviance / 2
    attr(out, "df") <- object$ic$np
    attr(out, "nobs") <- object$ic$n
    class(out) <- "logLik"
    return(out)
}
