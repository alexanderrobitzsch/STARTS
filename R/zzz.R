## File Name: zzz.R
## File Version: 0.081
#  zzz.R
#
# This function is simply copied from the mice package.


# on attach sirt
.onAttach <- function(libname,pkgname)
{
    d <- utils::packageDescription('STARTS')
    d1 <- d$Version
    packageStartupMessage(
        paste('#* ', d$Package,' ', d1,' (',d$Date,')',sep='')  )
}


version <- function(pkg='STARTS')
{
    lib <- dirname( system.file(package=pkg))
    d <- utils::packageDescription(pkg)
    return( paste(d$Package,d$Version,d$Date,lib))
}
