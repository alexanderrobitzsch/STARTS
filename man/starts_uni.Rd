%% File Name: starts_uni.Rd
%% File Version: 0.36

\name{starts_uni}
\alias{starts_uni_cov}
\alias{starts_cov}
\alias{starts_sim1dim}
\alias{starts_uni_estimate}
\alias{starts_uni}
\alias{starts_uni_sim}
\alias{summary.starts_uni}
\alias{plot.starts_uni}
\alias{logLik.starts_uni}
\alias{coef.starts_uni}
\alias{vcov.starts_uni}
%- Also NEED an '\alias' for EACH other topic documented here.



\title{
Functions for the Univariate STARTS Model
}


\description{
Functions for computing the covariance matrix and simulating data
from the univariate STARTS model (Kenny & Zautra, 1995, 2001). The STARTS model can
be estimated with maximum likelihood, penalized maximum likelihood (i.e., 
maximum posterior estimation) or Markov Chain Monte Carlo. 
See Luedtke et al. (xxxx) for comparisons among estimation methods.

Missing data cannot be handled in the recent package version. Full information
maximum likelihood will be included in future package versions. 
}

\usage{
## estimation of univariate STARTS model
starts_uni_estimate(data = NULL, covmat = NULL, nobs = NULL, estimator = "ML", 
    pars_inits = NULL, prior_var_trait = c(3, 0.33), prior_var_ar = c(3, 0.33), 
    prior_var_state = c(3, 0.33), prior_a = c(3, 0.5), est_var_trait = TRUE, 
    est_var_ar = TRUE, est_var_state = TRUE, var_meas_error = 0, constraints = TRUE, 
    time_index = NULL, type = "stationary", n.burnin=5000, n.iter=20000)

\method{summary}{starts_uni}(object, digits = 3, file = NULL, ...)

\method{plot}{starts_uni}(x, ...)

\method{logLik}{starts_uni}(object, ...)

\method{coef}{starts_uni}(object, ...)

\method{vcov}{starts_uni}(object, ...)

## computation of covariance matrix 
starts_uni_cov(W, var_trait, var_ar, var_state, a, time_index = NULL )

## simulation of STARTS model
starts_uni_sim(N, W, var_trait, var_ar, var_state, a, time_index = NULL )

#--- deprecated functions
starts_cov(W, var_trait, var_ar, var_state, a )
starts_sim1dim(N, W, var_trait, var_ar, var_state, a )
}


%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
Data frame
}
  \item{covmat}{
Covariance matrix (not necessary if \code{data} is provided)
}
  \item{nobs}{
Number of observations (not necessary if \code{data} is provided)
}
  \item{estimator}{
Type of estimator: \code{"ML"} for maximum likelihood estimation,
\code{"PML"} for penalized maximum likelihood estimation and
\code{"MCMC"} for Markov chain Monte Carlo estimation.
}
  \item{pars_inits}{
Optional vector of initial parameters
}
  \item{prior_var_trait}{
Vector of length two specifying the inverse gamma prior for trait variance. The first entry
is teh prior sample size, the second entry the guess of the proportion
of the variance that is attributed to the trait variance. See Luedtke et al. (xxxx)
for further details.  
}
  \item{prior_var_ar}{
Prior for autoregressive variance. See \code{prior_var_trait} for
details.
}
  \item{prior_var_state}{
Prior for state variance. See \code{prior_var_trait} for
details.
}
  \item{prior_a}{
Vector of length two for specification of the beta prior for stability parameter \code{a}.
The first entry corresponds to the prior sample size, the second entry corresponds
to the prior guess of the stability parameter. 
}
  \item{est_var_trait}{
Logical indicating whether the trait variance should be estimated.
}
  \item{est_var_ar}{
Logical indicating whether the autoregressive variance should be estimated.
}
  \item{est_var_state}{
Logical indicating whether the state variance should be estimated.
}
  \item{var_meas_error}{
Value of known measurement variance. Could be based on a reliability estimate of
internal consistency, for example.
}
  \item{constraints}{
Logical indicating whether variances should be constrained to be positive
}
  \item{time_index}{
Integer vector of time indices. Time points can be non-equidistant, but must 
be integer values.

}
  \item{type}{
Type of starts model. Only \code{"stationary"} is implemented in this
package version.
}
\item{n.burnin}{Number of burn-in iterations (if \code{estimator="MCMC"})}
\item{n.iter}{Total number of iterations (if \code{estimator="MCMC"})}
\item{W}{
Number of measurement waves.
}
\item{var_trait}{
Variance of trait component.
}
\item{var_ar}{
Variance of autoregressive component.
}    
\item{var_state}{
Variance of state component.
}    
\item{N}{
Sample size of persons
}    
\item{a}{
Stability parameter
}      
\item{object}{Object of class \code{starts_uni}}
\item{digits}{Number of digits after decimal in \code{summary} output}
\item{file}{Optional file name for \code{summary} output}
\item{x}{Object of class \code{starts_uni}}
\item{\dots}{Further arguments to be passed. For the \code{plot} method,
see the \code{plot} method for the \code{\link[LAM:amh]{LAM::amh}} 
function for arguments}
}

%%   'object' 'digits' 'file' '...'

%%\details{  
%% xxx    
%% }


\value{
Output of \code{starts_uni_estimate}


\item{coef}{Vector of estimated parameters}
\item{\dots}{Further values}


Output of \code{starts_uni_cov} is a covariance matrix.

Output of \code{starts_uni_sim} is a data frame containing simulated values.
}

%% include references
\references{
Kenny, D. A., & Zautra, A. (1995). The trait-state-error 
model for multiwave data. 
\emph{Journal of Consulting and Clinical Psychology}, 
\bold{63}, 52-59.

Kenny, D. A., & Zautra, A. (2001). Trait-state models for 
longitudinal data. In L. M. Collins & A. G. Sayer (Eds.), 
\emph{New methods for the analysis of change} (pp. 243-263).
Washington, DC, US: American Psychological Association.

Luedtke, O., Robitzsch, A., & Wagner, J. (xxxx). More stable estimation of the STARTS model:  
A Bayesian approach using Markov Chain Monte Carlo techniques. 
\emph{Psychological Methods}, \bold{xx}, xxx-xxx.

Wu, P.-C. (2016). Longitudinal stability of the Beck 
Depression Inventory II: A latent traitstate-occasion model. 
\emph{Journal of Psychoeducational Assessment}, 
\bold{34}, 39-53.
}

\author{
Alexander Robitzsch
}

%\note{
%%  ~~further notes~~
%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

%%\seealso{
%%xxx
%%}


\examples{
\dontrun{    
#############################################################################
# EXAMPLE 1: STARTS model specification using starts_uni_estimate  
#############################################################################  

## use simulated dataset according to Luedtke et al. (xxxx)

data(data.starts01a, package="STARTS")
dat <- data.starts01a

#--- covariance matrix and number of observations
covmat <- stats::cov( dat[ , paste0("E",1:5) ] )
nobs <- nrow(dat)

#*** Model 1a: STARTS model with ML estimation 
mod1a <- STARTS::starts_uni_estimate( covmat=covmat, nobs=nobs)
summary(mod1a)

#*** Model 1b: STARTS model with penalized ML estimation using the default priors
mod1b <- STARTS::starts_uni_estimate( covmat=covmat, nobs=nobs, estimator="PML")
summary(mod1b)

#*** Model 1c: STARTS model with MCMC estimation and default priors 
set.seed(987)
mod1c <- STARTS::starts_uni_estimate( covmat=covmat, nobs=nobs, estimator="MCMC")

# assess convergence
plot(mod1c)
# summary
summary(mod1c)
# extract more information
logLik(mod1c)
coef(mod1c)
vcov(mod1c)

#*** Model 1d: MCMC estimation with different prior distributions
mod1d <- STARTS::starts_uni_estimate( covmat=covmat, nobs=nobs, estimator="MCMC", 
             prior_var_trait = c(10, 0.5), prior_var_ar = c(10, 0.3), 
             prior_var_state = c(10, 0.2), prior_a = c(1, 0.5) )
summary(mod1d)

#*** Model 2: remove autoregressive process
mod2 <- STARTS::starts_uni_estimate( covmat=covmat, nobs=nobs, est_var_ar=FALSE)
summary(mod2)

#*** Model 3: remove stable trait factor
mod3 <- STARTS::starts_uni_estimate( covmat=covmat, nobs=nobs, est_var_trait=FALSE)
summary(mod3)

#*** Model 4: remove state variance from the model
mod4 <- STARTS::starts_uni_estimate( covmat=covmat, nobs=nobs, est_var_state=FALSE)
summary(mod4)
}
}


% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{vvvvv}
%\keyword{vvvvv}% __ONLY ONE__ keyword per line
