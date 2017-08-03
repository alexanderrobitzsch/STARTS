\name{starts_cov}
\alias{starts_cov}
\alias{starts_sim1dim}
%- Also NEED an '\alias' for EACH other topic documented here.



\title{
Some Functions for the STARTS model
}


\description{
Functions for computing the covariance matrix and simulating data
from the univariate STARTS model (Kenny & Zautra, 1995, 2001).
}

\usage{
starts_cov(W, var_trait, var_ar, var_state, a )

starts_sim1dim(N, W, var_trait, var_ar, var_state, a )
}


%- maybe also 'usage' for other objects documented here.
\arguments{
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
}

%%\details{  
%% xxx    
%% }


\value{
Covariance matrix or a data frame of simulated values
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
# EXAMPLE 1: STARTS model | Data from Wu (2016)
#############################################################################

library(LAM)
library(sirt)

## define list with input data
## S ... covariance matrix, M ... mean vector

# read covariance matrix of data in Wu (older cohort, positive affect)
S <- matrix( c( 12.745, 7.046, 6.906, 6.070, 5.047, 6.110,
    7.046, 14.977, 8.334, 6.714, 6.91, 6.624,
    6.906, 8.334, 13.323, 7.979, 8.418, 7.951,
    6.070, 6.714, 7.979, 12.041, 7.874, 8.099,
    5.047, 6.91, 8.418, 7.874, 13.838, 9.117,
    6.110, 6.624, 7.951, 8.099, 9.117, 15.132 ) ,  
    nrow=6 , ncol=6 , byrow=TRUE )
#* standardize S such that the average SD is 1 (for ease of interpretation)
M_SD <- mean( sqrt( diag(S) ))
S <- S / M_SD^2    
colnames(S) <- rownames(S) <- paste0("W",1:6)    
W <- 6   # number of measurement waves
data <- list( "S" = S , "M" = rep(0,W) , "n" = 660 , "W" = W  )

#*** likelihood function for the STARTS model
model <- function( pars , data ){ 
    # mean vector
    mu <- data$M
    # covariance matrix
    W <- data$W 
    var_trait <- pars["vt"]
    var_ar <- pars["va"]
    var_state <- pars["vs"]
    a <- pars["b"]
    Sigma <- STARTS::starts_cov( W=W , var_trait=var_trait , 
                var_ar=var_ar , var_state=var_state , a=a )
    # evaluate log-likelihood
    ll <- LAM::loglike_mvnorm( S = data$S , Sigma = Sigma , M = data$M , mu = mu ,
                n = data$n , lambda = 1E-5)
    return(ll)
}
#** Note: 
#   (1) The function starts_cov calculates the model implied covariance matrix
#       for the STARTS model.
#   (2) The function loglike_mvnorm evaluates the loglikelihood for a multivariate
#       normal distribution given sample and population means M and mu, and sample 
#       and population covariance matrix S and Sigma.

#*** starting values for parameters
pars <- c( .33 , .33 , .33 , .75) 
names(pars) <- c("vt","va","vs","b")
#*** bounds for acceptance rates
acceptance_bounds <- c( .45 , .55 )
#*** starting values for proposal standard deviations
proposal_sd <- c( .1 , .1 , .1 , .1 )
names(proposal_sd) <- names(pars)
#*** lower and upper bounds for parameter estimates
pars_lower <- c( .001 , .001 , .001 , .001 )
pars_upper <- c( 10 , 10 , 10 , .999 ) 
#*** define prior distributions | use prior sample size of 3
prior_model <- "
    vt ~ dinvgamma2(NA, 3, .33 )
    va ~ dinvgamma2(NA, 3, .33 )    
    vs ~ dinvgamma2(NA, 3, .33 )        
    b ~ dbeta(NA, 4, 4 )
        "
#*** define number of iterations
n.burnin <- 5000
n.iter <- 20000 
set.seed(987)    # fix random seed
#*** estimate model with 'LAM::amh' function       
mod <- LAM::amh( data=data, nobs=data$n, pars=pars, model=model,  
            prior=prior_model, proposal_sd=proposal_sd, n.iter=n.iter, 
            n.burnin=n.burnin, pars_lower=pars_lower, pars_upper=pars_upper)           
#*** model summary
summary(mod)
  ##  Parameter Summary (Marginal MAP estimation) 
  ##    parameter   MAP    SD  Q2.5 Q97.5  Rhat SERatio effSize accrate
  ##  1        vt 0.352 0.088 0.122 0.449 1.014   0.088     128   0.557
  ##  2        va 0.335 0.080 0.238 0.542 1.015   0.090     123   0.546
  ##  3        vs 0.341 0.018 0.297 0.367 1.005   0.042     571   0.529
  ##  4         b 0.834 0.065 0.652 0.895 1.017   0.079     161   0.522
  ##
  ##  Comparison of Different Estimators
  ##  
  ##  MAP: Univariate marginal MAP estimation
  ##  mMAP: Multivariate MAP estimation (penalized likelihood estimate)
  ##  Mean: Mean of posterior distributions
  ##   
  ##    Parameter Summary:
  ##    parm   MAP  mMAP  Mean
  ##  1   vt 0.352 0.294 0.300
  ##  2   va 0.335 0.371 0.369
  ##  3   vs 0.341 0.339 0.335
  ##  4    b 0.834 0.822 0.800

#* inspect convergence
plot(mod, ask=TRUE)

#--------------------------- 
# fitting the STARTS model with penalized maximum likelihood estimation
mod2 <- LAM::pmle( data=data , nobs= data$n , pars=pars , model = model ,  prior=prior_model , 
            pars_lower = pars_lower , pars_upper = pars_upper , method = "L-BFGS-B" ,
            control=list( trace=TRUE )  )           
# model summaries
summary(mod2)
  ##  Parameter Summary 
  ##    parameter   est    se      t     p active
  ##  1        vt 0.298 0.110  2.712 0.007      1
  ##  2        va 0.364 0.102  3.560 0.000      1
  ##  3        vs 0.337 0.018 18.746 0.000      1
  ##  4         b 0.818 0.074 11.118 0.000      1

#---------------------------
# fitting the STARTS model in lavaan

library(lavaan)

## define lavaan model
lavmodel <- "
     #*** stable trait
     T =~ 1*W1 + 1*W2 + 1*W3 + 1*W4 + 1*W5 + 1*W6
     T ~~ vt * T
     W1 ~~ 0*W1
     W2 ~~ 0*W2
     W3 ~~ 0*W3
     W4 ~~ 0*W4
     W5 ~~ 0*W5     
     W6 ~~ 0*W6     
     #*** autoregressive trait
     AR1 =~ 1*W1
     AR2 =~ 1*W2     
     AR3 =~ 1*W3     
     AR4 =~ 1*W4          
     AR5 =~ 1*W5               
     AR6 =~ 1*W6              
     #*** state component      
     S1 =~ 1*W1
     S2 =~ 1*W2     
     S3 =~ 1*W3     
     S4 =~ 1*W4        
     S5 =~ 1*W5          
     S6 =~ 1*W6               
     S1 ~~ vs * S1
     S2 ~~ vs * S2
     S3 ~~ vs * S3
     S4 ~~ vs * S4 
     S5 ~~ vs * S5      
     S6 ~~ vs * S6      
     AR2 ~ b * AR1
     AR3 ~ b * AR2     
     AR4 ~ b * AR3          
     AR5 ~ b * AR4               
     AR6 ~ b * AR5                    
     AR1 ~~ va * AR1              
     AR2 ~~ v1 * AR2                   
     AR3 ~~ v1 * AR3     
     AR4 ~~ v1 * AR4   
     AR5 ~~ v1 * AR5       
     AR6 ~~ v1 * AR6 
     #*** nonlinear constraint
     v1 == va * ( 1 - b^2 )  
     #*** force variances to be positive
     vt > 0.001
     va > 0.001
     vs > 0.001
     #*** variance proportions
     var_total := vt + vs + va
     propt := vt / var_total
     propa := va / var_total
     props := vs / var_total
	"	
# estimate lavaan model
mod <- lavaan::lavaan(model = lavmodel, sample.cov = S, sample.nobs = 660)
# summary and fit measures
summary(mod)
lavaan::fitMeasures(mod)
coef(mod)[ ! duplicated( names(coef(mod))) ]
  ##           vt          vs           b          va          v1 
  ##  0.001000023 0.349754630 0.916789054 0.651723144 0.103948711 
}
}


% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{DETECT}
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
