%% File Name: data.starts.Rd
%% File Version: 0.04
%% File Last Change: 2017-08-01 14:09:48

\name{data.starts}
\alias{data.starts}
\alias{data.starts01a}
\alias{data.starts01b}

\docType{data}

\title{
Datasets in the \pkg{STARTS} Package
}

\description{
Some datasets for illustration used in the examples of the
\pkg{STARTS} package. 
}

\usage{
data(data.starts01a)
data(data.starts01b)
}



\format{
\itemize{	
\item  \code{data.starts01a}.
A resimulated dataset containing three factors from the Big5 scale measured at
five time points used in Luedtke et al. (xxxx). The dataset only contains observations
without missing data. 
  
\code{'data.frame':   890 obs. of  16 variables:} \cr
\code{ $ id: int  100006 100008 100010 100014 100032 100033 100035 100038 100049 100050 ...} \cr
\code{ $ E1: num  -0.28 1.48 0.12 -1.05 -0.28 ...} \cr
\code{ $ E2: num  0.12 -0.092 0.495 -0.679 -0.467 ...} \cr
\code{ $ E3: num  1.08 0.12 0.12 -1.27 -0.28 ...} \cr
\code{ $ E4: num  0.495 0.12 1.294 -2.229 -0.28 ...} \cr
\code{ $ E5: num  -0.092 0.707 0.707 -2.041 -0.092 ...} \cr
\code{ $ N1: num  1.114 -0.173 -0.017 0.958 1.27 ...} \cr
\code{ $ N2: num  -0.348 0.003 -1.167 1.602 1.758 ...} \cr
\code{ $ N3: num  -0.192 0.471 -0.348 1.114 0.627 ...} \cr
\code{ $ N4: num  -0.348 -1.167 -0.504 1.426 1.27 ...} \cr
\code{ $ N5: num  -0.192 -0.836 -0.192 2.421 1.27 ...} \cr
\code{ $ O1: num  1.994 -1.82 -0.107 -0.678 -0.792 ...} \cr
\code{ $ O2: num  1.423 -0.678 -0.678 -0.678 1.423 ...} \cr
\code{ $ O3: num  1.423 -1.066 -0.678 0.075 0.852 ...} \cr
\code{ $ O4: num  -0.29 -0.678 -0.29 0.075 -0.107 ...} \cr
\code{ $ O5: num  1.217 -1.637 -0.29 -0.678 0.646 ...} \cr

  
\item  \code{data.starts01b}. Like \code{data.starts01a}, but the dataset also
contains cases with missing data. 
  
\code{'data.frame':   3215 obs. of  17 variables:} \cr
\code{ $ id  : int  100001 100002 100003 100004 100005 100006 100007 100008 100009 100010 ...} \cr
\code{ $ patt: Factor w/ 26 levels "P00010","P00011",..: 24 19 20 25 22 26 18 26 19 26 ...} \cr
\code{ $ E1  : num  0.308 1.67 0.308 0.308 -0.468 ...} \cr
\code{ $ E2  : num  0.308 0.895 0.707 0.707 0.12 0.12 NA -0.092 -0.28 0.496 ...} \cr
\code{ $ E3  : num  0.895 NA NA 0.895 NA ...} \cr
\code{ $ E4  : num  NA NA NA 0.496 0.496 ...} \cr
\code{ $ E5  : num  0.707 NA 0.308 NA 0.496 -0.092 -0.28 0.707 NA 0.707 ...} \cr
\code{ $ N1  : num  0.783 -0.017 -0.192 -0.017 -0.504 ...} \cr
\code{ $ N2  : num  1.114 -0.348 -0.348 -0.348 -0.836 ...} \cr
\code{ $ N3  : num  -0.348 NA NA -0.348 NA ...} \cr
\code{ $ N4  : num  NA NA NA -0.504 -1.811 ...} \cr
\code{ $ N5  : num  0.471 NA -0.192 NA -1.421 ...} \cr
\code{ $ O1  : num  -0.495 -0.107 -0.495 1.035 -0.792 ...} \cr
\code{ $ O2  : num  -0.107 -0.107 -0.29 1.035 -0.29 ...} \cr
\code{ $ O3  : num  0.464 NA NA 1.423 NA ...} \cr
\code{ $ O4  : num  NA NA NA 1.423 0.281 ...} \cr
\code{ $ O5  : num  0.646 NA -1.066 NA 0.281 ...} \cr


 }
}


\references{
Luedtke, O., Robitzsch, A., & Wagner, J. (xxxx). More stable estimation of the STARTS model:  
A Bayesian approach using Markov Chain Monte Carlo techniques. 
\emph{Psychological Methods}, \bold{xx}, xxx-xxx.
}




\keyword{datasets}