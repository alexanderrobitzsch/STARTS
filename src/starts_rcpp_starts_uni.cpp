//// File Name: starts_rcpp_starts_uni.cpp
//// File Version: 0.22


// [[Rcpp::depends(RcppArmadillo)]]

#include <RcppArmadillo.h>
// #include <Rcpp.h>

using namespace Rcpp;
using namespace arma;


///********************************************************************
///** starts_rcpp_starts_uni_cov
// [[Rcpp::export]]
Rcpp::NumericMatrix starts_rcpp_starts_uni_cov( double var_trait, double var_ar,
    double var_state, double a, Rcpp::NumericVector time_index  )
{
    int W = time_index.size();
    Rcpp::NumericMatrix covmat(W,W);
    covmat.fill(var_trait);
    for (int ww=0; ww<W; ww++){
        covmat(ww,ww) += var_state;
    }
    for (int ii=0; ii<W; ii++){
        for (int jj=ii; jj<W; jj++){
            int diff_time = time_index[jj] - time_index[ii];
            covmat(ii,jj) += std::pow( a, diff_time ) * var_ar;
            covmat(jj,ii) = covmat(ii,jj);
        }
    }
    //-- output
    return covmat;
}
///********************************************************************


