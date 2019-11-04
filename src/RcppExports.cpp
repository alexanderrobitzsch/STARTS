//// File Name: RcppExports.cpp
//// File Version: 1.003002
// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <RcppArmadillo.h>

using namespace Rcpp; using namespace arma;

// starts_rcpp_starts_uni_cov
Rcpp::NumericMatrix starts_rcpp_starts_uni_cov(double var_trait, double var_ar, double var_state, double a, Rcpp::NumericVector time_index);
RcppExport SEXP _STARTS_starts_rcpp_starts_uni_cov(SEXP var_traitSEXP, SEXP var_arSEXP, SEXP var_stateSEXP, SEXP aSEXP, SEXP time_indexSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< double >::type var_trait(var_traitSEXP);
    Rcpp::traits::input_parameter< double >::type var_ar(var_arSEXP);
    Rcpp::traits::input_parameter< double >::type var_state(var_stateSEXP);
    Rcpp::traits::input_parameter< double >::type a(aSEXP);
    Rcpp::traits::input_parameter< Rcpp::NumericVector >::type time_index(time_indexSEXP);
    rcpp_result_gen = Rcpp::wrap(starts_rcpp_starts_uni_cov(var_trait, var_ar, var_state, a, time_index));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_STARTS_starts_rcpp_starts_uni_cov", (DL_FUNC) &_STARTS_starts_rcpp_starts_uni_cov, 5},
    {NULL, NULL, 0}
};

RcppExport void R_init_STARTS(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
