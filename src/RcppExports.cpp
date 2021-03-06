// This file was generated by Rcpp::compileAttributes
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <Rcpp.h>

using namespace Rcpp;

// gillespie
Rcpp::DataFrame gillespie(Rcpp::IntegerVector start_states, Rcpp::NumericVector start_times, const int n_state, Rcpp::NumericVector rates, Rcpp::NumericVector patientEndTimes, Rcpp::NumericVector calendarEndTimes, Rcpp::NumericMatrix shape, Rcpp::NumericVector resetEdges, const double duration);
RcppExport SEXP badminton_gillespie(SEXP start_statesSEXP, SEXP start_timesSEXP, SEXP n_stateSEXP, SEXP ratesSEXP, SEXP patientEndTimesSEXP, SEXP calendarEndTimesSEXP, SEXP shapeSEXP, SEXP resetEdgesSEXP, SEXP durationSEXP) {
BEGIN_RCPP
    Rcpp::RObject __result;
    Rcpp::RNGScope __rngScope;
    Rcpp::traits::input_parameter< Rcpp::IntegerVector >::type start_states(start_statesSEXP);
    Rcpp::traits::input_parameter< Rcpp::NumericVector >::type start_times(start_timesSEXP);
    Rcpp::traits::input_parameter< const int >::type n_state(n_stateSEXP);
    Rcpp::traits::input_parameter< Rcpp::NumericVector >::type rates(ratesSEXP);
    Rcpp::traits::input_parameter< Rcpp::NumericVector >::type patientEndTimes(patientEndTimesSEXP);
    Rcpp::traits::input_parameter< Rcpp::NumericVector >::type calendarEndTimes(calendarEndTimesSEXP);
    Rcpp::traits::input_parameter< Rcpp::NumericMatrix >::type shape(shapeSEXP);
    Rcpp::traits::input_parameter< Rcpp::NumericVector >::type resetEdges(resetEdgesSEXP);
    Rcpp::traits::input_parameter< const double >::type duration(durationSEXP);
    __result = Rcpp::wrap(gillespie(start_states, start_times, n_state, rates, patientEndTimes, calendarEndTimes, shape, resetEdges, duration));
    return __result;
END_RCPP
}
