// Libraries included
#include <RcppArmadillo.h>

// Header files included
#include "config.h"


// [[Rcpp::export]]
double pf_CPP(double & F_val, arma::uword& df1, arma::uword& df2){
  
  return(R::pf(F_val, df1, df2, 0, 1));
}





  