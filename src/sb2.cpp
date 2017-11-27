// [[Rcpp::depends(RcppArmadillo)]]
#include <RcppArmadillo.h>
using namespace Rcpp;
using namespace arma;

// [[Rcpp::export]] 
NumericVector sb2(NumericVector m1, NumericVector bg, IntegerVector d,
                 IntegerVector e) {
  
  NumericVector mat(m1);
  NumericVector b(bg);
  NumericVector empty(e);

  IntegerVector dim(d);
  arma::cube array(mat.begin(),dim[0],dim[1],dim[2],false);
  arma::cube bgg(b.begin(),dim[0],dim[1],dim[2],false);
  arma::cube subs(empty.begin(),dim[0],dim[1],dim[2],false);
  
  int nrows = dim[0];
  int ncols = dim[1];
  int images = dim[2];
  
  
 for (int j = 0; j < nrows; j++) {
    for (int i = 0; i < ncols; i++) {
      for (int k = 0; k < images; k++) {
        subs(j,i,k) = array(j,i,k) - bgg(j,i,k);
      }
    }
 }
 
  return(wrap(subs));
}
