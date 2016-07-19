// [[Rcpp::depends(RcppArmadillo)]]
#include <RcppArmadillo.h>
using namespace Rcpp;
using namespace arma;

//' @export
// [[Rcpp::export]] 
NumericVector sb(NumericVector m1, NumericVector bg, IntegerVector d) {
  
  NumericVector mat(m1);
  NumericMatrix b(bg);
  IntegerVector dim(d);
  arma::cube array(mat.begin(),dim[0],dim[1],dim[2],false);
  //arma::cube arrayBg(b.begin(),dim[0],dim[1],dim[2],false);
  arma::cube subs(mat.begin(),dim[0],dim[1],dim[2],false);
  
  int nrows = dim[0];
  int ncols = dim[1];
  int images = dim[2];
  
  
 for (int j = 0; j < nrows; j++) {
    for (int i = 0; i < ncols; i++) {
      for (int k = 0; k < images; k++) {
        subs(j,i,k) = array(j,i,k) - b(j,i);
      }
    }
 }
 
  return(wrap(subs));
}
