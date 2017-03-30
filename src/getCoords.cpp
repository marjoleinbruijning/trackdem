// [[Rcpp::depends(RcppArmadillo)]]
#include <RcppArmadillo.h>
using namespace Rcpp;
using namespace arma;

// [[Rcpp::export]]

NumericVector getCoords(NumericVector m, IntegerVector d) {
	// input matrix
  NumericMatrix mat(m); 
  IntegerVector dim(d);
  arma::mat y(mat.begin(),dim[0],dim[1],false);
 
  // find coordinates
  arma::uvec ind = arma::find(y > 0) + 1;
 
  // translate indices to coordinates
  IntegerVector sz(3);
  sz[0] = 1;
  sz[1] = dim[0];
  sz[2] = dim[1];
  IntegerVector den(1);
  den[0] = 1;
  IntegerMatrix sub(ind.size(),2);
  IntegerVector num(1);
  IntegerVector s(1);
  
  for (int i = 1; i < 3; i++) {
    den = den * sz[i-1];
    num = den * sz[i];
    int num2 = Rcpp::as<int>(num);
    for (int k = 0; k < ind.size(); k++) {
		s = (((ind[k]-1) % num2)/den) + 1;
		sub(k,i-1) = s(0);
	}
  }
  
  return(wrap(sub));
}
