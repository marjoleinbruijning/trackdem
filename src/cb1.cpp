// [[Rcpp::depends(RcppArmadillo)]]
#include <RcppArmadillo.h>
using namespace Rcpp;
using namespace arma;

// [[Rcpp::export]]
NumericVector cb1(NumericVector m1, 
                 IntegerVector d, IntegerVector e) {
	
NumericVector mat1(m1);
NumericVector empty(e);
IntegerVector dim(d);

arma::cube r(empty.begin(),dim[0],dim[1],1,false);
std::fill(r.begin(),r.end(),0);
 
arma::cube array1(mat1.begin(),dim[0],dim[1],dim[2],false);

int nrows = dim[0];
int ncols = dim[1];
int images = dim[2];
  
for (int j = 0; j < nrows; j++) {
  for (int i = 0; i < ncols; i++) {
    for (int k =0; k < images; k++) {
      r(j,i,0) = r(j,i,0) + array1(j,i,k);
    }
    r(j,i,0) =  r(j,i,0) / images;
  }
}

return(wrap(r));

}
