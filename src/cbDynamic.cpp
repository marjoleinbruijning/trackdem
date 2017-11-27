// [[Rcpp::depends(RcppArmadillo)]]
#include <RcppArmadillo.h>
using namespace Rcpp;
using namespace arma;

// [[Rcpp::export]]
NumericVector cbDynamic(NumericVector m1, 
                 IntegerVector d, IntegerVector e,NumericVector w) {
	
NumericVector mat1(m1);
NumericVector empty(e);
IntegerVector dim(d);
NumericMatrix weight(w);

arma::cube r1(empty.begin(),dim[0],dim[1],dim[2],false);
std::fill(r1.begin(),r1.end(),0);

arma::cube array1(mat1.begin(),dim[0],dim[1],dim[2],false);

int nrows = dim[0];
int ncols = dim[1];
int images = dim[2];
  
for (int j = 0; j < nrows; j++) {
  for (int i = 0; i < ncols; i++) {
    for (int k =0; k < images; k++) {
      for (int z = 0; z < images; z++) {
        r1(j,i,k) = r1(j,i,k) + w(z,k)*array1(j,i,z);
      }
    }
  }
}

return(wrap(r1));

}
