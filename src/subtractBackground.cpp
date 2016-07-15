// [[Rcpp::depends(RcppArmadillo)]]
#include <RcppArmadillo.h>
using namespace Rcpp;
using namespace arma;

//' Background subtraction
//'
//' \code{subtractBackground} is a function to subtract each
//' image from the created still background.
//' The objects created through the function contain all changing
//' pixels.
//' @param bg Array containing still background, as returned from
//' \code{\link{createBackground}}.
//' @param images Array containing all images for one color layer.
//' @param d Vector containing dimensions of images(number of rows, number
//' of columns, number of frames).
//' @author Marjolein Bruijning & Marco D. Visser
//' @examples
//' \dontrun{
//' ## For all color layers
//' allImages <- sapply(1:3, 
//'                   function(x) 
//'                      subtractBackground(allFullImages[,,x,],
//'                      stillBack[,,x],dim(allFullImages[,,x,])),
//'                   simplify='array') }
//' @return Returns array with same size as images, subtracted from background.
//' @export
// [[Rcpp::export]] 

NumericVector subtractBackground(NumericVector m1, NumericVector bg, IntegerVector d) {
  
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
