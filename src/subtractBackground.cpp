// [[Rcpp::depends(RcppArmadillo)]]
#include <RcppArmadillo.h>
using namespace Rcpp;
using namespace arma;

//' Background subtraction
//'
//' \code{subtractBackground} is a function to subtract each
//' image from the created still background.
//' The objects created through the function contain all moving
//' pixels.
//' @param background Array containing still background.
//' @param images Array containing all images.
//' @author Marjolein Bruijning & Marco D. Visser
//' @examples
//' \dontrun{
//'
//'   allImages <- subtractBackground(background=stillBack,allFullImages)
//'	}
//' @seealso \code{\link{createBackground}}
//' @return Returns array with same size as images, subtracted from background.
//' @concept What is the broad searchable concept?
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
