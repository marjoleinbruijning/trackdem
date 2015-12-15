#### Functions in C++

src <- '
  NumericVector mat1(m1);
  NumericVector mat2(m2);
  NumericVector mat3(m3);
  
  IntegerVector dim(d);
  //Rcpp::NumericMatrix r(dim[0],dim[1]);

  arma::cube r(mat1.begin(),dim[0],dim[1],3,false);
  std::fill(r.begin(),r.end(),0);
 
  arma::cube array1(mat1.begin(),dim[0],dim[1],dim[2],false);
  arma::cube array2(mat2.begin(),dim[0],dim[1],dim[2],false);
  arma::cube array3(mat3.begin(),dim[0],dim[1],dim[2],false);
  
  int nrows = dim[0];
  int ncols = dim[1];
  int images = dim[2];
  
 for (int j = 0; j < nrows; j++) {
    for (int i = 0; i < ncols; i++) {
      for (int k =0; k < images; k++) {
        r(j,i,0) = r(j,i,0) + array1(j,i,k);
        r(j,i,1) = r(j,i,1) + array2(j,i,k);
        r(j,i,2) = r(j,i,2) + array3(j,i,k);
      }
      r(j,i,0) =  r(j,i,0) / images;
      r(j,i,1) =  r(j,i,1) / images;
      r(j,i,2) =  r(j,i,2) / images;
    }
 }
 
  return(wrap(r));
'
createBackground3 <- cxxfunction(signature(m1="numeric", 
                                           m2="numeric",
                                           m3="numeric",
                                           d="integer"), 
                                 src, 
                                 plugin="RcppArmadillo")


############### Background subtraction
src <- '
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
'

subtractBackground2 <- cxxfunction(signature(
                  m1="numeric",bg="numeric", d="integer"), src, 
                  plugin="RcppArmadillo")
	

######## Get coords
rcpp_inc <- "\nusing namespace Rcpp;\nusing namespace arma;\n"

src <- '
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
'

getCoords2 <- cxxfunction(signature(m='numeric',d='numeric'),
                  src, 
                  plugin='RcppArmadillo', rcpp_inc)

############ Diff frame ###########
src <- '
  NumericVector mat(m1);
  IntegerVector dim(d);
  arma::cube array(mat.begin(),dim[0],dim[1],dim[2],false);

  arma::cube detect(dim[0],dim[1],dim[2]-1);
  std::fill(detect.begin(),detect.end(),0);
  
  int nrows = dim[0];
  int ncols = dim[1];
  int images = dim[2]-1;
  
  
  for (int j = 0; j < nrows; j++) {
    for (int i = 0; i < ncols; i++) {
      for (int k =0; k < images; k++) {
        detect(j,i,k) = array(j,i,k) - array(j,i,k+1);
      }
    }
  }
 
  return(wrap(detect));
'

diffFrame2 <- cxxfunction(signature(m1="numeric", d="integer"), src, 
                  plugin="RcppArmadillo")
                  
####### Sum RGB

src <- '
  NumericVector mat1(m1);
  NumericVector mat2(m2);
  NumericVector mat3(m3);

  IntegerVector dim(d);

  arma::cube array1(mat1.begin(),dim[0],dim[1],dim[2],false);
  arma::cube array2(mat2.begin(),dim[0],dim[1],dim[2],false);
  arma::cube array3(mat3.begin(),dim[0],dim[1],dim[2],false);
  
  arma::cube r(mat1.begin(),dim[0],dim[1],dim[2],false);
  std::fill(r.begin(),r.end(),0);
   
  int nrows = dim[0];
  int ncols = dim[1];
  int images = dim[2];
  
 for (int j = 0; j < nrows; j++) {
    for (int i = 0; i < ncols; i++) {
      for (int k =0; k < images; k++) {
        r(j,i,0) = r(j,i,0) + array1(j,i,k);
        r(j,i,1) = r(j,i,1) + array2(j,i,k);
        r(j,i,2) = r(j,i,2) + array3(j,i,k);
      }
    }
 }
 
  return(wrap(r));
'

sumColors <- cxxfunction(signature(m1="numeric",m2="numeric",m3="numeric", d="integer"), src, 
                  plugin="RcppArmadillo")
