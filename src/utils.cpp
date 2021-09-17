#include <Rcpp.h>
using namespace Rcpp;


template <int RTYPE>
Vector<RTYPE> lag_cpp_impl(Vector<RTYPE> x, int n) {
  int N = x.size();
  if (n >= N || n <= -N) {
    std::fill(x.begin(), x.end(), traits::get_na<RTYPE>());
  } else if (n > 0) {
    std::move(x.begin(), x.end() - n, x.begin() + n);
    std::fill(x.begin(), x.begin() + n, traits::get_na<RTYPE>());
  } else if (n < 0) {
    std::move(x.begin() - n, x.end(), x.begin());
    std::fill(x.end() + n, x.end(), traits::get_na<RTYPE>());
  }
  return x;
}



template <int RTYPE>
Vector<RTYPE> subsequence_vec_impl(const Vector<RTYPE>& x,
                          int start,
                          int end) {

  if (start > end)
    return Vector<RTYPE>();

  // translate from R to C indexing
  --start;
  --end;

  // bounds checking
  if (start < 0) start = 0;
  if (end > x.size()) end = x.size();

  // note: want to be tail inclusive
  Vector<RTYPE> output = no_init(end - start + 1);
  std::copy(x.begin() + start,
            x.begin() + end + 1,
            output.begin());

  return output;
}


template <int RTYPE>
Matrix<RTYPE> subsequence_mat_impl(Matrix<RTYPE> x,
                                   int start,
                                   int end) {

  if (start > end)
    return Matrix<RTYPE>();

  // translate from R to C indexing
  --start;
  --end;

  // bounds checking
  if (start < 0) start = 0;
  if (end > x.nrow()) end = x.nrow();

  // note: want to be tail inclusive
  // Create an output matrix
  return x(Range(start, end), _);
}

// [[Rcpp::export]]
SEXP subsequence( SEXP x, int start, int end){
  //return subsequence_impl<TYPEOF(x)>(x, start, end);
  if(Rf_isMatrix(x)){
    RCPP_RETURN_MATRIX(subsequence_mat_impl, x, start, end);
  }else{
    RCPP_RETURN_VECTOR(subsequence_vec_impl, x, start, end);
  }

}


// [[Rcpp::export]]
SEXP lag_cpp(SEXP x, int n = 1) {
  switch (TYPEOF(x)) {
  case INTSXP: return lag_cpp_impl<INTSXP>(x, n);
  case LGLSXP: return lag_cpp_impl<LGLSXP>(x, n);
  case STRSXP: return lag_cpp_impl<STRSXP>(x, n);
  case REALSXP: return lag_cpp_impl<REALSXP>(x, n);
  default: stop("type not implemented");
  }
}
