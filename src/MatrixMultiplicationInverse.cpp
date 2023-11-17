#include <Rcpp.h>
using namespace Rcpp;


//' Matrix Multiplication
//'
//' Multiplies two matrices.
//' @param mat1 NumericMatrix, the first matrix.
//' @param mat2 NumericMatrix, the second matrix.
//' @return NumericMatrix, the result of multiplying `mat1` and `mat2`.
//' @export
//' @examples
//' mat1 <- matrix(1:4, nrow = 2)
//' mat2 <- matrix(1:4, nrow = 2)
//' matrixMultiply(mat1, mat2)
// [[Rcpp::export]]
NumericMatrix matrixMultiply(NumericMatrix mat1, NumericMatrix mat2) {
  int nrow = mat1.nrow(), ncol = mat2.ncol();
  NumericMatrix out(nrow, ncol);

  for (int i = 0; i < nrow; i++) {
    for (int j = 0; j < ncol; j++) {
      double total = 0;
      for (int k = 0; k < mat1.ncol(); k++) {
        total += mat1(i, k) * mat2(k, j);
      }
      out(i, j) = total;
    }
  }
  return out;
}

//' Matrix Inverse
//'
//' Calculates the inverse of a matrix.
//' @param mat NumericMatrix, the matrix for which to find the inverse.
//' @return NumericMatrix, the inverse of `mat`.
//' @export
//' @examples
//' mat <- matrix(1:4, nrow = 2)
//' matrixInverse(mat)
// [[Rcpp::export]]
NumericMatrix matrixInverse(NumericMatrix mat) {
  Environment base("package:base");
  Function solve = base["solve"];
  return solve(mat);
}
