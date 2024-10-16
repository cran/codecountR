#'BoxMullerGen
#'
#' @param r a number
#' @param s a number
#'
#' @return a vector
#' @export
#'
#' @examples
#' #with runif
#' v=BoxMullerGen(runif(1), runif(1))
#' print(v)
#'
#' #with congruGen
#' seed = 123456789
#' X=c()
#' for(i in 1: 2) {
#' Z=congruGen(seed)
#' seed=Z$seedUpdate
#' X=append(X, Z$aleaNum)
#' }
#' #print(X)
#'
#' N=BoxMullerGen(X[1], X[2])
#' print(N[1])
#' print(N[2])
#'
BoxMullerGen =function(r, s)
{
  a=sqrt(-2*log(r));
  b=2*pi*s;
  x= a * sin(b);
  y= a * cos(b);
  return(c(x, y))
}
