#' subCalcBoxAndCox
#'
#' @param sortedVect a vector
#' @param actualLambda a number
#'
#' @return a vector
#' @export
#'
#' @examples
#' vec=rlnorm(100, log(3), log(3))
#' BandC=subCalcBoxAndCox(vec, -3)
subCalcBoxAndCox = function(sortedVect, actualLambda)
{
  sortedVectBC = c() #empty vector

  if (actualLambda==0)
  {
    sortedVectBC=unlist(lapply(sortedVect, function(x) (log(x))))
  }
  else
  {
    sortedVectBC=unlist(lapply(sortedVect, function(x) (x ^ actualLambda -1)/actualLambda ))
  }
  return (sortedVectBC)
}
