#' BoxAndCox
#'
#' @param rawVect a vector
#' @param minLambda a number
#'
#' @return a list
#' @importFrom stats cor
#' @importFrom stats qnorm
#' @importFrom stats optim
#' @export
#'
#' @examples
#' vec=rlnorm(100, log(3), log(3))
#' BandC=BoxAndCox(vec, -3)
#' BandC
#' BAC=unlist(BandC$par)
#' BAC
#' rawVectBCFinal=unlist(subCalcBoxAndCox(vec, BandC$par))
BoxAndCox = function(rawVect, minLambda)
{
  maxLambda=abs(minLambda)
  indx = c(1:length(rawVect))
  piBl = piBlom(indx, length(indx))
  qNrm = qnorm(piBl)
  dat=sort(rawVect)


  max.cor <- function(data, par)
  {
    print(par)
    sortedVectBC=subCalcBoxAndCox(data, par)
    resu=cor(sortedVectBC, qNrm)
    print(-resu)
    return(-resu)
  }

  result <- optim(par = -3.0, fn = max.cor, data = dat, method = "Brent",lower = minLambda, upper = maxLambda)
  return(result)
}
