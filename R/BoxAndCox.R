#' BoxAndCox
#'
#' @param rawVect a vector
#' @param minLambda a number
#'
#' @return a list
#' @importFrom stats cor
#' @importFrom stats qnorm
#' @importFrom stats optim
#' @importFrom stats var
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
  dat=rawVect

  max.LL <- function(data, par)
  {
    N=length(data)
    lambda=par
    lambdadep=subCalcBoxAndCox(data, lambda)
    lnVal=log(data)
    sumLnVal=sum(lnVal)
    s2=var(lambdadep)*(N-1)/N
    soca= sum( (lambdadep - mean(lambdadep) )^2 )
    LL=-(N/2)*log(s2)+(lambda-1)*sumLnVal
    return(-LL)
  }

  result <- optim(par = minLambda, fn = max.LL, data = dat, method = "Brent",lower = minLambda, upper = maxLambda)
  result$value=-(result$value)
  return(result)
}
