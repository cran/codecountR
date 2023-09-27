#' YeoAndJohnson
#'
#' @param rawVect a vector
#' @param minLambda a number
#'
#' @return a list
#' @export
#'
#' @examples
#' vec=rlnorm(100, log(3), log(3))
#' YandJ=YeoAndJohnson(vec, -3)
#' YandJ
#' YAJ=unlist(YandJ$par)
#' rawVectYJFinal=unlist(subCalcYeoAndJohnson(vec, YandJ$par))
YeoAndJohnson = function(rawVect, minLambda)
{
  maxLambda=abs(minLambda)
  indx = c(1:length(rawVect))
  piBl = piBlom(indx, length(indx))
  qNrm = qnorm(piBl)
  dat=sort(rawVect)


  max.cor <- function(data, par)
  {
    print(par)
    sortedVectYJ=subCalcYeoAndJohnson(data, par)
    resu=cor(sortedVectYJ, qNrm)
    print(-resu)
    return(-resu)
  }

  result <- optim(par = minLambda, fn = max.cor, data = dat, method = "Brent",lower = minLambda, upper = maxLambda)
  return(result)
}
