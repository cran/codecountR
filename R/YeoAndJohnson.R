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
#' YAJ=unlist(YandJ[3])

YeoAndJohnson = function(rawVect, minLambda)
{
  indx = c(1:length(rawVect))
  valCorel = c() #empty vector
  Lambda = c() #empty vector
  maxLambda=abs(minLambda)
  actualLambda=minLambda
  piBl = piBloom(indx, length(indx))
  qNrm = qnorm(piBl)
  sortedVect= sort(rawVect, decreasing = FALSE)

  while (actualLambda <= maxLambda)
  {
    actualLambda=round(actualLambda, digits = 7) #prevent side effect
    transformVec=subCalcYeoAndJohnson(sortedVect, actualLambda)
    corVal= cor(transformVec, piBl)
    Lambda=append(Lambda,actualLambda)
    valCorel=append(valCorel,corVal)
    actualLambda = actualLambda + 0.001
  }
  dataFra= data.frame(Lambda, valCorel)
  LambdaMaxCalc=dataFra[dataFra['valCorel']== max(valCorel)]
  transformVecFinal=unlist(subCalcYeoAndJohnson(rawVect, LambdaMaxCalc[1]))
  return (list(dataFra, LambdaMaxCalc, transformVecFinal,LambdaMaxCalc[1] ))
}
