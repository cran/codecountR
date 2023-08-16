#' BoxAndCox
#'
#' @param rawVect a vector
#' @param minLambda a number
#'
#' @return a list
#' @importFrom stats cor
#' @importFrom stats qnorm
#' @export
#'
#' @examples
#' vec=rlnorm(100, log(3), log(3))
#' BandC=BoxAndCox(vec, -3)
#' BandC
#' BAC=unlist(BandC[3])
#' BAC
BoxAndCox = function(rawVect, minLambda)
{
  valCorel = c() #empty vector
  Lambda = c() #empty vector
  maxLambda=abs(minLambda)
  sortedVect= sort(rawVect, decreasing = FALSE)
  indx = c(1:length(rawVect))
  actualLambda=minLambda
  piBl = piBloom(indx, length(indx))
  qNrm = qnorm(piBl)

  while (actualLambda <= maxLambda)
  {
    sortedVectBC=subCalcBoxAndCox(sortedVect, actualLambda)
    corVal= cor(sortedVectBC, piBl )
    Lambda=append(Lambda,actualLambda)
    valCorel=append(valCorel,corVal)
    actualLambda =actualLambda + 0.001

  }
  dataFra= data.frame(Lambda, valCorel)
  LambdaMaxCalc=dataFra[dataFra['valCorel']== max(valCorel)]
  rawVectBCFinal=unlist(subCalcBoxAndCox(sortedVect, LambdaMaxCalc[1]))
  return (list(dataFra, LambdaMaxCalc, rawVectBCFinal,LambdaMaxCalc[1] ))
}
