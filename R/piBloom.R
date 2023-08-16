#' piBloom
#'
#' @param nameVect a vector
#' @param sSize a number
#'
#' @return a vector
#' @export
#'
#' @examples
#' vec=c(1,2,3)
#' piBloom(vec, length(vec))
piBloom=function(nameVect, sSize)
{
  for (j in 1:length(nameVect))
  {
    nameVect[j]<- (nameVect[j]-0.375)/(sSize +0.25)
  }
  return (nameVect)
}
