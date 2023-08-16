#' subCalcYeoAndJohnson
#'
#' @param Vect a vector
#' @param lambda a number
#'
#' @return a vector
#' @export
#'
#' @examples
#' vec=rlnorm(100, log(3), log(3))
#' subCalcYeoAndJohnson(vec, -3)
#' #only one lamba value tested
subCalcYeoAndJohnson = function(Vect, lambda)
{
  transformVect = c() #empty vector
  for (value in Vect)
  {
    calcVal=0

    if (value >= 0)
    {
      if (lambda == 0)
      {
        calcVal = log(value )+1
      }

      else
      {
        calcVal = (((value +1)^lambda) - 1)/lambda
      }
    }
    else
    {
      if (lambda == 2)
      {
        calcVal = -1 * (log(-value +1))
      }
      else
      {
        calcVal = -1 * (((-value +1)^(2-lambda))-1)/(2-lambda)
      }
    }

    transformVect = append(transformVect,calcVal)
  }
  return (transformVect)
}
