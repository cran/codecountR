#' congruGen
#'
#' @param seed a number
#' @param a a number
#'
#' @return a list
#' @export
#'
#' @examples
#' seed = 123456789
#' for(i in 1: 10) {
#' Z=congruGen(seed)
#' seed=Z$seedUpdate
#' num=Z$aleaNum
#' print(num)
#' }
#'
congruGen =function(seed, a)
{
  if(missing(a))
  {
    a = 48271
  }

  m = (2^31)-1

  aleaNum = (a * seed) %% m
  seedUpdate = aleaNum
  aleaNum = aleaNum / m
  return(list(aleaNum=aleaNum, seedUpdate=seedUpdate))
}

