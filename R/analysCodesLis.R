#' analysCodesList
#'
#' @param dataS a character
#' @param codesLis a character
#'
#' @return a list
#' @export
#'
#' @examples
#' codes=list("@@essai@@","@@test@@")
#' data = "this is an example @@essai@@, a bit long @@essai@@ text"
#' Result=analysCodesList(data,codes)
#' Result
analysCodesList = function(dataS,codesLis)
{
  freq= c() #empty vector
  for (cod in codesLis)
  {
    nbCd=codeCount(dataS,cod)
    freq=append(freq,nbCd)

  }
  nbCodesLis=data.frame(codesLis, freq)
  nbCodesLis=nbCodesLis[order(nbCodesLis$freq,decreasing=TRUE),] #sorting DESC
  return (nbCodesLis)
}
