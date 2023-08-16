#' loadCodes
#'
#' @param txtFile a character
#'
#' @return a list
#' @export
#'
#' @examples
#' theFile =system.file("codesList.txt", package = "codecountR")
#' data=loadCodes(theFile)
loadCodes = function(txtFile)
{
  dataSet = scan(txtFile, character(), quote = "")
  return (dataSet)
}
