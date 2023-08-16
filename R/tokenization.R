#' tokenization
#'
#' @param txtFile a character
#'
#' @return a list
#' @export
#'
#' @examples
#' theFile =system.file("ExText.txt", package = "codecountR")
#' data=tokenization(theFile)
tokenization = function(txtFile)
{
  dataSet = scan(txtFile, character(), quote = "")
  return (dataSet)
}
