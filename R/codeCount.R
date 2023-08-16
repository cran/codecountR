#' codeCount
#'
#' @param dataSet a character
#' @param code a character
#'
#' @return a number
#' @export
#'
#' @examples
#' data = "this is an example @@essai@@"
#' codeCount(data, "@@essai@@") #number of lines containing the chain
codeCount = function(dataSet,code)
{
    count=length(grep(code, dataSet))
    return (count)

}
