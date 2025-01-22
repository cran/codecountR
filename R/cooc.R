#' cooc
#'
#' @param lines character
#' @param code1 character
#' @param code2 character
#'
#' @return an integer
#' @export
#'
#' @examples
#' lines ="Companies can boost responsiveness @@performance@@ by digital @@digital@@."
#' code1 = "@@performance@@"
#' code2 = "@@digital@@"
#' res=cooc(lines, code1, code2)
#' print(res)
cooc = function(lines, code1, code2) {
  linesCode1 = grep(code1, lines, value = TRUE)
  linesBoth = grep(code2, linesCode1, value = TRUE)
  nbLines = length(linesBoth)
  return(nbLines)
}

