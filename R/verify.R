#'verify
#'
#' @param lines character
#' @param code1 character
#' @param code2 character
#'
#' @export
#'
#' @examples
#' lines ="Companies can boost responsiveness @@performance@@ by digital @@digital@@."
#' code1 = "@@performance@@"
#' code2 = "@@digital@@"
#' verify(lines,code1,code2)
verify= function(lines, code1, code2) {

  # Filtrer out lines containing code1
  linesCtCode1 = grep(code1, lines, value = TRUE)

  # Filter out the lines containing code2 from those containing code1
  linesCtBoth = grep(code2, linesCtCode1, value = TRUE)

  # Counting number of lines containing both
  numberLines = length(linesCtBoth)

  # print results
  cat(sprintf("###RESULT : %d coocurrences. %s - %s ### \n",numberLines, code1, code2))
  print(linesCtBoth )
}
