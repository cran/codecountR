#' testPairs
#'
#' @param listCodes character
#' @param lines character
#'
#' @return a list
#'
#' @export
#'
#' @examples
#' #Co-occurrences computed line by line in the file. Structure the file accordingly.
#' #Multiple identical pairs on one line count as one unit.
#' lines =c("Companies can boost responsiveness @@performance@@ by digital @@digital@@.",
#'   "softwares @@digital@@ may reduce response time @@performance@@ improving @@satisfaction@@.")
#' listCodes=c("@@satisfaction@@", "@@digital@@", "@@performance@@")
#' coocurences = testPairs(listCodes, lines)
#' print(coocurences$matrix)
#' #save to file
#' #nameFile = paste("CooccurrenceMatrix_",format(Sys.time(),"%d_%m_%Y-%Hh%Mm%Ss"),".csv",sep = "")
#' #write.csv(coocurences$matrix, nameFile, row.names = TRUE)

testPairs = function(listCodes, lines) {
  # Initialize an empty matrix
  n = length(listCodes)
  coocMatrix = matrix(0, nrow = n, ncol = n, dimnames = list(listCodes, listCodes))

  # Loops on all combinations without duplicates
  for (i in 1:(n-1)) {			#avoid duplicates, identical pairs and reduce iterations
    for (j in (i+1):n) {
      code1 = listCodes[i]
      code2 = listCodes[j]

      # Compute the coocurrence for this pair
      coocValue = cooc(lines, code1, code2)

      # Make the matrix symmetrical
      coocMatrix[i, j] = coocValue
      coocMatrix[j, i] = coocValue
    }
  }
  coocurences = list(matrix = coocMatrix)
  return(coocurences)
}
