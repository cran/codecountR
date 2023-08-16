#' zScore
#'
#' @param nameDframe a data.frame
#'
#' @return a data.frame
#' @importFrom stats sd
#' @export
#'
#' @examples
#' j=c(10,14,56,30,58,78,99,1)
#' k=c(10,12,14,16,18,20,22,24)
#' x=data.frame(j,k)
#' xZsc=zScore(x)
#' xZsc
zScore =function(nameDframe)
{
    for (i in 1:ncol(nameDframe))
    {
      mea=mean(nameDframe[,i])
      std=sd(nameDframe[,i])
      for (j in 1:nrow(nameDframe))
      {
        nameDframe[j,i]<- (nameDframe[j,i]-mea)/std
      }
    }
    return(nameDframe)
}
