#' normMinMax
#'
#' @param nameDframe a data.frame
#'
#' @return a data.frame
#' @export
#'
#' @examples
#' j=c(10,14,56,30,58,78,99,1)
#' k=c(10,12,14,16,18,20,22,24)
#' x=data.frame(j,k)
#' xMinMax=normMinMax(x)
#' xMinMax
normMinMax =function(nameDframe)
{
    for (i in 1:ncol(nameDframe))
    {
      mini=min(nameDframe[,i])
      maxi=max(nameDframe[,i])
      for (j in 1:nrow(nameDframe))
      {
        nameDframe[j,i]<- (nameDframe[j,i]-mini)/(maxi-mini)
      }
    }
    return(nameDframe)
}
