#' robustScal
#'
#' @param nameDframe a data.frame
#'
#' @return a data.frame
#' @importFrom stats IQR
#' @importFrom stats median
#' @export
#'
#' @examples
#' j=c(10,14,56,30,58,78,99,1)
#' k=c(10,12,14,16,18,20,22,24)
#' x=data.frame(j,k)
#' xRsc=robustScal(x)
#' xRsc
robustScal =function(nameDframe)
{
  for (i in 1:ncol(nameDframe))
  {
    med=median(nameDframe[,i])
    iqr=IQR(nameDframe[,i])
    for (j in 1:nrow(nameDframe))
    {
      nameDframe[j,i]<- (nameDframe[j,i]-med)/iqr
    }
  }
  return(nameDframe)
}



