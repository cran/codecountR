#' bootStrap
#'
#' @param nameDframe a data.frame
#' @param grpSize a number
#' @return a matrix
#' @importFrom stats runif
#' @export
#'
#' @examples
#' j=c(10,14,56,30,58,78,99,1)
#' k=c(10,12,14,16,18,20,22,24)
#' x=data.frame(j,k)
#' res=bootStrap(x,5)
#' res
bootStrap=function(nameDframe, grpSize)
{
	tempVect=c()
	out=matrix(data=NA, nrow = 0, ncol = ncol(nameDframe))

	for (j in 1:grpSize)
	{
		dice=runif(1, 1, nrow(nameDframe))
		tempVect=unlist(nameDframe[dice,], use.names = FALSE)
		out=rbind(out, as.numeric(tempVect))
		out
		tempVect=c()
	}
colnames(out)=colnames(nameDframe)
return(out)
}
