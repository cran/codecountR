#' medAbsDev
#'
#' @param nameDframe a data.frame
#'
#' @return a data.frame
#' @importFrom stats median
#' @export
#'
#' @examples
#' vec=rnorm(100, mean=10, sd=1)
#' vec2=rnorm(100, mean=15, sd=1)
#' df= data.frame(vec,vec2)
#' colnames(df) = c("vec", "vec2")
#' MAD=medAbsDev(df)
#' MAD
medAbsDev =function(nameDframe)
{
  med=c()
  mad=c()
  rawDf=data.frame(nameDframe)

    for (i in 1:ncol(nameDframe))
    {
      medi=median(nameDframe[,i])
      med=append(med,medi)
      for (j in 1:nrow(nameDframe))
      {
        nameDframe[j,i]<- abs((nameDframe[j,i]-med[i]))
      }
      madCal=median(nameDframe[,i])
      mad=append(mad,madCal)
    }

    for (i in 1:ncol(nameDframe))
    {

      for (j in 1:nrow(nameDframe))
      {
        nameDframe[j,i]<- ((rawDf[j,i]-med[i])/mad[i])
      }

    }
    return(nameDframe)

}
