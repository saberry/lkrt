\name{poorOutorEx}
\alias{poorOutorEx}
\title{Poor to Outstanding/Excellent}
\description{
Gives "poor" to "outstanding" or "excellent" response option labels to variables.
}
\usage{
poorOutorEx(dat, vars, ros, ord=TRUE)
}
\arguments{
  \item{dat}{
the data frame
}
  \item{vars}{
a single variable or a list of multiple variables  
}
  \item{ros}{
which label set to use; 0 will return "poor", "good", "very good", and "outstanding"; anything else will return "poor", "fair", "good", "excellent"  
}
  \item{ord}{
logical indicating whether the items should be ordinal (TRUE:default) or factor (FALSE)  
}
}
\value{
This function will return response option labels for ordered factors.
}
\author{Seth Berry, Michael Clark}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--  or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function(dat, vars, ros, ord=TRUE){
  require(dplyr)
  datReduced = select(dat, one_of(vars))
  if(ros == 0)
    likOrd = lapply(datReduced, function(x)
      factor(x,
             levels = c(1,2,3,4),
             labels = c("Poor", "Good", 
                        "Very good", "Outstanding"),
             ordered=ord)
    )
  else {
    datReduced = select(dat, one_of(vars))
    likOrd = lapply(datReduced, function(x)
      factor(x,
             levels = c(1,2,3,4),
             labels = c("Poor", "Fair",
                        "Good", "Excellent"),
             ordered=ord)
    )  
  }
  likOrd = as.data.frame(likOrd)
  dat[,vars] = likOrd
}
}
\keyword{~likert}
\keyword{~survey}