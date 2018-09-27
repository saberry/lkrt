\name{noGreatImportance}
\alias{noGreatImportance}
\title{Not at all to A great deal
}
\description{
Provides the following response options: no importance, little importance, some importance, much importance, great importance
}
\usage{
noGreatImportance(dat, vars, ord=TRUE)
}
\arguments{
  \item{dat}{
the data frame
}
  \item{vars}{
a single variable or a list of multiple variables  
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
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function(dat, vars, ord=TRUE){
  require(dplyr)
  datReduced = select(dat, one_of(vars))
    likOrd = lapply(datReduced, function(x)
      factor(x,
             levels = c(1,2,3,4,5),
             labels = c("No importance", "Little importance", "Some importance",
                        "Much importance", "Great importance"),
             ordered=ord)
    )
  likOrd = as.data.frame(likOrd)
  dat[,vars] = likOrd
}
}
\keyword{~likert}
\keyword{~survey}