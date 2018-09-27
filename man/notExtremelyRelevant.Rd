\name{notExtremelyRelevant}
\alias{notExtremelyRelevant}
\title{Not at all to A great deal
}
\description{
Provides the following response options: Not at all relevant, not very relevant, slightly relevant, somewhat relevant, very relevant, and extremely relevant
}
\usage{
notExtremelyRelevant(dat, vars, ord=TRUE)
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
           levels = c(1,2,3,4,5,6),
           labels = c("Not at all relevant", "Not very relevant", "Slightly relevant",
                      "Somewhat relevant", "Very relevant", "Extremely relevant"),
           ordered=ord)
  )
  likOrd = as.data.frame(likOrd)
  dat[,vars] = likOrd
}
}
\keyword{~likert}
\keyword{~survey}