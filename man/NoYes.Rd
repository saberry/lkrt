\name{NoYes}
\alias{NoYes}
\title{No/Yes}
\description{
Gives "no" and "yes" response option labels to variables.
}
\usage{
NoYes(dat, vars, ros, ord=FALSE)
}
\arguments{
  \item{dat}{
the data frame
}
  \item{vars}{
a single variable or a list of multiple variables
}
  \item{ros}{
0 denotes that the values are 0 (no) and 1 (yes); 1 denotes that the values are 1 (no) and 2 (yes)
}
  \item{ord}{
logical indicating whether the items should be ordinal (TRUE) or factor (FALSE:Default)
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
function(dat, vars, ros, ord=FALSE){
  require(dplyr)
  datReduced = select(dat, one_of(vars))
  if(ros == 0)
    likOrd = lapply(datReduced, function(x)
      factor(x,
             levels = c(0,1),
             labels = c("No", "Yes"),
             ordered=ord)
    )
  else {
    datReduced = select(dat, one_of(vars))
    likOrd = lapply(datReduced, function(x)
      factor(x,
             levels = c(1,2),
             labels = c("No", "Yes"),
             ordered=ord)
    )  
  }
  likOrd = as.data.frame(likOrd)
  dat[,vars] = likOrd
}
}
\keyword{~likert}
\keyword{~survey}
