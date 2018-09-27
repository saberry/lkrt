\name{StrongDisStrongAgr}
\alias{StrongDisStrongAgr}
\title{Strongly disagree to Strongly agree
}
\description{
Gives "strongly disagree" to "strongly agree" response option labels to variables.
}
\usage{
StrongDisStrongAgr(dat, vars, ros, ord=TRUE)
}
\arguments{
  \item{dat}{
the data frame
}
  \item{vars}{
a single variable or a list of multiple variables  
}
  \item{ros}{
the number of response options; currently supports 5 or 7  
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
function(dat, vars, ros, ord=TRUE){
  require(dplyr)
  datReduced = select(dat, one_of(vars))
  if(ros == 7)
    likOrd = lapply(datReduced, function(x)
      factor(x,
             levels = c(1,2,3,4,5,6,7),
             labels = c("Strongly disagree", "Disagree", "Somewhat disagree",
                        "Neither disagree nor agree", "Somewhat agree",
                        "Agree", "Strongly agree"),
             ordered=ord)
    )
  else {
    datReduced = select(dat, one_of(vars))
    likOrd = lapply(datReduced, function(x)
      factor(x,
             levels = c(1,2,3,4,5),
             labels = c("Strongly disagree", "Disagree",
                        "Neither disagree nor agree",
                        "Agree", "Strongly agree"),
             ordered=ord)
    )  
  }
  likOrd = as.data.frame(likOrd)
  dat[,vars] = likOrd
}
}
\keyword{~likert}
\keyword{~survey}