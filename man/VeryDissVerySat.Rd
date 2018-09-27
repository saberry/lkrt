\name{VeryDissVerySat}
\alias{VeryDissVerySat}
\title{Very dissatisfied to Very satisfied
}
\description{
Gives "very dissatisfied" to "very satisfied" response option labels to variables.
}
\usage{
VeryDissVerySat(dat, vars, ros, ord=TRUE)
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
             labels = c("Very dissatisfied", "Dissatisfied", "Somewhat dissatisfied",
                        "Neither dissatisfied nor satisfied", "Somewhat satisfied",
                        "Satisfied", "Very satisfied"),
             ordered=ord)
    )
  else {
    datReduced = select(dat, one_of(vars))
    likOrd = lapply(datReduced, function(x)
      factor(x,
             levels = c(1,2,3,4,5),
             labels = c("Very dissatisfied", "Dissatisfied",
                        "Neither dissatisfied nor satisfied",
                        "Satisfied", "Very satisfied"),
             ordered=ord)
    )  
  }
  likOrd = as.data.frame(likOrd)
  dat[,vars] = likOrd
}
}
\keyword{~likert}
\keyword{~survey}