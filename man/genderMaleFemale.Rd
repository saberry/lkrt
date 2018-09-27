\name{genderMaleFemale}
\alias{genderMaleFemale}
\title{Male and Female Response Option}
\description{
Gives "male" and "female" response option labels to variables.}
\usage{
genderMaleFemale(dat, vars, ros, ord=FALSE)}
\arguments{
  \item{dat}{
the data frame
}
  \item{vars}{
a single variable or a list of multiple variables
}
  \item{ros}{
0 denotes that the values are 0 (male) and 1 (female); 1 denotes that the values are 1 (male) and 2 (female)
}
  \item{ord}{
logical indicating whether the items should be ordinal (TRUE) or factor (FALSE:Default)
}
}
\value{
This function will return response option labels for factors. 
}

\author{Seth Berry, Michael Clark}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--  or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function(dat, vars, ros, ord=FALSE){
  require(dplyr)
  datReduced = select(dat, one_of(vars))
  if(ros == 0)
    likOrd = lapply(datReduced, function(x)
      factor(x,
             levels = c(0,1),
             labels = c("male", "female"),
             ordered=ord)
    )
  else {
    datReduced = select(dat, one_of(vars))
    likOrd = lapply(datReduced, function(x)
      factor(x,
             levels = c(1,2),
             labels = c("male", "female"),
             ordered=ord)
    )  
  }
  likOrd = as.data.frame(likOrd)
  dat[,vars] = likOrd
}
}
\keyword{~likert}
\keyword{~survey}