neverAlways = function(dat, vars, ros, ord=TRUE){
  require(dplyr)
  datReduced = select(dat, one_of(vars))
  if(ros == 0)
    likOrd = lapply(datReduced, function(x)
      factor(x,
             levels = c(1,2,3,4,5,6),
             labels = c("Never", "Seldom", "Sometimes",
                        "Often", "Usually", "Always"),
             ordered=ord)
    )
  else {
    datReduced = select(dat, one_of(vars))
    likOrd = lapply(datReduced, function(x)
      factor(x,
             levels = c(1,2,3,4,5,6),
             labels = c("Never/Almost Never", "Seldom/Rarely", 
                        "Sometimes", "Often", 
                        "Usually/Most of the time", "Almost Always/Always"),
             ordered=ord)
    )  
  }
  likOrd = as.data.frame(likOrd)
  dat[,vars] = likOrd
}