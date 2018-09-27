CompleteDisCompleteAgr = function(dat, vars, ros, ord=TRUE){
  require(dplyr)
  datReduced = select(dat, one_of(vars))
  if(ros == 7)
    likOrd = lapply(datReduced, function(x)
      factor(x,
             levels = c(1,2,3,4,5,6,7),
             labels = c("Completely disagree", "Disagree", "Somewhat disagree",
                        "Somewhat agree", "Agree", "Completely agree",
                        "NA"),
             ordered=ord)
    )
  else {
    datReduced = select(dat, one_of(vars))
    likOrd = lapply(datReduced, function(x)
      factor(x,
             levels = c(1,2,3,4,5),
             labels = c("Completely disagree", "Disagree",
                        "Neither disagree nor agree",
                        "Agree", "Completely agree"),
             ordered=ord)
    )  
  }
  likOrd = as.data.frame(likOrd)
  dat[,vars] = likOrd
}