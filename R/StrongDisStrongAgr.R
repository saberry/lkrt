StrongDisStrongAgr = function(dat, vars, ros, ord=TRUE){
  require(dplyr)
  datReduced = select(dat, one_of(vars))
  if(ros == 7){
    likOrd = lapply(datReduced, function(x)
      factor(x,
             levels = c(1,2,3,4,5,6,7),
             labels = c("Strongly disagree", "Disagree", "Somewhat disagree",
                        "Neither disagree nor agree", "Somewhat agree",
                        "Agree", "Strongly agree"),
             ordered=ord)
    )
  }
  else if(ros == 6){
    likOrd = lapply(datReduced, function(x)
      factor(x,
             levels = c(1,2,3,4,5,6),
             labels = c("Strongly disagree", "Disagree", "Slightly disagree",
                        "Slightly agree", "Agree", "Strongly agree"),
             ordered=ord)
    )
  }
  else if(ros == 7.1){
    likOrd = lapply(datReduced, function(x)
      factor(x,
             levels = c(1,2,3,4,5,6,7),
             labels = c("Strongly disagree", "Disagree", "Slightly disagree",
                        "Neither disagree nor agree",
                        "Slightly agree", "Agree", "Strongly agree"),
             ordered=ord)
    )
  }
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