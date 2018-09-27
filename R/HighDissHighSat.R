HighDissHighSat = function(dat, vars, ros, ord=TRUE){
  require(dplyr)
  datReduced = select(dat, one_of(vars))
  if(ros == 7)
    likOrd = lapply(datReduced, function(x)
      factor(x,
             levels = c(1,2,3,4,5,6,7),
             labels = c("Highly dissatisfied", "Dissatisfied", "Somewhat dissatisfied",
                        "Neither dissatisfied nor satisfied", "Somewhat satisfied",
                        "Satisfied", "Highly satisfied"),
             ordered=ord)
    )
  else {
    datReduced = select(dat, one_of(vars))
    likOrd = lapply(datReduced, function(x)
      factor(x,
             levels = c(1,2,3,4,5),
             labels = c("Highly dissatisfied", "Dissatisfied",
                        "Neither dissatisfied nor satisfied",
                        "Satisfied", "Highly satisfied"),
             ordered=ord)
    )  
  }
  likOrd = as.data.frame(likOrd)
  dat[,vars] = likOrd
}