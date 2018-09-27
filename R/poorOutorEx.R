poorOutorEx = function(dat, vars, ros, ord=TRUE){
  require(dplyr)
  datReduced = select(dat, one_of(vars))
  if(ros == 0)
    likOrd = lapply(datReduced, function(x)
      factor(x,
             levels = c(1,2,3,4),
             labels = c("Poor", "Good", 
                        "Very good", "Outstanding"),
             ordered=ord)
    )
  else {
    datReduced = select(dat, one_of(vars))
    likOrd = lapply(datReduced, function(x)
      factor(x,
             levels = c(1,2,3,4),
             labels = c("Poor", "Fair",
                        "Good", "Excellent"),
             ordered=ord)
    )  
  }
  likOrd = as.data.frame(likOrd)
  dat[,vars] = likOrd
}