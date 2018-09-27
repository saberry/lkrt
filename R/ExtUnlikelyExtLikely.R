ExtUnlikelyExtLikely = function(dat, vars, ros, ord=TRUE){
  require(dplyr)
  datReduced = select(dat, one_of(vars))
  if(ros == 7)
    likOrd = lapply(datReduced, function(x)
      factor(x,
             levels = c(1,2,3,4,5,6,7),
             labels = c("Extremely unlikely", "Quite unlikely", "Slightly unlikely",
                        "Neither unlikely nor likely", "Slightly likely",
                        "Quite likely", "Extremely likely"),
             ordered=ord)
    )
  else {
    datReduced = select(dat, one_of(vars))
    likOrd = lapply(datReduced, function(x)
      factor(x,
             levels = c(1,2,3,4,5),
             labels = c("Extremely unlikely", "Unlikely",
                        "Neither unlikely nor likely",
                        "Likely", "Extremely likely"),
             ordered=ord)
    )  
  }
  likOrd = as.data.frame(likOrd)
  dat[,vars] = likOrd
}