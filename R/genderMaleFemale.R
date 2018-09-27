genderMaleFemale = function(dat, vars, ros, ord=FALSE){
  require(dplyr)
  datReduced = select(dat, one_of(vars))
  if(ros == 0)
    likOrd = lapply(datReduced, function(x)
      factor(x,
             levels = c(0,1),
             labels = c("Male", "Female"),
             ordered=ord)
    )
  else {
    datReduced = select(dat, one_of(vars))
    likOrd = lapply(datReduced, function(x)
      factor(x,
             levels = c(1,2),
             labels = c("Male", "Female"),
             ordered=ord)
    )  
  }
  likOrd = as.data.frame(likOrd)
  dat[,vars] = likOrd
}