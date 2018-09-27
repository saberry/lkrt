notAtAllGreatDeal = function(dat, vars, ord=TRUE){
  require(dplyr)
  datReduced = select(dat, one_of(vars))
    likOrd = lapply(datReduced, function(x)
      factor(x,
             levels = c(1,2,3,4,5),
             labels = c("Not at all", "Very little", "Somewhat",
                        "Quite a bit", "A great deal"),
             ordered=ord)
    )
  likOrd = as.data.frame(likOrd)
  dat[,vars] = likOrd
}