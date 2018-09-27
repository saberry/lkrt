notExtremelyRelevant = function(dat, vars, ord=TRUE){
  require(dplyr)
  datReduced = select(dat, one_of(vars))
  likOrd = lapply(datReduced, function(x)
    factor(x,
           levels = c(1,2,3,4,5,6),
           labels = c("Not at all relevant", "Not very relevant", "Slightly relevant",
                      "Somewhat relevant", "Very relevant", "Extremely relevant"),
           ordered=ord)
  )
  likOrd = as.data.frame(likOrd)
  dat[,vars] = likOrd
}