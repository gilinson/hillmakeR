aggregate.hillmakeRpattern<-function(pattern, fields = NULL, mean = TRUE, quantiles = seq(from = 0, to = 1, by = .01)) {
  
  # upfront checks
  if(!class(pattern) == "hillmakeRpattern")
    stop("pattern must be class hillmakeRpattern")
  
  returnList<-list() # initialize return object
  
  Formula<- "freq ~ yday + year" # initialize aggregation formula
  
  
  # Build formula
  returnList$factors <- pattern$factors[pattern$factors %in% fields] # what are the factors?
  
 #add fields,
 for(i in fields){
    Formula<-paste(Formula, " + ", i, sep = "")
  }
  
  returnList$formula <-Formula
  aggFrame<-aggregate(formula = formula(Formula), data = pattern$df, FUN = "sum")
  #aggFrame$freq<-aggFrame$freq
  dfunordered<-ddply(aggFrame, fields, function (df2) c(mean = mean(df2$freq), quantile(df2$freq, quantiles)), .drop = FALSE)
  dfunordered[is.na(dfunordered)]<-0 # replace NAs from ddply (.drop will result in some)
  # Reorder colomns (this helps the plotting function work correctly)
  timeCols<-fields[!(fields %in% pattern$factors)]
  firstCols<-c(timeCols, returnList$factors)
  returnList$df<-dfunordered[,c(firstCols, names(dfunordered)[!(names(dfunordered) %in% firstCols)])]
  #returnList$df<-returnList$df[order(returnList$df[,firstCols]),]
  outputOrder<-do.call(order, as.list(returnList$df))
  returnList$df<-returnList$df[outputOrder,] # reorder colomns
  #To-Do - set up for arrivals and depatures ??
  
  attr(returnList, "class") <- "hillmakeRpatternStats"
  return(returnList)
    
}