processdata<-function(df, timeCols, factors = NULL){
  returnList<- list()
  # Build object
  
  for(i in timeCols){
    if(!identical(class(df[, i]), c("POSIXct","POSIXt")))
      stop(paste("timeCols element '", i,"' is class ", class(i),". Must be class POSIXct POSIXt.", sep = ""))
  }
  #   if(!identical(class(df[, ArrivalCol]), c("POSIXct","POSIXt")))
  #     stop(paste("ArrivalCol '", ArrivalCol,"' is class ", class(ArrivalCol),". Must be class POSIXct POSIXt.", sep = ""))
  #   
  #   # Build object
  #   if(!identical(class(df[, DepartureCol]), c("POSIXct","POSIXt")))
  #     stop(paste("DepartureCol '", DepartureCol,"' is class ", class(DepartureCol),". Must be class POSIXct POSIXt.", sep = ""))
  #   
  # Check that LOS is always positive, if not, remove bad rows and warn
  #   df$LOS<-as.numeric(df[, DepartureCol] - df[, ArrivalCol], units = "mins")
  #   badrecords<- which(df$LOS <= 0 | is.na(df$LOS))
  #   if(length(badrecords >= 1)) {
  #     warning(paste(length(badrecords), " removed because occupancy duration is either negative or NA", sep = ""))
  #     df<-df[-badrecords,]
  #   }
  
  # Convert factors to R factors
  for(i in factors){
    df[, i] <- factor(df[,i])
  }
  
  # Build return object
  returnList$timeCols <- timeCols
  returnList$df <- data.frame(df[,c(timeCols, factors)]) # Build return df
  names(returnList$df)<- c(timeCols, factors) # Set names of return df
  returnList$factors<- factors # Define factors in return object
  attr(returnList, "class") <- "processdata" # Set return object class
  
  return(returnList)
  
}

print.processdata<-function(pd){
  # Defines generic print function. Outputs data.frame that contains data
  print(pd$df)
}

# Get data.frame that describes units used by package
getdfAllowedUnits <- function(){
  unitname <- c("sec", "min", "hour", "day")
  conversion <- c(1, 60, 3600, 86400)
  returnFrame <- data.frame(unitname, conversion)
  return(returnFrame)
}

occupancyPatternFunc.processdata <- function(startTimes, stopTimes, resolution = "min", fillup = NULL, countlast = TRUE){
  #library.dynam("updateArray")
  #orginTime<-as.POSIXct('01/01/1970 00:00',format ="%m/%d/%Y %H:%M", tz = "GMT")
  
  dfAllowedUnits <- getdfAllowedUnits()
  # Check if resolution is an allowed unit
    if (!(resolution %in% dfAllowedUnits$unitname)) {
      allowedunitspaste <-  paste(dfAllowedUnits$unitname, collapse = ", ")
    stop(paste('Resolution can only be set to the following:', allowedunitspaste))
  }
  
  # Calculate LOS (length-of-stay) for each item, remove any negatives. units requires an S added to resolution
  LOS <- as.numeric(stopTimes - startTimes, units = paste(resolution, "s", sep = ""))
  badrecords <- which(LOS <= 0 | is.na(LOS))
  if(length(badrecords) > 0) {
    warning(paste(length(badrecords), " removed because duration is either negative or NA", sep = ""))
    startTimes <- startTimes[-badrecords]
    stopTimes<- stopTimes[-badrecords]
  }
  
  earliestTime <-  min(startTimes)
  latestTime <-  max(stopTimes)
  
  # Get conversion factor
  conversion <- dfAllowedUnits[dfAllowedUnits$unitname == resolution, "conversion"]
  
  startMin <- ceiling((as.numeric(startTimes) -  as.numeric(min(startTimes)))/conversion)
  stopMin  <- ceiling((as.numeric(stopTimes) -   as.numeric(min(startTimes)))/conversion)
  totalMin <- length(startMin)
  
  # consider if the last time unit should be counted (i.e., count the minute that something departs?)
  if(countlast){
    countlastpassed <- 0
  } else {
    countlastpassed <- 1
  }

  census<-.C("updateArray", size = as.integer(totalMin), starts = as.double(startMin), stops = as.double(stopMin), returnX = as.double(rep(0,max(stopMin) + 1)), countlast = as.integer(countlastpassed))$returnX
  
  timeSequence <- seq(from = trunc(earliestTime, units = resolution), to = trunc(latestTime,units = resolution), by = resolution)
  returnFrame <- as.data.frame(timeSequence)
  returnFrame <- cbind(returnFrame, census)
  
  #returnFrame$hour <- as.POSIXlt(returnFrame$dateHourList)$hour
  if(!(is.null(fillup))){
    # recalculate LOS (w/o removed records)
    LOS <- as.numeric(stopTimes - startTimes, units = paste(resolution, "s", sep = ""))
    timetoremove = quantile(LOS, fillup)
    warning(paste(timetoremove, " time steps removed based on fillup quantile"))
    returnFrame <- returnFrame[(timetoremove + 1):nrow(returnFrame),]
  }
  
  return(returnFrame)
}

occupancyPattern.processdata<-function(pd, startCol, endCol){
  
  # upfront checks
  if(!class(pd) == "processdata")
    stop("pd must be class processdata.")
  
  if(!startCol %in% pd$timeCols)
    stop("startCol must be defined as in timeCols.")
  
  if(!endCol %in% pd$timeCols)
    stop("stopCol must be defined as in timeCols.")
  
  returnList<-list()
  
  data<-pd$df
  
  # Check that LOS is always positive, if not, remove bad rows and warn
  data$LOS<-as.numeric(data[, endCol] - data[, startCol], units = "mins")
  badrecords<- which(data$LOS <= 0 | is.na(data$LOS))
  if(length(badrecords >= 1)) {
    warning(paste(length(badrecords), " removed because duration is either negative or NA", sep = ""))
    data<-data[-badrecords,]
  }
    
  dataCol<-ncol(data)
  dataRow<-nrow(data)
  orginTime<-as.POSIXct('01/01/1970 00:00',format ="%m/%d/%Y %H:%M", tz = "GMT")
  data<-rbind(data, rep(NA, ncol(data))) #Dummy Row
  data[(nrow(data)), startCol]<-min(data[,startCol], na.rm = TRUE)
  data[(nrow(data)), endCol]<-max(data[,endCol], na.rm = TRUE)
  totalMin<-sum(as.numeric((data[,endCol] - data[,startCol] + 1), units = "mins"))
  data[,startCol]<-as.numeric(data[,startCol], tz = "GMT", origin = orginTime)
  data[,endCol]<-as.numeric(data[,endCol], tz = "GMT", origin = orginTime)
  
  # Build Occupancy Matrix, where one row is created for each min that a patient is in the system
  OccupancyMatrix1<-matrix(nrow = totalMin, ncol = length(pd$factors) + 1)
  OccupancyMatrix2<-vector(length = totalMin)
  
  FactorMat<-data.matrix(as.data.frame(data[,pd$factors]))
  #class(FactorMat)<- "numeric" # Apparently data.matrix sometimes returns char
  
  if(!is.null(pd$factors)) {
    factorMap<-createFactorMap(data)
  }
  TimeMat<-as.matrix(data[,c(startCol, endCol)])
  rm(data)
  loc<-1
  for(i in 1:nrow(TimeMat)){
    TimeInSys = seq(from = TimeMat[i,1], to = TimeMat[i,2], by = 60)
    numOfDates<-length(TimeInSys)
    OccupancyMatrix1[loc:(loc + numOfDates -1),]<-cbind(matrix(rep(FactorMat[i,], numOfDates), nrow = numOfDates, byrow= TRUE),TimeInSys)
    loc<-loc + numOfDates
  }
  
  OccupancyMatrix1df<-data.frame(OccupancyMatrix1)
  names(OccupancyMatrix1df)<- c(pd$factors, "TimeInSys")
  class(OccupancyMatrix1df[,"TimeInSys"])<-c("POSIXct","POSIXt")
  OccupancyMatrix1df$hour<-as.POSIXlt(OccupancyMatrix1df$TimeInSys)$hour
  OccupancyMatrix1df$wday<-as.POSIXlt(OccupancyMatrix1df$TimeInSys)$wday
  OccupancyMatrix1df$year<-as.POSIXlt(OccupancyMatrix1df$TimeInSys)$year
  OccupancyMatrix1df$yday<-as.POSIXlt(OccupancyMatrix1df$TimeInSys)$yday
  returnMat<-count(OccupancyMatrix1df)
  returnMat$freq<- returnMat$freq - 1 # remove dummy row
  rm(OccupancyMatrix1df)
  
  if(!is.null(pd$factors)){
    returnList$df<-convertNumericToFactors(returnMat, factorMap)
  }else {
    returnList$df<-returnMat
  }
  
  returnList$factors <- pd$factors #copy factors foward
  
  attr(returnList, "class") <- "hillmakeRpattern"
  return(returnList)
  
}

occupancyPattern <- function (x, ...) {
  UseMethod("occupancyPattern", x)
}

timeStampPattern.processdata<-function(pd, Col){
  # function to shape arrival data so that it is easy to extract descriptive stats
  # need to fix....
  
  # upfront checks
  if(!class(pd) == "processdata")
    stop("pd must be class processdata.")
  
  if(!(Col %in% pd$timeCols ))
    stop("Col must be a column of timestamps, defined using processdata.")
  
  # Check that LOS is always positive, if not, remove bad rows and warn
  badrecords<- which(is.na(pd$df[,Col]))
  if(length(badrecords >= 1)) {
    warning(paste(length(badrecords), " removed because timestamps are NA or missing.", sep = ""))
    pd$df<-pd$df[-badrecords,]
  }
  
  returnList<-list()
  
  StartDate<- min(pd$df[,Col]) # Determine the Earliest DateTime in Dataset
  EndDate<- max(pd$df[,Col]) # Determine the Latest DateTime in Dataset
  
  # Build vector of Dates
  dateHourList = seq(from = trunc(StartDate, units = "min"), to = trunc(EndDate,units = "min"), by = "min")  # list of min dates
  
  # Truncate Dates to Just Mins
  countingFrame<-trunc(pd$df[,Col], units = "min")
  
  # If factors are within the Process Data, add them to the data.frame of truncated dates
  if(!is.null(pd$factors)) {
    countingFrame<-data.frame(countingFrame, pd$df[,pd$factors])
    names(countingFrame)[2:ncol(countingFrame)]<- pd$factors
  }
  
  # Count Arrivals
  frequencyCounts<-count(data.frame(countingFrame))
  
  # Map counts into vector of dates and factors (0's would not be known to plyr)
  rownamesList<-lapply(X=data.frame(pd$df[,pd$factors]),FUN = function (x) levels(x))
  names(rownamesList)<-pd$factors
  
  rownamesList$dateHourList<-dateHourList
  allCombinations<-expand.grid(rownamesList)
  frequencyCounts<-merge(allCombinations,frequencyCounts, by.x=c("dateHourList", pd$factors), by.y=c("countingFrame", pd$factors), all.x = TRUE)
  frequencyCounts[is.na(frequencyCounts$freq),"freq"] <- 0
  
  
  # Add some additional fields for segmenting data
  hour<-as.POSIXlt(frequencyCounts$dateHourList)$hour
  wday<-as.POSIXlt(frequencyCounts$dateHourList)$wday
  yday<-as.POSIXlt(frequencyCounts$dateHourList)$yday
  year<-as.POSIXlt(frequencyCounts$dateHourList)$year
  
  # Prepare return
  returnList$factors <- pd$factors #copy factors foward
  returnList$df<-data.frame(frequencyCounts, hour, wday, yday, year)
  attr(returnList, "class") <- "hillmakeRpattern"
  return(returnList)
  
}

timeStampPattern <- function (x, ...) {
  UseMethod("timeStampPattern", x)
}

createFactorMap<-function(df){
  # FactorMap<-setClass("factorMap", representation(factorMat = "matrix", mapping = "list"))
  #result<-FactorMap()
  factorNames<-names(df)
  result <- vector("list", length(factorNames))
  names(result)<-factorNames
  
  for(i in 1:length(factorNames)){
    fac<-factor(df[,factorNames[i]])
    result[[factorNames[i]]]<-attributes(fac)$levels
  }
  
  #result@factorMat<-data.matrix(df)
  
  return(result)
  
}

convertNumericToFactors<-function(df, factormap){
  
  factorNames<-names(df)
  listdata <- vector("list", length(factorNames))
  names(listdata)<-factorNames
  
  
  for(i in factorNames){
    if(i == "TimeInSys"){
      datedata<-df[[i]]
      class(datedata)<-c("POSIXct","POSIXt")
      listdata[[i]]<- datedata
    } else if(i == "freq" | i == "hour" | i == "wday" | i == "yday" | i == "year") {
      listdata[[i]]<- df[[i]]
    }else {
      listdata[[i]]<- factor(df[[i]], labels = factormap[[i]])
    }
  }
  
  returnFrame<-as.data.frame(listdata)
  return(returnFrame)
  
}