\name{occupancyPattern}
\alias{occupancyPattern}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
occupancyPattern
}
\description{
Calculates the number of items between time stamps using an efficient algorithm.
}
\usage{
occupancyPattern(startTimes, stopTimes, resolution = "min", fillup = NULL, countlast = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{startTimes}{
a vector of POSIXct objects representing the time that an item arrived
}

  \item{stopTimes}{
a vector of POSIXct objects representing the time that an item departed
}

  \item{resolution}{
size of buckets to calculte occupancy within. May be "sec" "min" "hour" or "day"
}

  \item{fillup}{
a percentile of the residence times of items to exclude from the return. Only either fillup or initial can be defined.
}

  \item{initial}{
the initial count of items in the system at time 0. Only either fillup or initial can be defined.
}

  \item{countlast}{
if true, then the last then items are treated as if they depart at the end of the depart time stamp. See more information in Details
}
}

\details{
fillup should be used when the initial conditions of the system are unknown. For example a value of 0.95 indicates a 95\% chance that any items that were in the system at time 0 have departed and will not be included in the occupancy count.

countlast determines how the stopTimes timestamps are treated. For example if an item has a startTime of 07/07/2014 and the endTime of 07/07/2014 with resulution = "day", then the item will be counted as in the system on 07/07/2014 with countlast = TRUE, but will not be counted as in the system with countlast = FALSE

}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
library(plyr) # need ddply
attach(hflights) # use hflights dataset

# determine how many planes are at airport by hour of day and hour of week
planeCount <- occupancyPattern(startTimes=Arrivals, stopTimes=Departures, resolution="min", fillup = 0.95)
planeCount$hour <- as.POSIXlt(planeCount$timeSequence)$hour

# use ddply to calculate summary stats
byHourOfDay <- ddply(planeCount, c("hour"), function(x) c(mean = mean(x$census), median = median(x$census), q90 = quantile(x$census, 0.9, names = FALSE)))

# display output graphically
plot(byHourOfDay$mean)

# Repeat by Carrier, wrapping ddply around call to hillmakeR functions
# This is the pattern to use whenever there are different types of items to count
planeCountbyCarrier <- ddply(hflights, "hflights.UniqueCarrier", function(x) occupancyPattern(startTimes=x$Arrivals, stopTimes=x$Departures, resolution="min", fillup = 0.95))

# add POSIXlt dateparts
planeCountbyCarrier$hour <- as.POSIXlt(planeCountbyCarrier$timeSequence)$hour

# Use ddply again for summary stats
byHourOfWeekAndCarrier <- ddply(planeCountbyCarrier, c("hflights.UniqueCarrier", "hour"), function(x) c(mean = mean(x$census), median = median(x$census), q90 = quantile(x$census, 0.9)))

plot(subset(byHourOfWeekAndCarrier, hflights.UniqueCarrier == "AA")$mean, ylim = c(0, 12.5), type = "o")
for (i in levels(hflights.UniqueCarrier)){
  lines(subset(byHourOfWeekAndCarrier, hflights.UniqueCarrier == i)$mean, col = sample(colours(), 1), type = "o" )
}

# example of using initial values instead of fillup

# make a dummy lookup table
initial.lookup <- data.frame(key = levels(hflights$hflights.UniqueCarrier), 
                            value = c(380, 164, 131, 74, 114, 161, 334, 106, 127, 498, 485, 158,  68, 100, 60))


planeCountbyCarrier <- ddply(hflights, "hflights.UniqueCarrier", 
                              function(x) occupancyPattern(startTimes=x$Arrivals, stopTimes=x$Departures, resolution="min", 
                              initial = initial.lookup[initial.lookup$key == x$hflights.UniqueCarrier[1], "value"]))



}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
