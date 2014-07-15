\name{occupancy}
\alias{occupancy}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
occupancy
}
\description{
Calculates the number of items between time stamps using an efficient algorithm.
}
\usage{
occupancy(startTimes, stopTimes, resolution = "min", fillup = NULL, countlast = TRUE)
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
A data.frame with two coloumns
 \item{times}{A datetime, class POSIXct}
 \item{counts}{The number of items in the system at current time}
%% ...
}
\references{
Concept based on software written by MW Isken.
See simular work at http://www.hselab.org/tags/hillmaker.
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

# determine how many planes are at airport each minute
planeCount <- occupancy(startTimes=Arrivals, stopTimes=Departures, resolution="min", fillup = 0.95)

# determine how many planes are at airport by hour of day using ddply for summary stats
planeCount$hour <- as.POSIXlt(planeCount$times)$hour
byHourOfDay <- ddply(planeCount, c("hour"), function(x) c(mean = mean(x$counts), median = median(x$counts), q90 = quantile(x$counts, 0.9, names = FALSE)))

# display output graphically
plot(byHourOfDay$mean, type = "o")

# Repeat by Carrier, wrapping ddply around call to hillmakeR functions
# This is the pattern to use whenever there are different types of items to count
planeCountbyCarrier <- ddply(hflights, "Carrier", function(x) occupancy(startTimes=x$Arrivals, stopTimes=x$Departures, resolution="min", fillup = 0.95))

# determine how many planes are at airport by hour of day
planeCountbyCarrier$hour <- as.POSIXlt(planeCountbyCarrier$times)$hour
byHourAndCarrier <- ddply(planeCountbyCarrier, c("Carrier", "hour"), function(x) c(mean = mean(x$counts), median = median(x$counts), q90 = quantile(x$counts, 0.9)))

plot(subset(byHourAndCarrier, Carrier == "AA")$mean, ylim = c(0, 12.5), type = "o")
for (i in levels(Carrier)){
  lines(subset(byHourAndCarrier, Carrier == i)$mean, col = sample(colours(), 1), type = "o" )
}

# example of using initial values instead of fillup option with serveral types of items

# make a dummy lookup table
initial.lookup <- data.frame(key = levels(hflights$Carrier), 
                            value = c(380, 164, 131, 74, 114, 161, 334, 106, 127, 498, 485, 158,  68, 100, 60))


planeCountbyCarrier <- ddply(hflights, "Carrier", 
                              function(x) occupancy(startTimes=x$Arrivals, stopTimes=x$Departures, resolution="min", 
                              initial = initial.lookup[initial.lookup$key == x$Carrier[1], "value"]))



}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
