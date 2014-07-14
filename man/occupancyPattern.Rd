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
a percentile of the residence times of items to exclude from the return. See more information in Details
}
  \item{countlast}{
if true, then the last then items are treated as if they depart at the end of the depart time stamp. See more information in Details
}
}

\details{
fillup should be used when there are more than 0 items in the system, or the initial conditions of the system are unknown. For example a value of 0.95 indicates a 95\% chance that any items that were in the system at time 0 have departed and will not be included in the occupancy count.

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
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (x, ...) 
{
    UseMethod("occupancyPattern", x)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
