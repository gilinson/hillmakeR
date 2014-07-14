\name{processData}
\alias{processData}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
processData
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
processData(df, ArrivalCol, DepartureCol, factors = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{df}{
%%     ~~Describe \code{df} here~~
}
  \item{ArrivalCol}{
%%     ~~Describe \code{ArrivalCol} here~~
}
  \item{DepartureCol}{
%%     ~~Describe \code{DepartureCol} here~~
}
  \item{factors}{
%%     ~~Describe \code{factors} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
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
function (df, ArrivalCol, DepartureCol, factors = NULL) 
{
    returnList <- list()
    if (!identical(class(df[, ArrivalCol]), c("POSIXct", "POSIXt"))) 
        stop(paste("ArrivalCol '", ArrivalCol, "' is class ", 
            class(ArrivalCol), ". Must be class POSIXct POSIXt.", 
            sep = ""))
    if (!identical(class(df[, DepartureCol]), c("POSIXct", "POSIXt"))) 
        stop(paste("DepartureCol '", DepartureCol, "' is class ", 
            class(DepartureCol), ". Must be class POSIXct POSIXt.", 
            sep = ""))
    df$LOS <- as.numeric(df[, DepartureCol] - df[, ArrivalCol], 
        units = "mins")
    badrecords <- which(df$LOS <= 0 | is.na(df$LOS))
    if (length(badrecords >= 1)) {
        warning(paste(length(badrecords), " removed because occupancy duration is either negative or NA", 
            sep = ""))
        df <- df[-badrecords, ]
    }
    returnList$ArrivalCol <- ArrivalCol
    returnList$DepartureCol <- DepartureCol
    returnList$df <- df
    returnList$factors <- factors
    attr(returnList, "class") <- "processData"
    return(returnList)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
