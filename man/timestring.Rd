\name{timestring}

\alias{timestring}

\title{Calculate the Elapsed Time in Sensible Units}

\description{

Function to calculate the elapsed time between 2 time periods (in
seconds), or to calculate a number of seconds into a time measurement in
more sensible units.

}

\usage{

timestring(time1, time2=NA, units=NA, show.units=TRUE)

}

\arguments{

\item{time1}{either the time index (from Sys.time()) at the start of the
time period, a length of time in seconds, or an object of class 'difftime'.}

\item{time2}{either the time index (from Sys.time()) at the end of the
time period, or missing data if converting a single length of time. 
Default NA.}

\item{units}{either missing, in which case a sensible time unit is
chosen automatically, or one of 's', 'm', 'h', 'd', 'w', 'y' to force a
specific unit.  Default NA.}

\item{show.units}{if TRUE, then the time is returned with units, if
FALSE then just an integer is returned.  Default TRUE.}

}

\value{A time measurement, with or without units.

}

\seealso{

\code{\link{Sys.time}}

}



\examples{

# time how long it takes to complete a task:

pre.time <- Sys.time()
Sys.sleep(2)  # PROCESS TO TIME
post.time <- Sys.time()
timestring(pre.time, post.time)

# Convert 4687 seconds into hours:

timestring(4687, units='hours', show.units=FALSE)

}

\keyword{methods}