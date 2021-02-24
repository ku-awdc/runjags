\name{testjags}

\alias{testjags} \alias{testJAGS}

\title{Analyse the System to Check That JAGS Is Installed}

\description{

Test the users system to determine the operating system, version of R
installed, and version of JAGS installed.  Some information is collected
from other functions such as .platform and Sys.info.  Used by the
run.jags function.

}

\usage{

testjags(jags=runjags.getOption('jagspath'), silent=FALSE)}

\arguments{

\item{jags}{the system call or path for activating JAGS.  Default calls
findjags() to attempt to locate JAGS on your system automatically.  In
unix the system call should always be 'jags', in Windows a path to the
JAGS executable or the enclosing /bin or /JAGS folder is required.}

\item{silent}{should on-screen feedback be suppressed?  Default FALSE.}

}

\value{A named list of values containing information about the JAGS
installs found on the user's system (returned invisibly).}

\seealso{

\code{\link{run.jags}},

\code{\link{findjags}}

}


\examples{ 
# Run the function to determine if JAGS is installed: 
testjags()
testjags('some/jags/path') 
}

\keyword{methods}