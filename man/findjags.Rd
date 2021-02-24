\name{findjags}
\alias{findjags}
\alias{findJAGS}

\title{Attempt to Locate a JAGS Install}
\description{
   Search the most likely locations for JAGS to be installed on the
   users system, based on the operating system, and return the most
   likely path to try.  Where multiple installs exist, findjags will
   attempt to return the path to the install with the highest version
   number.  For Unix systems, calling jags using 'jags' requires the
   jags binary to be in the search path, which may be specified in your
   user '.Profile' if necessary (the JAGS executable is also looked for
   in the default install location of /usr/local/bin/jags if popen
   support is enabled).
}
\usage{

findjags(ostype = .Platform$OS.type, look_in = NA, ...)

}
\arguments{
   \item{ostype}{the operating system type.  There is probably no reason
   to want to change this...}
   
   \item{look_in}{for Windows only, the path to a folder (or vector of 
   folders) which contains another folder with name containing 'JAGS', 
   where the JAGS executable(s) are to be found.  findjags() will attempt 
   to find the highest version, assuming that the version number is 
   somewhere in the file path to the executable (as per default installation).}
   
   \item{...}{provided for compatibility with deprecated arguments.}
}

\value{A path or command for the most likely location of the desired JAGS 
executable on the system.  On unix this will always be 'jags', on Windows 
for example "C:/Program Files/JAGS/bin/jags-terminal.exe" or 
"C:/Program Files/JAGS/JAGS-1.0.0/bin/jags-terminal.exe"
}
\seealso{
   \code{\link{testjags}},
   
   \code{\link{runjags.options}},
   
   \code{\link{run.jags}}
}

\keyword{methods}