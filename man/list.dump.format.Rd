\name{dump.format}
\alias{dump.format}
\alias{list.format}

\title{Conversion Between a Named List and a Character String in the R Dump Format}
\description{
   Convert a named list of numeric vector(s) or array(s) of data or
   initial values to a character string in the correct format to be read
   directly by JAGS as either data or initial values.
}
\usage{
	dump.format(namedlist=list(), checkvalid=TRUE,
		convertfactors = TRUE)
	
	list.format(data=character(), checkvalid=TRUE)

}
\arguments{
   \item{namedlist}{a named list of numeric or integer (or something that can
   be coerced to numeric) vectors, matrices or arrays.  The name of each
   list item will be used as the name of the resulting dump.format
   variables.}

   \item{data}{a character string in the R dump format, such as that produced by dump.format.}
   
   \item{checkvalid}{option to ensure that the object returned from the
   function does not contain any values that would be invalid for import
   into JAGS, such as Inf, -Inf or character values etc.}

   \item{convertfactors}{option to automatically convert any factor
   variables to numeric (otherwise the presence of factors will
   create an error if checkvalid==TRUE).}
}
\value{Either a character string in the R dump format (for dump.format), or a named list (for list.format).
}

\details{The 'dump.format' function creates a character string of the supplied
variables in the same way that dump() would, except that the result is
returned as a character string rather than written to file. 
Additionally, dump.format() will look for any variable with the name
'.RNG.name' and double quote the value if not already double quoted (to
ensure compatibility with JAGS).}

\seealso{
   \code{\link{run.jags}},
   \code{\link[base]{dump}}
}

\examples{

# A named list:
namedlist1 <- list(N=10, Count=c(4,2,7,0,6,9,1,4,12,1))

# Conver to a character vector:
chardata <- dump.format(namedlist1)

# And back to a named list:
namedlist2 <- list.format(chardata)

# These should be the same:
stopifnot(identical(namedlist1, namedlist2))

}
\keyword{methods}