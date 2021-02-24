\name{ask}
\alias{ask}

\title{Obtain Input from User With Error Handling}

\description{
   A simple function to detect input from the user, and keep prompting
   until a response matching the class of input required is given.
}
\usage{
ask(prompt="?", type="logical", bounds=c(-Inf, Inf),  na.allow=FALSE)
}
\arguments{

\item{prompt}{what text string should be used to prompt the user?
(character string)}

\item{type}{the class of object expected to be returned - "logical",
"numeric", "integer", "character".  If the user input does not match
this return, the prompt is repeated}

\item{bounds}{the lower and upper bounds of number to be returned. 
Ignored if type is "logical" or "character"}

\item{na.allow}{if TRUE, allows the user to input "NA" for any type,
which is returned as NA}
}
\seealso{
   \code{\link[base]{readline}}, 
   
   \code{\link[utils]{menu}}
}

\keyword{methods}
