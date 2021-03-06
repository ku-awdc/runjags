% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utilities.R
\name{new_unique}
\alias{new_unique}
\title{Create a Unique Filename}
\usage{
new_unique(
  name = NA,
  suffix = "",
  ask = FALSE,
  prompt = "A file or directory with this name already exists.  Overwrite?",
  touch = FALSE,
  type = "file"
)
}
\arguments{
\item{name}{the filename to be used (character string).  A vector of character strings is also permissible, in which case they will be pasted together.  One or more missing (NA) values can also be used, which will be replaced with a randomly generated 9 character alphanumeric string. Default NA.}

\item{suffix}{the file extension (including '.') to use (character string).  If this does not start with a '.', one will be prepended automatically.  Default none.}

\item{ask}{if a file exists with the input name, should the function ask to overwrite the file? (logical)  If FALSE, a new filename is used instead and no files will be over-written.  Default FALSE.}

\item{prompt}{what text string should be used to prompt the user? (character string)  Ignored is ask==FALSE.  A generic default is supplied.}

\item{touch}{option to create (touch) the file/folder after generating the unique name, which prevents other processes from sneaking in and creating a file with the same name before the returned filename has had chance to be used.  Default FALSE.}

\item{type}{if touch==TRUE, then type controls if a file or directory is created.  One of 'file', 'f', 'directory', or 'd'.  Default 'file'.}
}
\value{
A unique filename that is safe to use without fear of destroying existing files
}
\description{
Search the current working directory for a file or directory matching the input name, and if it exists suggest a new name by appending a counter to the input name.  Alternatively, the function can ask the user if the existing file should be overwritten, in which case the existing file will be erased if the answer is 'yes'.  The function also checks for write access permissions at the current working directory.
}
\examples{
#  Create a unique file name with a .R extension.
new_unique(c("new_file", NA), ".R", ask=FALSE)

}
\seealso{
\code{\link{ask}}
}
\keyword{methods}
