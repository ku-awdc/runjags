#' @title Conversion Between a Named List and a Character String in the R Dump Format
#' @name dump.list.format
#' @aliases dump.format list.format

#' @description
#'    Convert a named list of numeric vector(s) or array(s) of data or initial values to a character string in the correct format to be read directly by JAGS as either data or initial values.

#' @details
#' The 'dump.format' function creates a character string of the supplied variables in the same way that dump() would, except that the result is returned as a character string rather than written to file.  Additionally, dump.format() will look for any variable with the name '.RNG.name' and double quote the value if not already double quoted (to ensure compatibility with JAGS).

#' @return
#' Either a character string in the R dump format (for dump.format), or a named list (for list.format).

#' @keywords methods

#' @examples
#' # A named list:
#' namedlist1 <- list(N=10, Count=c(4,2,7,0,6,9,1,4,12,1))
#' # Convert to a character vector:
#' chardata <- dump.format(namedlist1)
#' # And back to a named list:
#' namedlist2 <- list.format(chardata)
#' # These should be the same:
#' stopifnot(identical(namedlist1, namedlist2))

#' @seealso
#' \code{\link{run.jags}} and \code{\link[base]{dump}}

#' @param namedlist a named list of numeric or integer (or something that can be coerced to numeric) vectors, matrices or arrays.  The name of each list item will be used as the name of the resulting dump.format variables.
#' @param data a character string in the R dump format, such as that produced by dump.format.
#' @param checkvalid option to ensure that the object returned from the function does not contain any values that would be invalid for import into JAGS, such as Inf, -Inf or character values etc.
#' @param convertfactors option to automatically convert any factor variables to numeric (otherwise the presence of factors will create an error if checkvalid==TRUE).
NULL

#' @rdname dump.list.format
#' @export
dump.format <- function(namedlist=list(), checkvalid=TRUE, convertfactors=TRUE){

	data <- namedlist

	if(identical(data, list()))
		return('')

	if(length(data)==2 & is.null(names(data)) & is.character(data[[1]]) & length(data[[1]])==1){  # allows old style dump.format (length = 1)
		names <- data
		data <- list(data[[2]])
		names(data) <- names[[1]]
	}

	if(inherits(data, 'data.frame'))
		data <- as.list(data)
	if(!inherits(data,"list") || length(data)==0) stop("Data must be provided as a named list or data frame", call.=FALSE)
	if(any(names(data)=="") || is.null(names(data))) stop("Data must be provided as a named list or data frame", call.=FALSE)
	if(length(unique(names(data)))!=length(data)) stop('All elements in the data list must have unique names', call.=FALSE)

	if(convertfactors){
		for(c in which(sapply(data,inherits,what='factor')))
			data[[c]] <- as.numeric(data[[c]])
	}

	if(checkvalid){
		valid <- checkvalidforjags(data)
		if(!valid$valid) stop(paste("The following problem was identified in the data provided:  ", valid$probstring, sep=""))
	}

	variable = names(data)
	value <- data

	if(any(variable==".RNG.name")){
		n <- which(variable==".RNG.name")
		split <- strsplit(value[[n]], split="")[[1]]
		if(split[1]!="\"" & split[length(split)]!="\""){
			split <- c("\"", split, "\"")
			value[[n]] <- paste(split, collapse="")
		}
	}

	output.string <- ""
	for(i in 1:length(variable)){
		if(length(value[[i]])==1 && length(dim(value[[i]]))==0){
			value.string <- as.character(value[[i]])
		}else{
			dims <- dim(value[[i]])
			if(length(dims) > 1){
				value.string <- "structure(c("
			}else{
				value.string <- "c("
			}
			value.string <- paste(value.string, paste(value[[i]], collapse=", "), ")", sep="")
			if(length(dims) > 1){
				value.string <- paste(value.string, ", .Dim = c(", paste(dims, collapse=", "), "))", sep="")
			}
		}
		output.string <- paste(output.string, "\"", variable[[i]], "\" <- ", value.string, "\n", sep="")
	}

	return(output.string)
}


#' @rdname dump.list.format
#' @export
list.format <- function(data=character(), checkvalid=TRUE){

	if(! inherits(data, c("character","runjagsdata","runjagsinits")) || length(data)==0) stop("Data must be provided as a character string in the R dump format")

	if(all(data=='' | data=='\n'))
		return(list())

	out <- vector('list', length=length(data))

	for(i in 1:length(data)){
		if(data[i]==""){
			out[[i]] <- list()
		}else{
			str <- data[i]
      # Remove anything following a comment:
      str <- gsub('#[^\n]*', '', str)
      # Remove leading and trailing white space:
			str <- gsub('^ *\n', '', str)
			str <- gsub('\n *\n$', '\n', str)

      str <- gsub("<-", "=", str)
			str <- gsub("`", "", str)
			str <- gsub("= \n", "=", str)
			str <- gsub("^\n", "", str)
			str <- gsub("\n\n", "\n", str)
			str <- gsub("\n\n", "\n", str)
			str <- gsub("\n\n", "\n", str)
      str <- gsub(",\n.Dim", ", .Dim", str, fixed=TRUE)
			str <- gsub("\n", ",", str)

			if(str!='' && strsplit(str, split="")[[1]][length(strsplit(str, split="")[[1]])] == ",") str <- paste(strsplit(str, split="")[[1]][1:(length(strsplit(str, split="")[[1]])-1)], collapse="")
			out[[i]] <- eval(parse(text=paste('list(', str, ')')))
		}
	}

	if(length(data)>1){
		names(out) <- paste('Chain.', 1:length(data), sep='')
	}else{
		out <- out[[1]]
	}

	if(checkvalid){
		valid <- checkvalidforjags(out)
		if(!valid$valid)
			stop(paste("The following problem was identified in the data provided:  ", valid$probstring, sep=""))
	}

	return(out)
}
