#' @title Extract Any Models, Data, Monitored Variables or Initial Values As Character Vectors from a JAGS or WinBUGS Format Textfile
#' @name read.jagsfile
#' @aliases read.jagsfile read.winbugs read.WinBUGS read.JAGSfile
#'
#' @description
#' Read a user specified BUGS or JAGS textfile or character variable and extract any models, data, monitored variables or initial values as character vectors.  Used by (auto)run.jags to interpret the input file(s) or strings.  This function is more likely to be used via \code{\link{run.jags}} where the model specified to run.jags is the path used by this function.  The read.winbugs function is an alias to read.jagsfile.
#'
#' @details
#' There are a number of special strings permitted inside the model
#' specification as follows:
#'
#' #data# variables to be retrieved from a list or environment
#'
#' #inits# variables to be retrieved from a list or environment
#'
#' #monitors# monitored variables to use
#'
#' #modules# JAGS extension modules optionally also specifying the status
#' (e.g. #modules# glm on, dic on)
#'
#' #factories# JAGS factories and types required, optionally also specifying
#' the status (e.g. #factories# mix::TemperedMix sampler on)
#'
#' #response# - a single variable name specifying the response variable
#' (optional)
#'
#' #residual# - a single variable name specifying a variable that represents
#' the residuals (optional)
#'
#' #fitted# - a single variable name specifying a variable that represents
#' the fitted value (optional)
#'
#' #Rdata# when placed inside a data{ } or inits{ } block, this signifies
#' that any arrays indside are in column major order. This is the default
#' for any blocks that are not specified as a list( ).
#'
#' #BUGSdata# when placed inside a data{ } or inits{ } block, this signifies
#' that any arrays indside are in row major order. This is the default
#' for any blocks that are specified as a list( ), such as those that
#' have been created for use with WinBUGS.
#'
#' #modeldata# when placed inside a data{ } block, this signifies that
#' the code is to be passed to JAGS along with the model{ } block
#'
#' @param file either a relative or absolute path to a textfile (including the file extension) containing a model in the JAGS language and possibly monitored variable names, data and/or initial values, or a character string of the same.  May also be a vector of paths to different text files, possibly separately containing the model, data and intitial values.  No default.  The model must be started with the string 'model\{' and ended with '\}' on new lines.  Data must be similarly started with 'data\{', monitored variables with 'monitor\{', and initial values as 'inits\{', and all ended with '\}'.  Seperate variables in such blocks must be separated by a line break.  If multiple models are found, all but the first one are ignored with a warning.  Multiple data blocks and monitor blocks are combined, multiple inits blocks are used for different chains.  Monitors may also be given using the phrase '#monitor# variable' within the model block, in which case 'variable' is added to the list of monitored variables found in the monitor block(s).  The use of automatically generated data and initial values is also supported using similar syntax, with '#data# variable' for automatically generated data variables or '#inits# variable' for automatically generated initial value variables in which case 'variable' is used as data or initial values with a value taken by \code{\link{run.jags}} from datalist, initlist or R objects as appropriate.  '#inits#', '#data#' and '#monitor#' statements can appear on the same line as model code, but no more than one of these statements should be used on the same line.  Examples of acceptable model syntax are given below.
#'
#' @keywords methods
#'
#' @return
#' A named list of elements required to compile a model.  These can be used to create a call to \code{\link{run.jags}}, but it would be more usual to call this function directly on the model file.
#'
#' @seealso
#' \code{\link{run.jags}} and \code{\link{write.jagsfile}} for the reverse operation, and possibly an example of the formatting allowed
#'
#' @references
#' Lunn D, Jackson C, Best N, Thomas A, Spiegelhalter D (2012). The BUGS book: A practical introduction to Bayesian analysis. CRC press; and Matthew J. Denwood (2016). runjags: An R Package Providing Interface Utilities, Model Templates, Parallel Computing Methods and Additional Distributions for MCMC Models in JAGS. Journal of Statistical Software, 71(9), 1-25. doi:10.18637/jss.v071.i09
#'
#' @examples
#' # ALL SYNTAX GIVEN BELOW IS EQUIVALENT
#'
#' # Use a modified WinBUGS text file with manual inits and manual data and
#' # a seperate monitor block (requires least modification from a WinBUGS
#' # file).  For compatibility with WinBUGS, the use of list() to enclose
#' # data and initial values is allowed and ignored, however all seperate
#' # variables in the data and inits blocks must be seperated with a line
#' # break (commas or semicolons before linebreaks are ignored).  data{ ... }
#' # and inits{ ... } must also be added to WinBUGS textfiles so that the
#' # function can seperate data from initial values.  Iterative loops are
#' # allowed in data blocks but not in init blocks.  See also the differences
#' # in JAGS versus WinBUGS syntax in the JAGS help file.
#'
#' # The examples below are given as character strings for portability,
#' # but these could also be contained in a separate model file with the
#' # arguments to read.jagsfile and run.jags specified as the file path
#'
#'
#' # A model that could be used with WinBUGS, incorporating data and inits.
#' # A note will be produced that the data and inits are being converted
#' # from WinBUGS format:
#'
#' string <- "
#' model{
#'
#' 	for(i in 1:N){
#' 		Count[i] ~ dpois(mean)
#' 	}
#' 	mean ~ dgamma(0.01, 100)
#' }
#'
#' data{
#' list(
#'  Count = c(1,2,3,4,5,6,7,8,9,10),
#'  N = 10
#'  )
#' }
#'
#' inits{
#' list(
#' 	mean = 1
#' 	)
#' }
#'
#' inits{
#' list(
#' 	mean = 100
#' 	)
#' }
#'
#' "
#'
#' model <- read.winbugs(string)
#' \dontrun{
#' results <- run.jags(string, monitor='mean')
#' }
#'
#'
#' # The same model but specified in JAGS format.  This syntax also defines
#' # monitors in the model, and uses data retrieved from the R environment:
#'
#' string <- "
#' model{
#'
#' 	for(i in 1:N){
#' 		Count[i] ~ dpois(mean) #data# Count, N
#' 	}
#' 	mean ~ dgamma(0.01, 100)
#' 	#monitor# mean
#' }
#'
#' inits{
#' 	mean <- 1
#' }
#'
#' inits{
#' 	mean <- 100
#' }
#' "
#'
#' model <- read.jagsfile(string)
#' Count <- 1:10
#' N <- length(Count)
#' \dontrun{
#' results <- run.jags(string)
#' }
#'
#'
#' # The same model using autoinits and a mixture of manual and autodata:
#' string <- "
#' model{
#'
#' 	for(i in 1:N){
#' 		Count[i] ~ dpois(mean) #data# Count
#' 	}
#' 	mean ~ dgamma(0.01, 100)
#' 	#monitor# mean
#' 	#inits# mean
#' }
#'
#' data{
#'
#' 	N <- 10
#'
#' }
#' "
#'
#' model <- read.jagsfile(string)
#' Count <- 1:10
#' mean <- list(1, 100)
#' \dontrun{
#' results <- run.jags(string, n.chains=2)
#' }
#'
NULL

#' @rdname read.jagsfile
#' @export
read.jagsfile <- function(file){

#	if(!is.character(file) || inherits(zz, 'connection'))
	if(is.runjags(file))
    	stop('Invalid model file - this argument must be specified as a character string or a character string giving a path to a file', call.=FALSE)

	st <- Sys.time()

	exists = likelystring <- logical(length(file))

	for(i in 1:length(file)){
		likelystring[i] <- any(grepl('\n',file[i],fixed=TRUE) || grepl('\r',file[i],fixed=TRUE) || (grepl('{',file[i],fixed=TRUE) && grepl('}',file[i],fixed=TRUE)))

		if(!likelystring[i]){
			exists[i] <- FALSE
			for(end in c('','.txt','.bug', '.R')){
				exists[i] <- suppressWarnings(try(file.exists(paste(file[i], end, sep='')), silent=TRUE))
				if(inherits(exists[i], "try-error")) exists[i] <- FALSE
				if(exists[i]){
					file[i] <- paste(file[i], end, sep='')
					break
				}
			}
		}
	}

	if(all(!likelystring) & all(!exists)){
	  if(length(file)==1){
	    error <- paste("No model file found at the file path provided: '", file.path(getwd(),file), "'", sep='')
	  }else{
	    error <- paste("No model file found at the file paths provided: ", paste("'", file.path(getwd(),file), "'", collapse=', ', sep=''),sep='')
	  }
	  stop(error)
	}

	if(length(file)==1){
		if(exists[1]) string <- paste(readLines(file, warn=FALSE), collapse="\n") else string <- file

	}else{
		string <- ""
		for(i in 1:length(file)){

			if(likelystring[i]==FALSE & exists[i]==FALSE) warning(paste("Specified file '", file[i], "' does not exist", sep=""))
			if(exists[i]) string <- paste(string, paste(readLines(file[i], warn=FALSE), collapse="\n"), sep="\n") else string <- paste(string, file[i], sep="\n")
		}
	}

	string <- paste(string, "\n", sep="")

	# Rubbish old code that didn't use gsub and was slow:
	#find <- c("\n\t", "\n ", "\r\t", "\r ", "\n\n", "\r\r") # ",\n", ",\r", ";\n", ";\r", - removed these since we need ',\n' for some winbugs data but they are added again later on after spaces between () are removed
	#for(i in 1:length(find)){
	#	repeat{

	#		splits <- strsplit(string, split=find[i])
	#		string <- paste(splits[[1]], collapse="\n")
	#		if(length(splits[[1]])==1) break

	#	}
	#}

	# New gsub based code:
	# Remove all tabs
	string <- gsub("\t", "", string, fixed=TRUE)
	# Convert carriage returns and form feeds to \n
	string <- gsub("\f", "\n", string, fixed=TRUE)
	string <- gsub("\r", "\n", string, fixed=TRUE)
	# Remove excess white space at the start of lines:
	string <- gsub("\n[[:space:]]*", "\n", string)

	# Get rid of all commented out lines - leaving in the special ones we need to keep:
	tokeep <- c('#bugsdata#','#Rdata#','#modeldata#','#BUGSdata#','#Rdata#')
	changeto <- c('--bugsformat--', '--rformat--', '--modelformat--', '--bugsformat--', '--rformat--')
	nohashstring <- string
	for(k in 1:length(tokeep)){
		nohashstring <- gsub(tokeep[k], paste(changeto[k],'#',sep=''), nohashstring)  # adding the hash here clears the rest of the line
	}
	nohashstring <- gsub('#[^\n]*','',nohashstring)   # \r are converted above
#	nohashstring <- paste(lapply(strsplit(nohashstring, "[\n\r]")[[1]], function(x) gsub("#.*", "", x)), collapse="\n")  #inefficient
	for(k in 1:length(tokeep)){
		nohashstring <- gsub(changeto[k], tolower(tokeep[k]), nohashstring)
	}

	# No helpful conversion of = to <- any more (was it doing this beforehand?)

#   MOVED TRAILING COMMA FIX ELSEWHERE
	# Remove remaining commas and semi-colons from end of lines (allowing for as many spaces as you like between, and \n)
#	nohashstring <- gsub(",[[:space:]]*\n", "\n", nohashstring)
#	nohashstring <- gsub(";[[:space:]]*\n", "\n", nohashstring)
	# the , .Dim will be put back in later
	#   MOVED TRAILING COMMA FIX ELSEWHERE

	if(!grepl('model[[:space:]]*\\{',string))
		stop("No valid model was found", call.=FALSE)

	# Check that the number of { and } match:
	openbraces <- as.numeric(gregexpr('\\{', nohashstring)[[1]])
	closebraces <- as.numeric(gregexpr('\\}', nohashstring)[[1]])
	if(length(openbraces)!=length(closebraces) || all(openbraces<0) || all(closebraces<0))
		stop('Unmatched number of { and } in the specified model file - ensure all (uncommented) braces are paired correctly', call.=FALSE)

	# First extract model:
	model <- paste("model{\n", winbugs.extract.big("model", string)[[1]], "\n}\n", sep="")

	if(length(model)>1){
		warning("More than 1 model block was found in the file - the first model was used and other(s) ignored", call.=FALSE)
		model <- model[1]
	}

	mainmutate <- winbugs.extract.big("mutate", nohashstring, remove.list=FALSE)[[1]]
	automutate <- winbugs.extract.small('mutate', string)

	maindata <- winbugs.extract.big("data", nohashstring)
	maindata <- sortjagsvsbugs(maindata, data.type=TRUE)
	model <- paste(maindata$model,model,sep='')
	maindata <- maindata$fixed

	autodata <- winbugs.extract.small("data", string)

	maininits <- winbugs.extract.big("inits", nohashstring)
	maininits <- sortjagsvsbugs(maininits, data.type=FALSE)$fixed


	############### to remove ->
	if(FALSE){
	####### Was previously in extract.big but now here as I want to see the = before converting the data.  Still always want to convert inits though.
	newstring <- maininits
	for(i in 1:length(newstring)){
		temp <- strsplit(newstring[i], "")[[1]]

		#  Because you can't have .Dim <- structure or variable = value:
		numbers <- which(temp=="=")
		if(length(numbers)>0){
			for(k in 1:length(numbers)){
				tstring <- character(length=10)
				for(j in 1:10){
					tstring[j] <- paste(temp[pmax((numbers[k]-3-j):(numbers[k]-j), 1)], collapse="")
				}
				if(all(tstring!=".Dim")) temp[numbers[k]] <- "<-"
			}
		}

		newstring[i] <- paste(temp, collapse="")
	}
	maininits <- newstring
	######


	### Also need to check for variable=value, variable=value without new line:
	for(i in 1:length(maininits)){

	# This code will change any "," to "\n" from between "<-" and "<-" unless a "(" or ")" are present:
	# [^\\)^\\(] means any character except ) or (
	# *? and perl makes it non-greedy matching
	# Need a while loop as the same character can't be used as the end point of one search and the start of another, so variable=value, variabile=value, variable=value would only remove the first (although variable=value, variable=value\nvariable=value, variable=value would be fine)
	s <- 0
	while(s!=-1){
	greps <- gregexpr("<-([^\\)^\\(]*?),([^\\)^\\(]*?)<-", maininits[i], perl=TRUE)
	s <- greps[[1]]
	e <- (s-1)+ attr(greps[[1]], "match.length")
	if(s[1]!=-1){ # If it doesn't find anything, s=-1
	finalstring <- strsplit(maininits[i], "", fixed=TRUE)[[1]]
	for(j in 1:length(s)){

		tstring <- finalstring[s[j]:e[j]]
		newstring <- gsub(",", "\n", tstring)
		finalstring[s[j]:e[j]] <- newstring

	}
	maininits[i] <- paste(finalstring, collapse="")
	}
	}
	}
	### Then make sure any new leading white space is removed (gsub is vectorised):
	maininits <- gsub("\n[[:space:]]*", "\n", maininits)
	####

	#   MOVED TRAILING COMMA FIX ELSEWHERE
	# As a final step, replace the removed , .Dim from data:
	maininits <- gsub('\n.Dim', ',\n.Dim', maininits, fixed=TRUE)
	# And remove leading and trailing white space:
	maininits <- gsub('^ *\n', '', maininits)
	maininits <- gsub('\n *\n$', '\n', maininits)
	#   MOVED TRAILING COMMA FIX ELSEWHERE

	}
	############### -> to remove


	autoinits <- winbugs.extract.small("inits", string)

	####### This does need to be here - an example in read.jagsfile uses it:
	monitors <- ""
	temp <- winbugs.extract.big("monitor", string)[[1]]
	for(i in 1:length(temp)){
		tempy <- strsplit(temp[i], "")[[1]]
		tempy <- paste(tempy[tempy!=" "], collapse="")
		#tempy <- paste(strsplit(temp[i], "")[[1]][strsplit(temp[i], "")!=""][[1]], collapse="")
		for(str in c("\n", "\t", ",", ":", ";")){
			tempy <- paste(strsplit(tempy, str, fixed=TRUE)[[1]], collapse="*")
		}
		tempy <- strsplit(tempy, "*", fixed=TRUE)[[1]]
		monitors <- c(monitors, tempy[tempy!=""])
	}
	#######

	monitors <- checkvalidmonitorname(c(monitors, winbugs.extract.small("monitor", string)))
	modules <- checkmodfact(winbugs.extract.small('modules', string), 'module')
	factories <- checkmodfact(winbugs.extract.small('factories', string), 'factory')
	response <- checkvalidmonitorname(winbugs.extract.small('response', string))
	residual <- checkvalidmonitorname(winbugs.extract.small('residual', string))
	fitted <- checkvalidmonitorname(winbugs.extract.small('fitted', string))
	monitors <- monitors[monitors!=""]

	#if(length(monitors)==0) warning("No monitor blocks or tags were found")

	#if(length(maindata)==0) warning("No data blocks or tags were found")

	model[model==''] <- NA
	maindata[maindata==''] <- NA
	autodata[autodata==''] <- NA
	maininits[maininits==''] <- NA
	autoinits[autoinits==''] <- NA
	monitors[monitors==''] <- NA
  mainmutate[mainmutate==''] <- NA
  automutate[automutate==''] <- NA

	if(is.null(model) || length(model)==0 || all(is.na(model)))
		model <- NA
	if(is.null(maindata) || length(maindata)==0 || all(is.na(maindata)))
		maindata <- NA
	if(is.null(autodata) || length(autodata)==0 || all(is.na(autodata)))
		autodata <- NA
	if(is.null(maininits) || length(maininits)==0 || all(is.na(maininits)))
		maininits <- NA
	if(is.null(autoinits) || length(autoinits)==0 || all(is.na(autoinits)))
		autoinits <- NA
	if(is.null(monitors) || length(monitors)==0 || all(is.na(monitors)))
		monitors <- NA
	if(is.null(modules) || length(modules)==0 || all(is.na(modules)))
		modules <- NA
	if(is.null(factories) || length(factories)==0 || all(is.na(factories)))
		factories <- NA
	if(is.null(response) || length(response)==0 || all(is.na(response)))
		response <- NA
	if(is.null(residual) || length(residual)==0 || all(is.na(residual)))
		residual <- NA
	if(is.null(fitted) || length(fitted)==0 || all(is.na(fitted)))
		fitted <- NA
	if(is.null(mainmutate) || length(mainmutate)==0 || all(is.na(mainmutate)))
		mainmutate <- NA
	if(is.null(automutate) || length(automutate)==0 || all(is.na(automutate)))
		automutate <- NA

	if(identical(modules, as.character(NA)))
		modules <- ''
	if(identical(factories, as.character(NA)))
		factories <- ''

	output <- list(model=model, data=maindata, autodata=autodata, inits=maininits, autoinits=autoinits, monitor=monitors, modules=modules, factories=factories, response=response, residual=residual, fitted=fitted, mutate=mainmutate, automutate=automutate)

	if(runjags.getOption('debug')>=10)
		swcat('Time taken to read model file: ', timestring(st, Sys.time()), '\n', sep='')

	return(output)

}

#' @rdname read.jagsfile
#' @export
read.JAGSfile <- read.jagsfile

#' @rdname read.jagsfile
#' @export
read.winbugs <- read.jagsfile

#' @rdname read.jagsfile
#' @export
read.WinBUGS <- read.winbugs


