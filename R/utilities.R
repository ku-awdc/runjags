#' Obtain Input from User With Error Handling
#'
#' @description A simple function to detect input from the user, and keep prompting until a response matching the class of input required is given.
#'
#' @param prompt what text string should be used to prompt the user? (character string)
#' @param type the class of object expected to be returned - "logical", "numeric", "integer", "character".  If the user input does not match this return, the prompt is repeated
#' @param bounds the lower and upper bounds of number to be returned.  Ignored if type is "logical" or "character".
#' @param na.allow if TRUE, allows the user to input "NA" for any type, which is returned as NA
#'
#' @keywords methods
#' @seealso
#' \code{\link[base]{readline}} and \code{\link[utils]{menu}}
#'
#' @export
ask <- function (prompt="?", type="logical", bounds=c(-Inf, Inf), na.allow=FALSE){
	prompt <- paste(prompt, '  ', sep='')

	rettypes <- c("logical", "numeric", "character", "integer")
	type <- pmatch(type,rettypes)
	if(is.na(type)){
		stop(paste("Unrecognised return type \'", type, "\'", sep=""))
	}
	type <- rettypes[type]

	if(type=="logical"){
		repeat{
			result <- readline(prompt = prompt)
			if(na.allow==TRUE && (result=="NA" | result=="na" | result=="Na")){
				return(NA)
			}
			if((result == "T") | (result == "TRUE") | (result == "True") | (result == "true") | (result == "t") | (result == "y") | (result == "YES") | (result == "yes") | (result == "Yes") | (result == "Y")){
				return(TRUE)
			}
			if((result == "F") | (result == "FALSE") | (result == "False") | (result == "false") | (result == "f") | (result == "n") | (result == "NO") | (result == "no") | (result == "No") | (result == "N")){
				return(FALSE)
			}
			swcat("ERROR:  Please enter 'y' or 'n'\n")
		}
	}

	if(type=="numeric"){
		repeat{
			result <- readline(prompt = prompt)
			if(na.allow==TRUE && (result=="NA" | result=="na" | result=="Na")){
				return(NA)
			}
			suppressWarnings(result <- as.numeric(result))
			if(is.na(result)){
				swcat("ERROR:  Please enter a number\n")
			}else{
				if((result > bounds[2]) | (result < bounds[1])){
					swcat("ERROR:  Please enter a number between ", bounds[1], " and ", bounds[2], "\n", sep="")
				}else{
					return(result)
				}
			}
		}
	}

	if(type=="character"){
		return(as.character(readline(prompt=prompt)))
	}

	if(type=="integer"){
		repeat{
			result <- readline(prompt=prompt)
			if(na.allow==TRUE && (result=="NA" | result=="na" | result=="Na")){
				return(NA)
			}
			suppressWarnings(result <- as.numeric(result))
			if(is.na(result) | (as.integer(result) != result)){
				swcat("ERROR:  Please enter a whole number\n")
			}else{
				if((result > bounds[2]) | (result < bounds[1])){
					swcat("ERROR:  Please enter a whole number between ", bounds[1], " and ", bounds[2], "\n", sep="")
				}else{
					return(result)
				}
			}
		}
	}
}

#' Attempt to Locate a JAGS Install
#'
#' @aliases findjags findJAGS
#'
#' @description   Search the most likely locations for JAGS to be installed on the users system, based on the operating system, and return the most likely path to try.  Where multiple installs exist, findjags will attempt to return the path to the install with the highest version number.  For Unix systems, calling jags using 'jags' requires the jags binary to be in the search path, which may be specified in your user '.Profile' if necessary (the JAGS executable is also looked for in the default install location of /usr/local/bin/jags if popen support is enabled).
#'
#' @param ostype the operating system type.  There is probably no reason to want to change this...
#' @param look_in for Windows only, the path to a folder (or vector of  folders) which contains another folder with name containing 'JAGS', where the JAGS executable(s) are to be found.  findjags() will attempt to find the highest version, assuming that the version number is somewhere in the file path to the executable (as per default installation).
#' @param ... provided for compatibility with deprecated arguments.
#'
#' @return A path or command for the most likely location of the desired JAGS executable on the system.  On unix this will always be 'jags', on Windows for example "C:/Program Files/JAGS/bin/jags-terminal.exe" or "C:/Program Files/JAGS/JAGS-1.0.0/bin/jags-terminal.exe"
#'
#' @keywords methods
#' @seealso
#' \code{\link{testjags}}. \code{\link{runjags.options}} and \code{\link{run.jags}}
#'
#' @export
findjags <- function(ostype = .Platform$OS.type, look_in = NA, ...){

	if(is.na(look_in)) look_in <- if(ostype=="windows") c("/Program Files/","/Windows/Program Files/","C:/Program Files/","C:/Windows/Program Files/","/") else NULL

	if(! ostype %in% c("unix","windows")) stop(paste("Unrecognised OS type '", ostype, "'.  Use either 'unix' or 'windows'", sep=""))

	# This is a bit of a mess now the option is deprecated...
	if(!exists('from.variable')) from.variable <- ""
	if(is.na(from.variable) | is.null(from.variable) | from.variable=="" | length(from.variable)==0){
		from.variable <- character(0)
	}else{
		if(length(find(from.variable))==0){
			from.variable <- character(0)
		}else{
			from.variable <- get(from.variable)
		}
	}
	if(length(from.variable)!=0){
		warning("The '.jagspath' option is deprecated - use the runjags.options() to set the default path to JAGS")
	}

	if(ostype=="unix"){

		# Test standard 'jags' as well as variable passed and some common install locations:
		suppressWarnings({
			paths <- c(from.variable, "jags", "/opt/local/bin/jags", "/opt/local/sbin/jags", "/usr/texbin/jags", "/usr/bin/jags", "/bin/jags", "/usr/sbin/jags", "/sbin/jags", "/usr/local/bin/jags", "/opt/R/arm64/bin/jags", "/usr/X11/bin/jags")
			for(i in 1:length(paths)){
				jagspath <- system(paste('which ', paths[i], ' 2>&1', sep=""), intern=TRUE)
				if(length(jagspath)!=0) return(jagspath)
			}
		})

	}

	if(ostype=="windows"){

		# First check if the environmental variable is set (by rjags), then this will work:
		suppressWarnings(s <- try(system('where jags',intern=TRUE),silent=TRUE))
		if(!inherits(s, 'try-error') && is.null(attr(s,'status'))){
			s <- gsub('jags.bat','jags-terminal.exe',s)
			s <- gsub('\\','/',s,fixed=TRUE)
			return(s)
		}

		if(!is.character(look_in)) stop("The look_in argument supplied to findjags must be a character vector of paths")
		look_in <- paste(look_in,"/",sep="")
		look_in <- gsub("//","/",look_in,fixed=TRUE)
		suppressWarnings(paths <- paste(unlist(lapply(look_in, function(x) return(paste(x,list.files(x),sep="")))),"/",sep=""))
		possible <- unique(paths[grepl("JAGS",paths)|grepl("jags",paths)])

		if(Sys.getenv("JAGS_HOME")!="") posspaths <- Sys.getenv("JAGS_HOME") else posspaths <- character(0)

		if(length(possible)>0) for(i in 1:length(possible)){
		binpaths <- list.files(possible[i],recursive=TRUE)
		posspaths <- c(posspaths, paste(possible[i],binpaths[grepl("jags-terminal.exe",binpaths)],sep=""))
		}

		posspaths <- c(unique(posspaths), from.variable)
		versions <- sapply(posspaths, function(x){
			pathsegs <- strsplit(x,"/")[[1]]
			withnum <- grepl("[0-9]",pathsegs)
			return(getvers(pathsegs[withnum][1]))
		})
		# If versions doesn't contain a number, getvers will return 0

		x64s <- grepl('x86',posspaths)

		if(any(x64s)){
			versions <- versions[x64s]
			posspaths <- posspaths[x64s]
		}

		if(length(na.omit(versions))>0){
			return(posspaths[which(versions==max(versions))[1]])
		}

		##  jags.bat spawns lots of horrible windows.  jags-terminal.exe requires environmental variables - set by testjags()
	}

	# JAGS executable wasn't found - see if rjags is installed:

	# If rjags is loaded no warning:
	if(! any(.packages(FALSE)=='rjags')){
		# If rjags is installed but not loaded suggest loading it:

		if(any(.packages(TRUE)=='rjags')){
			warning("JAGS was not found in the common install locations on your system; try loading the rjags package first or alternatively provide a path to the executable as the jags argument or using runjags.options()")
		}else{
			warning("JAGS was not found in the common install locations on your system; please provide a path to the executable as the jags argument or using runjags.options()")
		}
	}

	return("JAGS not found")

}


#' Create a Unique Filename
#'
#' @description
#' Search the current working directory for a file or directory matching the input name, and if it exists suggest a new name by appending a counter to the input name.  Alternatively, the function can ask the user if the existing file should be overwritten, in which case the existing file will be erased if the answer is 'yes'.  The function also checks for write access permissions at the current working directory.
#'
#' @param name the filename to be used (character string).  A vector of character strings is also permissible, in which case they will be pasted together.  One or more missing (NA) values can also be used, which will be replaced with a randomly generated 9 character alphanumeric string. Default NA.
#' @param suffix the file extension (including '.') to use (character string).  If this does not start with a '.', one will be prepended automatically.  Default none.
#' @param ask if a file exists with the input name, should the function ask to overwrite the file? (logical)  If FALSE, a new filename is used instead and no files will be over-written.  Default FALSE.
#' @param prompt what text string should be used to prompt the user? (character string)  Ignored is ask==FALSE.  A generic default is supplied.
#' @param touch option to create (touch) the file/folder after generating the unique name, which prevents other processes from sneaking in and creating a file with the same name before the returned filename has had chance to be used.  Default FALSE.
#' @param type if touch==TRUE, then type controls if a file or directory is created.  One of 'file', 'f', 'directory', or 'd'.  Default 'file'.
#'
#' @return A unique filename that is safe to use without fear of destroying existing files
#' @keywords methods
#'
#' @seealso \code{\link{ask}}
#'
#' @examples
#' #  Create a unique file name with a .R extension.
#' new_unique(c("new_file", NA), ".R", ask=FALSE)
#'
#' @export
new_unique <- function(name=NA, suffix="", ask=FALSE, prompt="A file or directory with this name already exists.  Overwrite?", touch=FALSE, type='file'){

	if(suffix!=""){
		if(strsplit(suffix, split="")[[1]][1]!=".") suffix <- paste(".", suffix, sep="")
	}

	if(!any(type==c('file', 'folder', 'directory', 'dir', 'f', 'd'))) stop('The type specified must be either file or directory')

	if(type=='f') type <- 'file'
	if(any(type==c('folder', 'dir', 'd'))) type <- 'directory'

	for(i in 1:length(name)){
		if(is.na(name[i])){
			alphanumeric <- c(as.numeric(0:9), "a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l", "m", "n", "o", "p", "q", "r", "s", "t", "u", "v", "w", "x", "y", "z", toupper(c("a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l", "m", "n", "o", "p", "q", "r", "s", "t", "u", "v", "w", "x", "y", "z")))
			name[i] <- paste(sample(alphanumeric, 9, replace=TRUE), sep="", collapse="")
		}
	}

	temp <- paste(name, sep="", collapse="")
	exists <- file.exists(paste(temp, suffix, sep=""))
	if(exists==TRUE){
		path.ok <- FALSE
		counter <- 1
		if(ask==TRUE){
			path.ok <- ask(paste("\'", temp, suffix, "\'.  ", prompt, "  ", sep=""), type="logical")
			if(path.ok==TRUE){
				unlink(paste(temp, suffix, sep=""), recursive = TRUE)
			}
		}
		while(path.ok == FALSE){
			temptemp <- paste(temp, "_", counter, "", sep="")
			exists <- file.exists(paste(temptemp, suffix, sep=""))
			if(exists==TRUE){
				counter <- counter + 1
			}else{
				path.ok <- TRUE
				temp <- temptemp
				break
			}
		}
	}

	if(type=='file'){
		suppressWarnings(try(file.create(paste(temp, suffix, sep="")), silent=TRUE))
	}else{
		suppressWarnings(try(dir.create(paste(temp, suffix, sep="")), silent=TRUE))
	}
	permissions <- file.exists(paste(temp, suffix, sep=""))
	if(permissions==FALSE){
		swcat("Error:  Directory not writable\n")
		return("Directory not writable")
	}else{
		if(!touch) unlink(paste(temp, suffix, sep=""), recursive = TRUE)
	}
	if(!touch) suppressWarnings(try(backupforspaces <- file.remove(paste(temp, suffix, sep="")), silent=TRUE))
	return(paste(temp, suffix, sep=""))
}


#' Calculate the Elapsed Time in Sensible Units
#'
#' @description
#' Function to calculate the elapsed time between 2 time periods (in seconds), or to calculate a number of seconds into a time measurement in more sensible units.
#'
#' @param time1 either the time index (from Sys.time()) at the start of the time period, a length of time in seconds, or an object of class 'difftime'.
#' @param time2 either the time index (from Sys.time()) at the end of the time period, or missing data if converting a single length of time. Default NA.
#' @param units either missing, in which case a sensible time unit is chosen automatically, or one of 's', 'm', 'h', 'd', 'w', 'y' to force a specific unit.  Default NA.
#' @param show.units if TRUE, then the time is returned with units, if FALSE then just an integer is returned.  Default TRUE.
#'
#' @return A time measurement, with or without units.
#'
#' @keywords methods
#' @seealso \code{\link{Sys.time}}
#'
#' @examples
#' # Time how long it takes to complete a task:
#'
#' pre.time <- Sys.time()
#' Sys.sleep(2)  # PROCESS TO TIME
#' post.time <- Sys.time()
#' timestring(pre.time, post.time)
#'
#' # Convert 4687 seconds into hours:
#'
#' timestring(4687, units='hours', show.units=FALSE)
#'
#' @export
timestring <- function(time1, time2=NA, units=NA, show.units=TRUE){

	if(all(is.na(time1)) && all(is.na(time2))){
		stop('Either one or both of time1, time2 must be specified as non-NA')
	}else if(all(is.na(time2))){
		time <- time1
	}else if(all(is.na(time1))){
		time <- time2
	}else{
		time <- c(time1, time2)
	}

	if(length(time)==2){
		time <- as.integer(difftime(time[2], time[1], units="secs")*10)
		time <- time/10
	}else{
		if(length(time)==1){
			time <- as.integer(time*10, units="secs")/10
		}else{
			stop("Input variables incorrectly specified")
		}
	}

	secs <- time
	mins <- round(time / (60), digits=1)
	hours <- round(time / (60*60), digits=1)
	days <- round(time / (60*60*24), digits=1)
	weeks <- round(time / (60*60*24*7), digits=1)
	years <- round(time / (60*60*24*7*52), digits=1)

	if(!is.na(units)){
		if(units=="s" | units=="secs" | units=="seconds"){
			if(show.units==FALSE){
				return(secs)
			}else{
				return(paste(secs, " seconds", sep=""))
			}
		}
		if(units=="m" | units=="minutes" | units=="mins"){
			if(show.units==FALSE){
				return(mins)
			}else{
				return(paste(mins, " minutes", sep=""))
			}
		}
		if(units=="h" | units=="hours"){
			if(show.units==FALSE){
				return(hours)
			}else{
				return(paste(hours, " hours", sep=""))
			}
		}
		if(units=="d" | units=="days"){
			if(show.units==FALSE){
				return(days)
			}else{
				return(paste(days, " days", sep=""))
			}
		}
		if(units=="w" | units=="weeks"){
			if(show.units==FALSE){
				return(weeks)
			}else{
				return(paste(weeks, " weeks", sep=""))
			}
		}
		if(units=="y" | units=="years"){
			if(show.units==FALSE){
				return(years)
			}else{
				return(paste(years, " years", sep=""))
			}
		}
		swcat("Error:  Unrecognised unit type '", units, "'\n", sep="")
	}

	if(secs < 60){
		if(show.units==FALSE){
			return(secs)
		}else{
			return(paste(secs, " seconds", sep=""))
		}
	}
	if(mins < 60){
		if(show.units==FALSE){
			return(mins)
		}else{
			return(paste(mins, " minutes", sep=""))
		}
	}
	if(hours < 24){
		if(show.units==FALSE){
			return(hours)
		}else{
			return(paste(hours, " hours", sep=""))
		}
	}
	if(days < 7){
		if(show.units==FALSE){
			return(days)
		}else{
			return(paste(days, " days", sep=""))
		}
	}
	if(weeks < 52){
		if(show.units==FALSE){
			return(weeks)
		}else{
			return(paste(weeks, " weeks", sep=""))
		}
	}

	if(show.units==FALSE){
		return(years)
	}else{
		return(paste(years, " years", sep=""))
	}

}

#' Analyse the System to Check That JAGS Is Installed
#'
#' @name testjags
#' @aliases testjags testJAGS
#'
#' @description Test the users system to determine the operating system, version of R installed, and version of JAGS installed.  Some information is collected from other functions such as .platform and Sys.info.  Used by the run.jags function.
#'
#' @param jags the system call or path for activating JAGS.  Default calls findjags() to attempt to locate JAGS on your system automatically.  In unix the system call should always be 'jags', in Windows a path to the JAGS executable or the enclosing /bin or /JAGS folder is required.
#' @param silent should on-screen feedback be suppressed?  Default FALSE.
#'
#' @return A named list of values containing information about the JAGS installs found on the user's system (returned invisibly).
#'
#' @keywords methods
#' @seealso \code{\link{run.jags}}, \code{\link{findjags}}
#'
#' @examples
#' # Run the function to determine if JAGS is installed:
#' testjags()
#' testjags('some/jags/path')

#' @rdname testjags
#' @export
testjags <- function(jags=runjags.getOption('jagspath'), silent=FALSE){

	# Evaluate jags outside a suppresswarnings:
	jags <- jags

	tempfile <- new_unique(name=tempfile("test"), suffix=".cmd", touch=TRUE, type='file')
	write("exit", file=tempfile)
	s.info <- Sys.info()
	p.info <- .Platform

	os <- p.info$OS.type
	username <- as.character(s.info["user"])
	rversion <- R.version$version
	gui <- p.info$GUI
	p.type <- p.info$pkgType

	libpaths <- NULL
	if(os=="windows"){

		if(file.exists(paste(jags, 'bin', .Platform$file.sep, 'jags-terminal.exe', sep=''))) jags <- paste(jags, 'bin/jags-terminal.exe', sep='')
		if(file.exists(paste(jags, .Platform$file.sep, 'bin', .Platform$file.sep, 'jags-terminal.exe', sep=''))) jags <- paste(jags, '/bin/jags-terminal.exe', sep='')
		if(file.exists(paste(jags, 'jags-terminal.exe', sep=''))) jags <- paste(jags, 'jags-terminal.exe', sep='')
		if(file.exists(paste(jags, .Platform$file.sep, 'jags-terminal.exe', sep=''))) jags <- paste(jags, '/jags-terminal.exe', sep='')

		path <- strsplit(jags, split=paste(.Platform$file.sep, 'bin', sep=''), fixed=TRUE)

		firstpath <- path[[1]][1:length(path[[1]])-1]

		binpath <- paste(firstpath, .Platform$file.sep, 'bin;', sep='')
		libpath <- paste(firstpath, .Platform$file.sep, 'modules;', sep='')

		libpaths <- list(PATH=binpath, LTDL_LIBRARY_PATH=libpath)
	}

	suppressWarnings(returnval <- try(system2(jags, args=tempfile, stdout=TRUE, stderr=TRUE), silent=TRUE))
	unlink(tempfile)

	if(inherits(returnval, "try-error")){
		success <- 0
	}else{
		if(is.null(attributes(returnval))){
			success <- 1
		}else{
			success <- -1
		}
	}

	if(length(returnval)==0){
		success <- 0
#		warning(paste("Running the command ", jags, " returned no output - ensure the path is correct and possibly try reinstalling jags\n", sep=""))
	}
	if(!any(grepl("Welcome",returnval))){
		success <- 0
#		warning(paste("Running the command ", jags, " did not return the expected welcome message - ensure the path is correct and possibly try reinstalling jags\n", sep=""))
	}

	if(success){

    	rightstring <- which(grepl("Welcome",returnval))[1]
    	if(is.na(rightstring)){  # Will be NA if which is length 0 as selected first element above
    		version <- 'unknown'
    		num.version <- NA
    	}else{
    		version <- strsplit(returnval[rightstring], split=" ", fixed=TRUE)[[1]][4]
			versioncom <- version
			if(grepl('(', versioncom, fixed=TRUE)){
				versioncom <- strsplit(versioncom, split='(', fixed=TRUE)[[1]][1]
			}
			versioncom <- sub('.',';',versioncom,fixed=TRUE)
			versioncom <- gsub('.','',versioncom,fixed=TRUE)
			num.version <- as.numeric(gsub(';','.',versioncom,fixed=TRUE))
			if(is.na(version)) version <- 'unknown'
    	}
    }

	if(any(.packages(TRUE)=="rjags")){
		rjags.avail <- TRUE
		rjags.major <- packageVersion('rjags')$major
		rjags.version <- packageDescription('rjags', fields='Version')
	}else{
		rjags.avail <- FALSE
		rjags.major <- 0
		rjags.version <- ""
	}

	if(!silent){
	  ## Specific output for macOS:
	  pkt <- .Platform$pkgType
	  if(grepl("mac", pkt)){
	    r_type <- ifelse(grepl("arm64",pkt), "arm64", "x86_64")
	    c_type <- ifelse(grepl("arm64",Sys.info()["machine"]), "arm64", "x86_64")
	    swcat("You are using ", gsub(")", paste0("; macOS ", r_type, ")"), rversion), " on a unix (macOS ", c_type, ") machine, with the ", gui, " GUI\n", sep="")
	  }else{
	    swcat("You are using ", rversion, " on a ", os, " machine, with the ", gui, " GUI\n", sep="")
	  }
	}

	if(jags=="JAGS not found") jags <- "findjags()"

	jags.major <- NA
	if(success){
		if(success==-1){
			if(!silent) suppressWarnings(swcat("JAGS version ", version, " found successfully using the command '", jags, "', but returned the status code '", attributes(returnval)$status, "' - this may indicate a compatibility issue, procede with caution\n", sep=""))
		}else{
		  ## Specific output for macOS:
		  pkt <- .Platform$pkgType
		  if(grepl("mac", pkt)){
		    r_type <- ifelse(grepl("arm64",pkt), "arm64", "x86_64")
		    c_type <- ifelse(grepl("arm64",Sys.info()["machine"]), "arm64", "x86_64")
		    j_type <- system(paste0("lipo -archs ", gsub("/bin/jags","/libexec/jags-terminal", jags)), intern=TRUE)
		    ss <- try({
		      if(j_type=="x86_64 arm64") j_type <- "universal"
		    }, silent=TRUE)
		    if(inherits(ss,"try-error")){
		      ## This may be due to invalid active developer path ... assume universal:
		      j_type <- "universal"
		      if(!runjagsprivate[["warned_macos_lipo"]]) warning("Reading the binary type of JAGS failed - do you need to update the Xcode Command Line Tools?")
		      runjagsprivate[["warned_macos_lipo"]] <- TRUE
		    }
		    if(grepl("build",returnval[rightstring])){
		      bld <- paste0("macOS ", j_type, " build")
		    }else{
		      bld <- paste0(j_type, " binary")
		    }
		    if(!silent) suppressWarnings(swcat("JAGS version ", version, " (", bld, ") found successfully using the command '", jags, "'\n", sep=""))
		    if(!j_type=="universal" && (j_type!=r_type)){
		      warning(paste0("Your R build (", r_type, ") does not match your JAGS build (", j_type, "): you should re-install JAGS and/or R so they match"))
		    }
		    if(!silent && !j_type=="universal" && (j_type!=c_type)){
		      swcat("Your JAGS build (", j_type, ") does not match your CPU architecture (", c_type, "): JAGS will run under Rosetta2 emulation, which will decrease performance")
		    }
		  }else{
		    if(!silent) suppressWarnings(swcat("JAGS version ", version, " found successfully using the command '", jags, "'\n", sep=""))
		  }
		}
		if(!is.na(num.version) & num.version<1){
			warning(paste("The version of JAGS currently installed on your system is no longer supported.  Please update JAGS from https://mcmc-jags.sourceforge.io/\n"))
			jags.avail <- FALSE
		}else{
			jags.avail <- TRUE
		}
		jagsfound <- TRUE
		jags.major <- floor(num.version)
	}else{
		if(rjags.avail){
			if(!silent) suppressWarnings(swcat("JAGS was not found on your system using the command '", jags, "'.  Please ensure that the command is correct and that the latest version of JAGS is installed from https://mcmc-jags.sourceforge.io/\n", sep=""))
			jags.avail <- TRUE
			# If it's just rjags assume the version number is high enough:
			num.version <- Inf
			jags.major <- NA
		}else{
			if(!silent) suppressWarnings(swcat("JAGS was not found on your system using the command '", jags, "'.  Please ensure that the command is correct.\n", sep=""))
			jags.avail <- FALSE
			num.version <- "none found"
			jags.major <- NA
		}
		if(gui %in% c('RStudio', 'AQUA') && !silent)
			swcat("Note that as of OS X Yosemite, the FULL path must be provided to JAGS as the $PATH global variable is not passed to processes started from within a GUI application\n", sep="")

		jagsfound <- FALSE
	}

	if(rjags.avail && num.version < Inf){  # If rjags and JAGS are both findable independently
		if(success!=0 && rjags.major!=floor(num.version)){   # If JAGS is installed and rjags major != JAGS major
			if(!silent)
				swcat("The rjags package version ", rjags.major, ".x is installed, but this is not compatible with your installed version of JAGS (version ", floor(num.version), ".x)\n",sep="")

			if(rjags.major > num.version){
				msg <- paste("You should update to JAGS version ", rjags.major, ".x from https://sourceforge.net/projects/mcmc-jags/files/JAGS/", sep="")

			}else{
				msg <- paste("You should update the rjags package to version ", floor(num.version), ".x - if the version on CRAN is not currently up to date, try downloading from https://sourceforge.net/projects/mcmc-jags/files/rjags/ instead", sep="")
			}
			if(!silent)
				swcat(msg,"\n")

			if(!runjagsprivate$warned_version_mismatch){
				warning(paste(msg, "\n(This warning is given once per R session)"), call. =FALSE)
				runjagsprivate$warned_version_mismatch <- TRUE
			}

		}else if(!silent){
			swcat("The rjags package is installed\n",sep="")
		}
	}else if(rjags.avail && num.version == Inf && !silent){
		swcat("The rjags package version ", rjags.major, ".x is installed (compatibility with the major version of JAGS cannot be verified)\n",sep="")
	}else if(!rjags.avail && !silent){
		swcat("The rjags package is not installed\n",sep="")
	}

	invisible(list("os"=os, "JAGS.available"=jags.avail, "JAGS.found"=jagsfound, "rjags.found"=rjags.avail, "rjags.version"=rjags.version, "rjags.major"=rjags.major, "JAGS.path"=jags, "JAGS.version"=version, "JAGS.major"=jags.major, "R.version"=rversion, "R.GUI"=gui, "R.package.type"=p.type, "username"=username, libpaths=libpaths))
}

findJAGS <- findjags
testJAGS <- testjags



runjagsprivate <- new.env()
# Use 'expression' for functions to avoid having to evaluate before the package is fully loaded:
assign("defaultoptions",list(jagspath=expression(findjags()),method=expression(if('rjags' %in% .packages(TRUE)){'rjags'}else{if(Sys.info()['user']=='nobody') 'simple' else 'interruptible'}), tempdir=TRUE, plot.layout=c(2,2), new.windows=TRUE, modules="", factories="", bg.alert='beep', linenumbers=TRUE, inits.warning=TRUE, rng.warning=TRUE, summary.warning=TRUE, blockcombine.warning=TRUE, blockignore.warning=TRUE, tempdir.warning=FALSE, nodata.warning=TRUE, adapt.incomplete='warning', repeatable.methods=FALSE, silent.jags=FALSE, silent.runjags=FALSE, predraw.plots=FALSE, force.summary=FALSE, mode.continuous=expression('modeest' %in% .packages(TRUE)), timeout.import=30, partial.import=FALSE, keep.crashed.files=TRUE, full.cleanup=FALSE, debug=FALSE, jags5=FALSE), envir=runjagsprivate)

assign("options",runjagsprivate$defaultoptions,envir=runjagsprivate)
assign("rjagsmethod",c('rjags','rjparallel'),envir=runjagsprivate)
assign("bgmethod",c('background','bgparallel'),envir=runjagsprivate)
assign("parallelmethod",c('parallel','bgparallel','snow','rjparallel','xgrid'),envir=runjagsprivate)
assign("runjagsversion", "notset", envir=runjagsprivate)
assign("simfolders", character(0), envir=runjagsprivate)
assign("failedsimfolders", character(0), envir=runjagsprivate)
assign("defaultsummarypars", list(vars=NA, mutate=NULL, psrf.target = 1.05, normalise.mcmc = TRUE, modeest.opts=list(), confidence=c(0.95), autocorr.lags=c(10), custom=NULL, silent.jags=expression(runjags.getOption('silent.jags')), plots=FALSE, plot.type=c('trace','ecdf','histogram','autocorr','key','crosscorr'), col=NA, summary.iters=10000, trace.iters=1000, separate.chains=FALSE, trace.options=list(), density.options=list(), histogram.options=list(), ecdfplot.options=list(), acplot.options=list()), envir=runjagsprivate)
assign("minjagsmajor", 3, envir=runjagsprivate)
assign("maxjagsmajor", 4, envir=runjagsprivate)
assign("warned_version_mismatch", FALSE, envir=runjagsprivate)
assign("warned_macos_lipo", FALSE, envir=runjagsprivate)

	# runjags.getOption is not available at compile time so has to be expression, but it's OK as it is eval()ed when getting defaultsumpars
getdefaultsummarypars <- function(){
	x <- runjagsprivate$defaultsummarypars
	x$silent.jags <- eval(x$silent.jags)
	return(x)
}
assign("defaultmethodoptions", list(n.sims=NA, cl=NA, remote.jags="*//*usefindjags*//*", rjags=NA, by=NA, progress.bar=expression(unlist(options('jags.pb'))), jags=expression(runjags.getOption('jagspath')), silent.jags=expression(runjags.getOption('silent.jags')), jags.refresh=0.1, batch.jags=expression(runjags.getOption('silent.jags'))), envir=runjagsprivate)
	# jags.pb is set by runjags - but its ok as it is eval()uated before use
	# is.na(cl/by) is what is expected if they are to be set at run time
getdefaultmethodoptions <- function(){
	x <- runjagsprivate$defaultmethodoptions
	x$progress.bar <- eval(x$progress.bar)
	x$silent.jags <- eval(x$silent.jags)
	x$jags <- eval(x$jags)
	x$batch.jags <- eval(x$batch.jags)
	if(is.null(x$progress.bar))   # Will be NULL if rjags isn't installed
    x$progress.bar <- 'text'
	return(x)
}

#' Options for the runjags package
#'
#' @name runjags.options
#' @aliases runjags.options runJAGS.options runjags.getOption runJAGS.getOption
#'
#' @description
#' Utility function to change the default options for the runjags package. Options will be used for all runjags function calls until the runjags package is unloaded.  For a permanent solution, create a named list called '.runjags.options' containing the desired options in an R profile file - on loading, runjags will check to see if this object exists in the global environment and set the options automatically.
#'
#' @details
#' The following default options can be specified:
#'
#' \itemize{
#' \item \bold{jagspath} - the path to JAGS to use unless over-ridden in a function call
#' (uses the findjags() function by default).
#'
#' \item \bold{method} - the runjags method to use unless over-ridden in a function call
#' (default is 'rjags' if the rjags package is installed, or 'interruptible'
#' otherwise).
#'
#' \item \bold{tempdir} - default to temporary directory unless over-ridden in a
#' function call (default TRUE).
#'
#' \item \bold{plot.layout} - the layout for plots unless over-ridden in a function
#' call. Must be a numeric vector of length 2.
#'
#' \item \bold{new.windows} - use multiple windows for plots unless over-ridden in a
#' function call (default is platform dependent).
#'
#' \item \bold{modules} - the modules to load unless over-ridden in a function call
#' (default none).
#'
#' \item \bold{factories} - the factories to load unless over-ridden in a function call
#' (default none).
#'
#' \item \bold{bg.alert} - an optional command to run once background JAGS processes have
#' completed. Note that this command is run on the command line via system(),
#' so will be system dependent. The default attempts to make an alert sound
#' using a system appropriate method, which may not work on all platforms.
#'
#' \item \bold{linenumbers} - display line numbers when printing runjags model, data and
#' inits class objects unless over-ridden in a function call (default none).
#'
#' \item \bold{inits.warning} - display warning messages about initial values being not
#' specified or re-used.
#'
#' \item \bold{rng.warning} - display warning messages relating to pseudo-random number
#' generation for parallel chains.
#'
#' \item \bold{summary.warning} - display a warning message if summary statistics are
#' requested for a small number of samples (and a few other similar situations).
#'
#' \item \bold{blockcombine.warning} - display a warning message if multiple data or
#' inits blocks are combined in a model file.
#'
#' \item \bold{blockignore.warning} - display a warning message if ignoring monitors,
#' data or inits in the model file because a character argument was given for the same
#' parameters to the run.jags function.
#'
#' \item \bold{tempdir.warning} - display a warning message if tempdir=TRUE is requested
#' with a background method.
#'
#' \item \bold{nodata.warning} - display a warning message if the model has been run
#' without any data.
#'
#' \item \bold{adapt.incomplete} - all models are checked to make sure that the
#' adaptive phase has completed - this option controls the behaviour of runjags if
#' this adaptation is not complete before MCMC sampling. If adapt.incomplete='silent'
#' no action is taken, if 'warning' then the model run is continued but a warning is
#' given after the simulation is finished, and if 'error' an error will be returned.
#' Note that for most methods the error is returned immediately following the
#' adapt/burnin phases (so the sample iterations are not run), but for the simple
#' and snow methods the full model will be run before the error is given.
#'
#' \item \bold{repeatable.methods} - option to ensure that the MCMC object produced by
#' the rjags and rjparallel methods are identical to those produced by other
#' methods (given the same starting values). This is primarily for extending
#' compiled models, where additional burnin iterations will be done
#' to replace unnecessary adpative steps for consistency with other methods, and
#' following dic sampling, where the rjags model will be reset to the state it was in
#' before dic sampling.  Note that the precision of the numbers returned may differ
#' between methods on some platforms.
#'
#' \item \bold{silent.jags} - suppress output of JAGS (or rjags) when updating models.
#'
#' \item \bold{silent.runjags} - suppress feedback provided by the runjags functions.
#'
#' \item \bold{predraw.plots} - automatically pre-calculate convergence diagnostic plots
#' (this will save time when displaying plots at the cost of increased
#' storage requirement for the runjags object).
#'
#' \item \bold{force.summary} - override the default behaviour to omit calculation of
#' summary statistics for greater than 50 variables, and attempt to
#' calculate summary statistics anyway (this may cause long delays in
#' the processing of results).
#'
#' \item \bold{mode.continuous} - calculate the mode of continuous variables for summary
#' statistics (requires the "modeest" package to be installed).
#'
#' \item \bold{timeout.import} - the maximum number of seconds for runjags to wait for
#' coda files to finish being written before giving up. If a large
#' number of monitored variables are being written, either the timeout
#' can be increased or results.jags() can be used once the files have
#' been written.
#'
#' \item \bold{partial.import} - force runjags to read in successful simulations even
#' when parallel simulations crashed. If this option is set to TRUE,
#' it is not guaranteed that a model result will contain the requested
#' number of chains!
#'
#' \item \bold{keep.crashed.files} - allows folders containing crashed simulations to be
#' preserved even if keep.jags.files = FALSE. Any folders kept will be
#' deleted when runjags is unlaoded or when R quits.
#'
#' \item \bold{full.cleanup} - when unloading the runjags package, should all simulation
#' folders preserved using keep.jags.files=TRUE be deleted?  This option
#' may not work as expected on all systems when quitting R, but should
#' always work for unloadNamespace('runjags').  Note also that folders
#' for any failed JAGS runs are *always* deleted on exit - if you want to
#' keep these, they will have to be copied manually.
#'
#' \item \bold{debug} - display internal debugging output.
#' }
#'
#' @param name the name of the option to get the current value of - for a  list of available options, see details below.
#' @param ... named option(s) to change - for a list of available options, see details below.
#'
#' @keywords methods
#' @return The current value of all available runjags options (after applying any changes specified) is returned invisibly as a named list.
#' @seealso \code{\link{run.jags}}, \code{\link{findjags}}, \code{\link{runjags-class}}
#'
#' @examples
#' \dontrun{
#'
#' # Create a list of options in the global environment (perhaps in an
#' # R startup profile file) BEFORE load()ing runjags:
#' .runjags.options <- list(inits.warning=FALSE, rng.warning=FALSE)
#' # Or if it is run in a different environment:
#' # .runjags.options <<- list(inits.warning=FALSE, rng.warning=FALSE)
#'
#' # Then load runjags and verify that the options have been set:
#' library('runjags')
#' print(runjags.options())
#'
#'
#' # Change the default option to remove all feedback provided by
#' # runjags and JAGS/rjags (only errors will be printed to screen):
#' runjags.options(silent.jags=TRUE, silent.runjags=TRUE)
#'
#' }
#'
NULL

#' @rdname runjags.options
#' @export
runjags.options <- function(...){
	opts <- list(...)

	# For backwards compatibility:
	if(any(names(opts)=='newwindows')){
		opts$new.windows <- opts$newwindows
		opts$newwindows <- NULL
	}

	if(length(opts)>0){
		options <- runjagsprivate$options
		recognised <- pmatch(names(opts), names(options))
		if(any(is.na(recognised))){
			warning(paste("Igoring unmatched or ambiguous option(s): ", paste(names(opts)[is.na(recognised)],collapse=", ")))
      opts <- opts[!is.na(recognised)]
		}
		optnames <- names(options)[recognised[!is.na(recognised)]]
		if(length(optnames)>0) for(i in 1:length(optnames)){
			options[optnames[i]] <- opts[[i]]
		}
		assign("options",options,envir=runjagsprivate)
	}

	# Some checks for valid option settings:
	if((!is.numeric(runjags.getOption('plot.layout')) || length(runjags.getOption('plot.layout'))!=2))
		stop('The "plot.layout" option must be a numeric vector of length 2')

	runjagsprivate$options$adapt.incomplete <- c('error','warning','silent')[pmatch(runjagsprivate$options$adapt.incomplete,c('error','warning','silent'))]
	if(is.na(runjagsprivate$options$adapt.incomplete))
		stop('The "adapt.incomplete" option must be either error, warning or silent')

	invisible(runjagsprivate$options)
}

#' @rdname runjags.options
#' @export
runjags.getOption <- function(name){
	if(length(name)!=1) stop("Only 1 option can be retrieved at a time")
	opt <- pmatch(name,names(runjagsprivate$options))
	if(is.na(opt)) stop(paste("Unmatched or ambiguous option '", name, "'", sep=""))
	# Use eval as some defaults are put in using 'expression' to avoid evaluating at load time:
	return(eval(runjagsprivate$options[[opt]]))
}

#' @rdname runjags.options
#' @export
runJAGS.getOption <- runjags.getOption


failedjags <- new.env()

assign("model", NA, envir=failedjags)
assign("data", NA, envir=failedjags)
assign("inits", NA, envir=failedjags)
assign("output", NA, envir=failedjags)
assign("end.state", NA, envir=failedjags)

