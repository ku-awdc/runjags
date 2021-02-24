as.bugsarray <- function(x, ...){
	UseMethod("as.bugsarray")	
}

as.bugsarray.default <- function(x, ...){
	stop("No default as.bugsarray method available")
}

print.bugsarray <- function(x, ...){
    if(length(dim(x))!=3){
      stop("Invalid bugsarray object")
    }
    print(summary(x, mean))
}

# Helper function for as.bugsarray.mcarray and as.bugsarray.list
process.mcarray <- function(x){
	
	stopifnot(inherits(x, 'mcarray'))
	
	## Get dims of input mcarray:
	dims <- attr(x, 'dim', exact=TRUE)

	# Ensure dim and dimnames are correctly set:
	if(is.null(dims)){
		dims <- length(x)
	}
	if(is.null(names(dims))){
		names(dims) <- rep('', length(dims))
	}
	dnames <- names(dims)

	if(length(dims)==1){
		arraydims <- dims
	}else{
		arraydims <- dims[! names(dims) %in% c('iteration','chain')]
	}
  
  	## Get the variable names:
	varname <- attr(x, "varname", exact=TRUE)
	vartype <- attr(x, "type", exact=TRUE)
	# If this is a deviance-type monitor where variables are NOT pooled:
#	if(varname=="_observed_stochastic_"
#			&& vartype != 'pv'
#			&& !grepl('_total', vartype, fixed=TRUE) ){
#		if(!is.null(attr(x, "elementnames"))){
#			colnames <- attr(x, "elementnames")
#		}else{
#			colnames <- varname
#		}
	# If a deviance-type monitor with pooled variables:
#	}else if(varname=="_observed_stochastic_"){
#		colnames <- vartype
	# Otherwise use coda.names:
#	}else{
#		colnames <- coda.names(varname, arraydims)
#	}
  
	# Catch old deviance/pd/popt monitors:
#	if(varname %in% c('deviance','pD','popt')
#	&& vartype %in% c('mean','trace')){
#		colnames <- varname
#		if(prod(arraydims)>1){
#			colnames <- paste0(varname,'[', 1:prod(arraydims), ']')
#		}else{
#			colnames <- varname
#		}
#	}

#	names(colnames) <- NULL
	
	if(!is.null(attr(x, "elementnames"))){
		colnames <- attr(x, "elementnames")
	}else{
		colnames <- coda.names(varname, arraydims)
	}
	
	
	if('iteration' %in% dnames){
		it <- attr(x, 'iterations')
		iternames <- paste0('Iteration ', seq(it[1], it[2], it[3]))
		niters <- dims['iteration']
		if(length(iternames) != niters){
			stop(paste0("Bad iteration attribute for ", varname, ": ", length(seq(it[1], it[2], it[3])), " iterations implied but ", niters, " detected"))
		}
	}else{
		iternames <- 'Iteration 0'
		niters <- 0
	}
	if('chain' %in% dnames){
		nchains <- dims['chain']
	}else{
		nchains <- 0
	}

	if(length(colnames)!=prod(arraydims)){
		stop(paste0('Length of variable names (', length(colnames), 
		') does not match the array length (', prod(arraydims), ') for ', varname))
	}
  	
	alldims <- c(prod(arraydims), niters, nchains)
	names(alldims) <- c('array', 'iteration', 'chain')
	
	return(list(type=vartype, varname=varname, dims=alldims, arraydims=arraydims, 
		colnames=colnames, nchains=nchains, niters=niters, iternames=iternames))
	
}

as.bugsarray.mcarray <- function(x, chains=NULL, ...){
  
    if(!all(c("varname","type","iterations")%in%names(attributes(x))))
		stop("Invalid mcarray object")
	
	# Get required attributes:
	attribs <- process.mcarray(x)
	dims <- attribs[["dims"]]
	arraydims <- attribs[["arraydims"]]
	colnames <- attribs[["colnames"]]
	nchains <- attribs[["nchains"]]
	niters <- attribs[["niters"]]
	iteration_names <- attribs[["iternames"]]

	if(nchains>0){
		chain_names <- paste0('Chain ', 1:dims['chain'])
	}else{
		chain_names <- 'Pooled'
	}
	if(is.null(chains)){  	
		if(identical(chain_names,'Pooled'))
			chains <- 0
		else
			chains <- 1:dims['chain']
	}

	if(!is.numeric(chains) || length(chains)==0){
		stop('The chains argument must give a vector of chains to extract (or 0 for pooled estimates, or NULL for all chains)')
	}
	if(any(chains==0) && !length(chains)==1){
		stop('If chains is 0 then it must be length 1')
	}
	if((any(chains>0)&&identical(chain_names,'Pooled')) || any(chains>length(chain_names))){
		stop("Requested chain number is out of range")
	}


	if(!is.null(dim(x))){
		ard <- length(arraydims)
		perm <- c(if(dims['chain']>0) ard+1+(dims['iteration']>0), 
			if(dims['iteration']>0) ard+1, 1:ard)
		stopifnot(length(dim(x))==length(perm))

		flatx <- array(aperm(x, perm=perm), dim=pmax(1,dims[c('chain','iteration','array')]), 
			dimnames=list(chain_names, iteration_names, colnames))
	}else{
		# Only if there are no chains or iterations and a single array dim:
		flatx <- array(as.numeric(x), dim=c(1,1,dims['array']), 
			dimnames=list(chain_names, iteration_names, colnames))
	}
  
	# Guarantee that the new array will be no larger than flatx:
	chains <- sort(unique(chains))
	if(!all(paste('Chain',chains)==chain_names)){
		## chains=0 means pool the chains (will be length 1):
		if(chains[1]==0){
			if(!identical(chain_names,'Pooled')){
				ans <- apply(flatx,c(2,3),mean)
				flatx[1,,] <- ans[]
				flatx <- flatx[1,,,drop=FALSE]
			}
			dimnames(flatx)[[1]] <- 'Pooled'
		}else{
			flatx <- flatx[chains,,,drop=FALSE]
		}
	}

	class(flatx) <- 'bugsarray'
	attr(flatx, 'type') <- attr(x, 'type', exact=TRUE)
	return(flatx)
}

as.bugsarray.list <- function(x, chains=NULL, ...){
  
  	if(length(x)==0){
  		stop('Cannot process an empty list')
  	}
	
	if(!all(sapply(x, inherits, what='mcarray'))){
		stop("The as.bugsarray method for list class must be given a list of mcarray objects")
	}

	## Check all types match:
	type <- attr(x[[1]], 'type', exact=TRUE)
	if(!all(sapply(x, attr, which='type', exact=TRUE) == type)){
		stop("Non-conforming monitor types in list of mcarray objects")
	}
	
	## Get attributes:
	allattribs <- lapply(x, process.mcarray)
	alldims <- do.call('rbind', lapply(allattribs, function(y) y[['dims']]))
	
	# Make a note of presence of pd/popt:
	islegacy <- sapply(allattribs, function(y) y[['type']]) %in% c('mean', 'type') &
		sapply(allattribs, function(y) y[['varname']]) %in% c('deviance', 'pD', 'popt')
	
	## Get all dims of input mcarray list:
	nchains <- max(alldims[,'chain'])
	if(any(alldims[,'chain'] < nchains)){
		problem <- alldims[,'chain'] < nchains		
		msg <- paste0('Dropping ', sum(problem), ' non-conforming mcarrays')
		if(any(islegacy & problem))
			msg <- paste(msg, '(for model fit statistics use e.g. type="pD" rather than variable.name="pD")')
		warning(msg)
		alldims <- alldims[!problem,,drop=FALSE]
		allattribs <- allattribs[!problem]
		x <- x[!problem]
	}
	niter <- max(alldims[,'iteration'])
	if(any(alldims[,'iteration'] < niter)){
		problem <- alldims[,'iteration'] < niter
		msg <- paste0('Dropping ', sum(problem), ' non-conforming mcarrays')
		if(any(islegacy & problem))
			msg <- paste(msg, '(for model fit statistics use e.g. type="pD" rather than variable.name="pD")')
		warning(msg)
		alldims <- alldims[!problem,,drop=FALSE]
		allattribs <- allattribs[!problem]
		x <- x[!problem]
	}
	
	arraydims <- lapply(allattribs, function(y) y[['arraydims']])

	# Hope they are all the same (length is guaranteed):
	iteration_names <- allattribs[[1]][['iternames']]  	
	
	if(nchains>0){
		chain_names <- paste0('Chain ', 1:nchains)
	}else{
		chain_names <- 'Pooled'
	}
	if(is.null(chains)){  	
		if(identical(chain_names,'Pooled'))
			chains <- 0
		else
			chains <- 1:nchains
	}

	if(!is.numeric(chains) || length(chains)==0){
		stop('The chains argument must give a vector of chains to extract (or 0 for pooled estimates, or NULL for all chains)')
	}
	if(any(chains==0) && !length(chains)==1){
		stop('If chains is 0 then it must be length 1')
	}
	if((any(chains>0)&&identical(chain_names,'Pooled')) || any(chains>length(chain_names))){
		stop("Requested chain number is out of range")
	}
	
	allvarnames <- do.call('c', lapply(allattribs, function(y) y[['colnames']]))
	names(allvarnames) <- NULL

	flatx <- array(0, dim=c(length(chain_names), length(iteration_names), length(allvarnames)), 
                    dimnames=list(chain_names, iteration_names, allvarnames))
	ends <- cumsum(alldims[,'array'])
	starts <- c(1, ends[-length(ends)]+1)
	for(i in 1:length(x)){
		if(is.null(dim(x[[i]]))){
			# Only if there are no chains or iterations and a single array dim:
			flatx[1,1,starts[i]:ends[i]] <- x[[i]]
		}else{
			ard <- length(arraydims[[i]])
			perm <- c(if(alldims[i,'chain']>0) ard+1+(alldims[i,'iteration']>0), 
				if(alldims[i,'iteration']>0) ard+1, 1:ard)
			stopifnot(length(dim(x[[i]]))==length(perm))
			flatx[,,starts[i]:ends[i]] <- aperm(x[[i]], perm=perm)
		}
	}
  
	# Guarantee that the new array will be no larger than flatx:
	chains <- sort(unique(chains))
	if(!all(paste('Chain',chains)==chain_names)){
		## chains=0 means pool the chains (will be length 1):
		if(chains[1]==0){
			if(length(chain_names) > 1){
				ans <- apply(flatx,c(2,3),mean)
				flatx[1,,] <- ans[]
				flatx <- flatx[1,,,drop=FALSE]
			}
			dimnames(flatx)[[1]] <- 'Pooled'
		}else{
			flatx <- flatx[chains,,,drop=FALSE]
		}
	}

	class(flatx) <- 'bugsarray'
	attr(flatx, 'type') <- type
	return(flatx)
}

as.mcmc.bugsarray <- function(x, ...){
  if(length(dim(x))!=3){
    stop("Invalid bugsarray object")
  }
  if(any("Iteration 0" %in% dimnames(x)[[2]])){
    stop("Cannot convert a monitor type with pooled iterations into an MCMC object")
  }
  if(dim(x)[1] > 1){
    stop("Cannot convert a monitor type with separate chains into a single MCMC object - try as.mcmc.list instead")
  }
  
  # Carefully drop the pooled chain dimension but preserve varnames:
  ans <- x[1,,,drop=FALSE]
  dim(ans) <- dim(x)[-1]
  dimnames(ans) <- list(NULL, dimnames(x)[[3]])
  ans <- as.mcmc(ans)  
  return(ans)
}

as.mcmc.list.bugsarray <- function(x, ...){
  if(length(dim(x))!=3){
    stop("Invalid bugsarray object")
  }
  if(any("Iteration 0" %in% dimnames(x)[[2]])){
    stop("Cannot convert a monitor type with pooled iterations into an MCMC list object")
  }
  
  ans <- as.mcmc.list(lapply(1:dim(x)[1], function(i){
	  # Need to convert back to bugsarray and copy attributes:
	  new <- x[i,,,drop=FALSE]
	  attrs <- attributes(x)
	  attrs$dim[1] <- 1
	  attrs$dimnames[[1]] <- attrs$dimnames[[1]][i]
	  attributes(new) <- attrs
	  return(as.mcmc(new))
  }))
  return(ans)
  
}


summary.bugsarray <- function(object, FUN, ...){
    if(length(dim(object))!=3){
      stop("Invalid bugsarray object")
    }
	
    drop.dims <- c("chain","iteration")[dim(object)[1:2]>1] 

    ans <- list("stat"=apply(object, 3, FUN, ...),
                "drop.dims" = drop.dims)
    class(ans) <- "summary.mcarray"

    return(ans)
}
