#' Create a Hui-Walter model based on paired test data for an arbitrary number of tests and populations
#'
#' @param testdata the input paired test data, where each column name corresponds to a test result - except possibly "ID" which is ignored, and "Population" indicating a population identifier for that row. Each row must represent test results from the same individual either as logical or a factor with two levels (and where the first level indicates a negative test result). Data may be missing at random (except for Population).
#' @param outfile the name of the text file to save the model representation
#' @param covariance should covariance terms be activated or omitted?
#' @param se_priors the priors to use for sensitivity parameters (can be adjusted in the model once it is generated)
#' @param sp_priors the priors to use for specificity parameters (can be adjusted in the model once it is generated)
#' @param cov_as_cor option for the prior for covariance terms to be put on the correlation rather than covariance directly
#'
#' @examples
#' N <- 600
#' status <- rbinom(N, 1, rep(c(0.25,0.5,0.75), each=N/3))
#' testdata <- data.frame(Population = rep(1:3, each=N/3),
#'     Test1 = rbinom(N, 1, status*0.75 + (1-status)*0.05),
#'     Test2 = rbinom(N, 1, status*0.75 + (1-status)*0.05),
#'     Test3=rbinom(N, 1, status*0.75 + (1-status)*0.05)
#' )
#' template_huiwalter(testdata, outfile="huiwalter_model.txt", covariance=TRUE)
#' \dontrun{
#' results <- run.jags("huiwalter_model.txt")
#' }
#' unlink("huiwalter_model.txt")
#'
#' @export
template_huiwalter <- function(testdata, outfile='huiwalter_model.txt', covariance=FALSE, se_priors='dbeta(1,1)', sp_priors='dbeta(1,1)', cov_as_cor=FALSE, populations_using=FALSE, agreement_check=FALSE, residual_check=FALSE, residual_min_obs=20L){

	stopifnot(is.data.frame(testdata))
	covon <- covariance

	## R code to generate a Hui-Walter model for N tests and P populations, with potential missing data

	# Function could be used as either just test names so generates code for no missing,
	# or data frame containing the data plus/minus some missing
	# If both given then test names tells us which columns to use, otherwise all columns not ID or Population
	# If a single population then do away with that loop

	# Note it is VERY important that the observations are missing at random

	# TODO:
	# allow 2-test (no covariance) and 1 population models
	# argument for test name (and population column) from data frame
	# option for no data frame, just create model for given N population/tests
	# testing

	if(is.null(testdata$Population)){
		testdata$Population <- factor(1)
	}
	if(!is.factor(testdata$Population)){
		testdata$Population <- factor(testdata$Population)
	}
	stopifnot(all(!is.na(testdata$Population)))
	npop <- length(levels(testdata$Population))

	## Initialise the file:
	cat('## Auto-generated Hui-Walter model created by runjags version ', runjagsprivate$runjagsversion, ' on ', as.character(Sys.Date()), '\n\nmodel{\n\n\t## Observation layer:', sep='', file=outfile, append=FALSE)
	catapp <- function(...){
	  cat(..., sep='', file=outfile, append=TRUE)
	}

	## Some variables that are needed in a few places:
	testcols <- names(testdata)[!names(testdata) %in% c('ID','Population')]
	stopifnot(length(testcols)>=2)

	if(length(se_priors)==1){
		se_priors <- rep(se_priors, length(testcols))
	}
	stopifnot(length(se_priors)==length(testcols))
	if(length(sp_priors)==1){
		sp_priors <- rep(sp_priors, length(testcols))
	}
	stopifnot(length(sp_priors)==length(testcols))

	# Make sure the tests are interpretable as logical and then convert to factor:
	for(col in testcols){
		if(is.logical(testdata[[col]])){
			testdata[[col]] <- factor(as.numeric(testdata[[col]]), levels=0:1)
		}
		if(is.numeric(testdata[[col]])){
			stopifnot(all(na.omit(testdata[[col]]) %in% c(0,1)))
			testdata[[col]] <- factor(as.numeric(testdata[[col]]), levels=0:1)
		}
		stopifnot(is.factor(testdata[[col]]))
		stopifnot(length(levels(testdata[[col]]))==2)
		levels(testdata[[col]]) <- 0:1
	}

	ntests <- length(testcols)
	ncomb <- 2^ntests
	testarr <- array(1:ncomb, dim=rep(2,length(testcols)), dimnames=lapply(testcols, paste0, c('-','+')))
	testcombs <- expand.grid(lapply(dimnames(testarr), paste0, ' '), stringsAsFactors=FALSE)

	args <- lapply(testcols, function(x) c(0,1))
	names(args) <- testcols
	outcomes <- do.call('expand.grid', args)

	testcombos <- unique(na.omit(t(apply(expand.grid(1:ntests, 1:ntests), 1, function(x) if(x[1]==x[2]) c(NA,NA) else sort(x)))))
	covcombs <- apply(testcombos,1,paste,collapse='')
	testagree <- apply(testcombos,1,function(x) outcomes[[x[1]]] == outcomes[[x[2]]])
	dimnames(testagree) <- list(NULL, paste0('cc', covcombs))

  datalist <- list(Populations=npop, PopulationsUsing = seq_len(npop), AcceptTest=rep(1,ntests), AcceptProb=matrix(1, nrow=ncomb, ncol=npop))
  if(populations_using) datalist[["PopulationsUsing"]] <- NULL
	datablock <- dump.format(datalist)

	nsum <- 0

	## temporary
	warning("REMOVEME")
	datablock <- c(datablock, dump.format(list(PopulationsUsing = seq_len(npop))))
	## /temporary

	## Helper function:
	writeobs <- function(tcode, ncomb, nobs=Inf){
	  catapp('for(p in PopulationsUsing){\n\t\tTally_', tcode, '[1:', ncomb, ',p] ~ dmulti(prob_', tcode, '[1:', ncomb, ',p], N_', tcode, '[p])\n\t')
	  if(residual_check && nobs >= residual_min_obs){
	    catapp('\n\tsim_tally_', tcode, '[1:', ncomb, ',p] ~ dmulti(prob_', tcode, '[1:', ncomb, ',p], N_', tcode, '[p])\n\t')
	    catapp('\tresidual_', tcode, '[1:', ncomb, ',p] <- Tally_', tcode, '[1:', ncomb, ',p] - sim_tally_', tcode, '[1:', ncomb, ',p]\n\t')
	  }
	  catapp('}\n\tfor(p in 1:Populations){\n\t\tprob_', tcode, '[1:', ncomb, ',p] <- (prev[p] * se_prob[1:', ncomb, ',p]) + ((1-prev[p]) * sp_prob[1:', ncomb, ',p])\n\t}')

	  # if(populations_using){
	  #   catapp('for(p in PopulationsUsing){\n\t\tTally_', tcode, '[1:', ncomb, ',p] ~ dmulti(prob_', tcode, '[1:', ncomb, ',p], N_', tcode, '[p])\n\t')
	  #   if(residual_check){
	  #     catapp('\tsim_tally_', tcode, '[1:', ncomb, ',p] ~ dmulti(prob_', tcode, '[1:', ncomb, ',p], N_', tcode, '[p])\n\t')
	  #     catapp('\tresidual_', tcode, '[1:', ncomb, ',p] <- Tally_', tcode, '[1:', ncomb, ',p] - sim_tally_', tcode, '[1:', ncomb, ',p]\n\t')
	  #   }
	  #   catapp('}\n\tfor(p in 1:Populations){\n\t\tprob_', tcode, '[1:', ncomb, ',p] <- (prev[p] * se_prob[1:', ncomb, ',p]) + ((1-prev[p]) * sp_prob[1:', ncomb, ',p])\n\t}')
	  # }else{
	  #   catapp('for(p in 1:Populations){\n\t\tTally_', tcode, '[1:', ncomb, ',p] ~ dmulti(prob_', tcode, '[1:', ncomb, ',p], N_', tcode, '[p])\n\t')
	  #   if(residual_check){
	  #     catapp('\tsim_tally_', tcode, '[1:', ncomb, ',p] ~ dmulti(prob_', tcode, '[1:', ncomb, ',p], N_', tcode, '[p])\n\t')
	  #     catapp('\tresidual_', tcode, '[1:', ncomb, ',p] <- Tally_', tcode, '[1:', ncomb, ',p] - sim_tally_', tcode, '[1:', ncomb, ',p]\n\t')
	  #   }
	  #   catapp('\tprob_', tcode, '[1:', ncomb, ',p] <- (prev[p] * se_prob[1:', ncomb, ',p]) + ((1-prev[p]) * sp_prob[1:', ncomb, ',p])\n\t}')
	  # }

	}

	## Complete observations (if there are any):
	if(nrow(na.omit(testdata[,testcols])) > 0){
		tcode <- paste0(rep('R',ntests), collapse='')
		tdata <- na.omit(testdata[,c(testcols,'Population')])
		tabdata <- vapply(seq_len(npop), function(x) as.numeric(do.call(`[`, c(list(table(tdata)), lapply(1:length(testcols), function(y) 1:2), list(x)))), numeric(2^length(testcols)))
		stopifnot(all(dim(tabdata)==c(ncomb,npop)))
		dlist <- list(table(tdata$Population), tabdata)
		names(dlist) <- c(paste0('N_', tcode), paste0('Tally_', tcode))
		datablock <- c(datablock, dump.format(dlist))
		nsum <- nsum+sum(tabdata)


		catapp('\n\n\t# Complete observations (N=', sum(tabdata), '):\n\t')
		writeobs(tcode, ncomb)
	}

	## Partially missing observations (whatever combinations):
	presencecombos <- unique(!is.na(testdata[,testcols]))
	# Remove the complete set and completely missing (if present):
	presencecombos <- presencecombos[! apply(presencecombos,1,sum) %in% c(0,ntests), , drop=FALSE]

	for(pc in seq_len(nrow(presencecombos))){
		tcode <- paste0(c('M','R')[presencecombos[pc,]+1], collapse='')
		tcomb <- 2^sum(presencecombos[pc,])

		tdata <- testdata[,c(testcols,'Population')]
		tdata <- tdata[apply(!is.na(tdata[,1:length(testcols)]),1,function(x) all(x==presencecombos[pc,])), c(presencecombos[pc,],TRUE)]
		tabdata <- vapply(seq_len(npop), function(x) as.numeric(do.call(`[`, c(list(table(tdata)), lapply(1:sum(presencecombos[pc,]), function(y) 1:2), list(x)))), numeric(2^sum(presencecombos[pc,])))
		stopifnot(all(dim(tabdata)==c((2^sum(presencecombos[pc,])), npop)))
		dlist <- list(table(tdata$Population), tabdata)
		names(dlist) <- c(paste0('N_', tcode), paste0('Tally_', tcode))
		datablock <- c(datablock, dump.format(dlist))
		nsum <- nsum+sum(tabdata)

		ardims <- do.call(expand.grid, c(lapply(presencecombos[pc,], function(x) if(x) 0 else 1:2), list(stringsAsFactors=FALSE)))
		indexes <- vapply(seq_len(nrow(ardims)), function(i){
		  do.call(`[`, c(list(testarr), lapply(ardims[i,], function(x) if(x==0) 1:2 else x)))
		}, numeric(2^sum(presencecombos[pc,])))
		stopifnot(length(indexes)==ncomb && all(testarr %in% indexes))
		arrin <- paste0('[c(',apply(indexes,2,paste,collapse=','),'),p]')

		catapp('\n\n\t# Partial observations (', paste0(testcols, ': ', c('Missing','Recorded')[presencecombos[pc,]+1], collapse=', '), '; N=', sum(tabdata),'):\n\t')
		writeobs(tcode, tcomb, sum(tabdata))
	}

	nsum <- nsum + sum(apply(is.na(testdata[,testcols]),1,all))
	stopifnot(nsum == nrow(testdata))

	## Main probability calculations based on the total number of tests:

	cat('\n\n\n\t## Observation probabilities:\n\n\tfor(p in 1:Populations){\n\n', sep='', file=outfile, append=TRUE)

	pasteargs <- c(list('\t\t# Probability of observing '), as.list(testcombs), 'from a true positive:')
	pasteargs <- c(pasteargs, list('\n\t\tse_prob[', 1:ncomb, ',p] <- '))
	pasteargs <- c(pasteargs, list(apply(outcomes, 1, function(x){
		text <- ifelse(x==1, paste0('se[', 1:ncomb, ']'), paste0('(1-se[', 1:ncomb, '])'))
		return(paste(text, collapse='*'))
	})))
	pasteargs <- c(pasteargs, lapply(covcombs, function(x) ifelse(testagree[,paste0('cc',x)], paste0(' +covse', x), paste0(' -covse', x))))

	pasteargs <- c(pasteargs, list('\n\t\t# Probability of observing '), as.list(testcombs), 'from a true negative:')
	pasteargs <- c(pasteargs, list('\n\t\tsp_prob[', 1:ncomb, ',p] <- '))
	pasteargs <- c(pasteargs, list(apply(outcomes, 1, function(x){
		text <- ifelse(x==1, paste0('(1-sp[', 1:ncomb, '])'), paste0('sp[', 1:ncomb, ']'))
		return(paste(text, collapse='*'))
	})))
	pasteargs <- c(pasteargs, lapply(covcombs, function(x) ifelse(testagree[,paste0('cc',x)], paste0(' +covsp', x), paste0(' -covsp', x))))

	#pasteargs <- c(pasteargs, list('\n\t\tconstraint_ok[', 1:ncomb, ',p] <- ifelse(se_prob[', 1:ncomb, ',p] >= 0 && se_prob[', 1:ncomb, ',p] <= 1 && sp_prob[', 1:ncomb, ',p] >= 0 && sp_prob[', 1:ncomb, ',p], 1, 0)'))
	#pasteargs <- c(pasteargs, list('\n\t\tAccept[', 1:ncomb, ',p] ~ dbern(constraint_ok[',1:ncomb,',p])'))
	# pasteargs <- c(pasteargs, list('\n\n\t\t# Ensure that the covariance terms do not result in an invalid probability:\n\t\tAcceptProb[', 1:ncomb, ',p] ~ dbern(constraint_ok[ifelse(se_prob[', 1:ncomb, ',p] >= 0 && se_prob[', 1:ncomb, ',p] <= 1 && sp_prob[', 1:ncomb, ',p] >= 0 && sp_prob[', 1:ncomb, ',p], 1, 0))'))

	cat(do.call('paste', c(pasteargs, list(sep=''))), sep='\n\n', file=outfile, append=TRUE)

	catapp('\n\n\t\t# Ensure that the covariance terms do not result in an invalid probability:\n\t\tfor(c in 1:', ncomb, '){\n\t\t\tAcceptProb[c,p] ~ dbern(ifelse(se_prob[c,p] >= 0 && se_prob[c,p] <= 1 && sp_prob[c,p] >= 0 && sp_prob[c,p], 1, 0))\n\t\t}\n')

	cat('\n\t}\n', sep='', file=outfile, append=TRUE)


	## Priors:

	cat('\n\n\t## Priors:\n', sep='', file=outfile, append=TRUE)

	for(p in seq_len(length(levels(testdata$Population)))){
		cat('\n\t# Prevalence in population ', levels(testdata$Population)[p], ':\n\tprev[', p, '] ~ dbeta(1,1)\n', sep='', file=outfile, append=TRUE)
	}

	cat('\n',file=outfile, append=TRUE)

	for(t in 1:length(testcols)){
		cat('\n\t# Sensitivity of ', testcols[t], ' test:\n\tse[', t, '] ~ ', se_priors[t], '\n', sep='', file=outfile, append=TRUE)
		cat('\t# Specificity of ', testcols[t], ' test:\n\tsp[', t, '] ~ ', sp_priors[t], '\n', sep='', file=outfile, append=TRUE)
	}
	catapp('\n\t# Ensure that label switching does not occur for any test:\n\tfor(t in 1:', length(testcols), '){\n\t\tAcceptTest[t] ~ dbern(ifelse((se[t]+sp[t]) >= 1.0, 1, 0))\n\t}')

	cat('\n',file=outfile, append=TRUE)

	for(t in 1:nrow(testcombos)){
	  if(cov_as_cor){
	    i1 <- testcombos[t,1]
	    i2 <- testcombos[t,2]
	    cat('\n\t# Correlation in sensitivity between ', testcols[i1], ' and ', testcols[i2], ' tests:\n\t', if(!covon) '# ', 'corse', paste(c(i1,i2), collapse=''), ' ~ dunif(-1, 1)  ## if the sensitivity of these tests may be correlated\n\t', if(covon) '# ', 'corse', paste(c(i1,i2), collapse=''), ' <- 0 ## if the sensitivity of these tests can be assumed to be independent\n\t', '# Corresponding covariance calculation:\n\t', 'covse', paste(c(i1,i2), collapse=''), ' <- ifelse(corse', paste(c(i1,i2), collapse=''), ' < 0, -corse', paste(c(i1,i2), collapse=''), ' * ((se[',i1,']-1)*(1-se[',i2,'])), corse', paste(c(i1,i2), collapse=''), ' * (min(se[',i1,'],se[',i2,']) - se[',i1,']*se[',i2,']))\n\t', sep='', file=outfile, append=TRUE)
	    cat('\n\t# Correlation in specificity between ', testcols[i1], ' and ', testcols[i2], ' tests:\n\t', if(!covon) '# ', 'corsp', paste(c(i1,i2), collapse=''), ' ~ dunif(-1, 1)  ## if the specificity of these tests may be correlated\n\t', if(covon) '# ', 'corsp', paste(c(i1,i2), collapse=''), ' <- 0  ## if the specificity of these tests can be assumed to be independent\n\t', '# Corresponding covariance calculation:\n\t', 'covsp', paste(c(i1,i2), collapse=''), ' <- ifelse(corsp', paste(c(i1,i2), collapse=''), ' < 0, -corsp', paste(c(i1,i2), collapse=''), ' * ((sp[',i1,']-1)*(1-sp[',i2,'])), corsp', paste(c(i1,i2), collapse=''), ' * (min(sp[',i1,'],sp[',i2,']) - sp[',i1,']*sp[',i2,']))\n\t', sep='', file=outfile, append=TRUE)
	  }else{
	    i1 <- testcombos[t,1]
	    i2 <- testcombos[t,2]

	    cat('\n\t# Covariance in sensitivity between ', testcols[i1], ' and ', testcols[i2], ' tests:\n\t', if(!covon) '# ', 'covse', paste(c(i1,i2), collapse=''), ' ~ dunif( (se[',i1,']-1)*(1-se[',i2,']) , min(se[',i1,'],se[',i2,']) - se[',i1,']*se[',i2,'] )  ## if the sensitivity of these tests may be correlated\n\t', if(covon) '# ', 'covse', paste(c(i1,i2), collapse=''), ' <- 0  ## if the sensitivity of these tests can be assumed to be independent\n', sep='', file=outfile, append=TRUE)
	    cat('\t# Calculated relative to the min/max for ease of interpretation:\n\t', 'corse', paste(c(i1,i2), collapse=''), ' <- ifelse(covse', paste(c(i1,i2), collapse=''), ' < 0, -covse', paste(c(i1,i2), collapse=''), ' / ((se[',i1,']-1)*(1-se[',i2,'])), covse', paste(c(i1,i2), collapse=''), ' / (min(se[',i1,'],se[',i2,']) - se[',i1,']*se[',i2,']))\n', sep='', file=outfile, append=TRUE)

	    cat('\n\t# Covariance in specificity between ', testcols[i1], ' and ', testcols[i2], ' tests:\n\t', if(!covon) '# ', 'covsp', paste(c(i1,i2), collapse=''), ' ~ dunif( (sp[',i1,']-1)*(1-sp[',i2,']) , min(sp[',i1,'],sp[',i2,']) - sp[',i1,']*sp[',i2,'] )  ## if the specificity of these tests may be correlated\n\t', if(covon) '# ', 'covsp', paste(c(i1,i2), collapse=''), ' <- 0  ## if the specificity of these tests can be assumed to be independent\n', sep='', file=outfile, append=TRUE)
	    cat('\t# Calculated relative to the min/max for ease of interpretation:\n\t', 'corsp', paste(c(i1,i2), collapse=''), ' <- ifelse(covsp', paste(c(i1,i2), collapse=''), ' < 0, -covsp', paste(c(i1,i2), collapse=''), ' / ((sp[',i1,']-1)*(1-sp[',i2,'])), covsp', paste(c(i1,i2), collapse=''), ' / (min(sp[',i1,'],sp[',i2,']) - sp[',i1,']*sp[',i2,']))\n', sep='', file=outfile, append=TRUE)

	  }
	}

	cat('\n}\n', sep='', file=outfile, append=TRUE)

	## Monitors:
	cat('\n#monitor# se, sp, prev', apply(expand.grid(c('covse','corse','covsp','corsp'), apply(testcombos,1,paste,collapse='')),1,paste,collapse=''), sep=', ', file=outfile, append=TRUE)

	## Initial values:
	alternate <- function(x,len){
		x <- rep(x,times=ceiling(len/length(x)))
		return(x[1:len])
	}
	if(cov_as_cor){
	  cvn <- apply(expand.grid(apply(testcombos,1,paste,collapse=''), c('corse','corsp'))[,2:1],1,paste,collapse='')
	}else{
	  cvn <- apply(expand.grid(apply(testcombos,1,paste,collapse=''), c('covse','covsp'))[,2:1],1,paste,collapse='')
	}
	# Fails to initialise with anything other than 0:
	covinitvals <- as.list(alternate(c(0,0), length(cvn)))
	names(covinitvals) <- cvn
	covinits <- c(dump.format(covinitvals), dump.format(lapply(covinitvals, function(x) -x)))
	if(!covon){
	  covinits <- gsub('\"cov', '# \"cov', covinits, fixed=TRUE)
	  covinits <- gsub('\"cor', '# \"cor', covinits, fixed=TRUE)
	}

	initblock <- c(dump.format(list(se=alternate(c(0.5,0.99), length(testcols)), sp=alternate(c(0.99,0.75), length(testcols)), prev=alternate(c(0.05,0.95), length(levels(testdata$Population))))), covinits[1])
	cat('\n\n## Inits:\ninits{\n', initblock, '}', sep='', file=outfile, append=TRUE)
	initblock <- c(dump.format(list(se=alternate(c(0.5,0.99)[2:1], length(testcols)), sp=alternate(c(0.99,0.75)[2:1], length(testcols)), prev=alternate(c(0.05,0.95)[2:1], length(levels(testdata$Population))))), covinits[2])
	cat('\ninits{\n', initblock, '}', sep='', file=outfile, append=TRUE)

	## Data:

	cat('\n\n## Data:\ndata{\n', datablock, '}\n\n', sep='', file=outfile, append=TRUE)

	cat('The model and data have been written to', outfile, 'in the current working directory\n')
	if(populations_using) cat('You will need to create a numeric vector of populations to include in the model within your R working environment, e.g.:\n\tPopulationsUsing <- seq_len(', length(testcols), ')\n')
	cat('You should check and alter priors before running the model\n')

}


