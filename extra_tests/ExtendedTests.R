# source("C:/Users/Administrator/Desktop/runjags-tests/ExtendedTests.R")

on.exit(system('rm -r /Users/matthewdenwood/Documents/Code/R/myrpackages/support/runjags-tests/testarea/*'))

require('lme4')
require('parallel')
require('rjags')
require('modeest')

# Clear working env:
rm(list=ls(all.names=TRUE))
# USE PACKAGE TESTS FROM COMMAND LINE!!!!

options(warn=0)
try(unloadNamespace('runjags'))
assign('.runjags.options', list(inits.warning=FALSE, rng.warning=FALSE, summary.warning=FALSE, nodata.warning=FALSE, blockcombine.warning=FALSE, debug=0, new.windows=FALSE), envir=.GlobalEnv)
suppressWarnings(library('runjags'))

windows <- .Platform$OS.type=='windows'

if(windows){
  testarea <- "C:/Users/Administrator/Desktop/runjags-tests/testarea"
  testfolder <- "C:/Users/Administrator/Desktop/runjags-tests/runjags/tests"
  examplearea <- "C:/Users/Administrator/Desktop/runjags-tests/runjags"
  dostandardchecks <- TRUE
  makeForkCluster <- parallel::makeCluster
}else{
  testarea <- '/Users/matthewdenwood/Documents/Code/R/myrpackages/support/runjags-tests/testarea'
  testfolder <- '/Users/matthewdenwood/Documents/GitHub/runjags/tests'
  examplearea <- '/Users/matthewdenwood/Documents/GitHub/runjags'
  dostandardchecks <- FALSE
}

if(!file.exists(testarea))
	dir.create(testarea)

# A function for displaying output prettily:
fb <- function(...){
  x <- paste(list(...),collapse='')
  msg <- gsub('\n','',x)
  astxs <- '*****'
  spaces <- '   '
  tb <- gsub('.','*',paste(astxs, msg, astxs, sep=spaces))
  cat(paste(astxs, msg, astxs, sep=spaces), '\n', sep='')
  invisible(TRUE)
}
fbb <- function(...){
  x <- paste(list(...),collapse='')
  msg <- gsub('\n','',x)
  astxs <- '*****'
  spaces <- '   '
  tb <- gsub('.','*',paste(astxs, msg, astxs, sep=spaces))
  cat('\n', tb,'\n',sep='')
  cat(paste(astxs, msg, astxs, sep=spaces), '\n', sep='')
  cat(tb,'\n',sep='')
  invisible(TRUE)
}


# these tests are done on the command line:
if(FALSE){
  options(warn=0)  # makes warnings non-fatal

  s <- try(devtools::install('/Users/matthewdenwood/Documents/Code/R/myrpackages/runjags'))
  if(inherits(s, 'try-error'))
    stop(paste('Install failed: ', s, sep='\n'))


  setwd(testarea)
  unloadNamespace('runjags')

  print(warnings())

  library('runjags')

  # These won't run on Windows - it tries to reinstall:
	fbb('Examples...\n')
	s <- try({
	devtools::run_examples(examplearea, show=FALSE, test=FALSE, run=FALSE, fresh=FALSE)
	})
	if(inherits(s, 'try-error'))
	stop(paste('Error in examples: ', s, sep='\n'))
	graphics.off()

	fbb('Examples warnings:')
	print(warnings())
	fbb('Done examples')

	Sys.sleep(5)
	unloadNamespace('runjags')

	suppressWarnings(library('runjags'))

}


options(warn=2)  # makes warnings fatal

if(dostandardchecks){

	setwd(testfolder)
	rjo <- .runjags.options
	fbb('Check: inputs...\n')
	s <- try({
	  source('checkinputs.R')
	})
	.runjags.options <- rjo
	if(inherits(s, 'try-error'))
	  stop(paste('Error in checkinputs: ', s, sep='\n'))

	fbb('Check: module...\n')
	s <- try({
	  source('checkmodule.R')
	})
	if(inherits(s, 'try-error'))
	  stop(paste('Error in checkmodule: ', s, sep='\n'))

	fbb('Check: methods...\n')
	s <- try({
	  source('checkmethods.R')
	})
	if(inherits(s, 'try-error'))
	  stop(paste('Error in checkmethods: ', s, sep='\n'))

	fbb('Check: study...\n')
	s <- try({
	  source('checkstudy.R')
	})
	if(inherits(s, 'try-error'))
	  stop(paste('Error in checkmethods: ', s, sep='\n'))

	graphics.off()

}

unloadNamespace('runjags')
suppressWarnings(library('runjags'))

setwd(testarea)
s <- try({
  fbb('Extra: utilities...\n')
  source('../utilitiestests.R')
})
if(inherits(s, 'try-error'))
  stop(paste('Error in utilities tests: ', s, sep='\n'))

s <- try({
  fbb('Extra: check summaries...\n')
  source('../checksummaries.R')
})
if(inherits(s, 'try-error'))
  stop(paste('Error in miscelanious tests: ', s, sep='\n'))

s <- try({
  fbb('Extra: miscelanious...\n')
  source('../miscelanious.R')
})
if(inherits(s, 'try-error'))
  stop(paste('Error in miscelanious tests: ', s, sep='\n'))

s <- try({
  fbb('Extra: run.jags...\n')
  source('../run.jagstests.R')
})
if(inherits(s, 'try-error'))
  stop(paste('Error in runjags tests: ', s, sep='\n'))

s <- try({
  fbb('Extra: study...\n')
  source('../studytests.R')
})
if(inherits(s, 'try-error'))
  stop(paste('Error in run.jags.study tests: ', s, sep='\n'))

s <- try({
  fbb('Extra: glm...\n')
  source('../glmtests.R')
})
if(inherits(s, 'try-error'))
  stop(paste('Error in glm tests: ', s, sep='\n'))

s <- try({
  fbb('Extra: 8schools...\n')
  source('../8schools.R')
})
if(inherits(s, 'try-error'))
  stop(paste('Error in 8schools tests: ', s, sep='\n'))

if(!windows){
s <- try({
  fbb('Paper replication...\n')
  source('../replicationcode.R')
})
if(inherits(s, 'try-error'))
  stop(paste('Error in replication code tests: ', s, sep='\n'))
}
# To get rid of new settings:
unloadNamespace('runjags')
suppressWarnings(library('runjags'))

s <- try({
  fbb('Extra: time consuming...\n')
  source('../timeconsuming.R')
})
if(inherits(s, 'try-error'))
  stop(paste('Error in time consuming tests: ', s, sep='\n'))

unlink(testarea, recursive=TRUE)

graphics.off()

