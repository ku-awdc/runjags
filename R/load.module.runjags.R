#' Load the internal JAGS module provided by runjags
#'
#' @name load.runjagsmodule
#' @aliases load.runjagsmodule load.runJAGSmodule unload.runjagsmodule unload.runJAGSmodule
#'
#' @description
#' The runjags package contains a JAGS extension module that provides several additional distributions for use within JAGS (see details below).  This function is a simple wrapper to load this module.  The version of the module supplied within the runjags package can only be used with the rjags package, or with the rjags or rjparallel methods within runjags.  For a standalone JAGS module for use with any JAGS method (or independent JAGS runs) please see:  https://sourceforge.net/projects/runjags/files/paretoprior/
#'
#' @details
#' This module provides the following distributions for JAGS:
#'
#' PARETO TYPE I:  dpar1(alpha, sigma)
#' \deqn{
#'   p(x) = \alpha \sigma^{\alpha} x^{-\left(\alpha+1 \right)}
#' }{
#'   p(x) = \alpha \sigma^\alpha x^-(\alpha+1)
#' }
#'
#' \deqn{\alpha > 0, \sigma > 0, x > \sigma}{\alpha > 0, \sigma > 0, x > \sigma}
#'
#' PARETO TYPE II:  dpar2(alpha, sigma, mu)
#'
#' \deqn{
#'   p(x) = \frac{\alpha}{\sigma} \left( \frac{\alpha + x - \mu}{\sigma}\right)^{-\left(\alpha+1\right)}
#' }{
#'   p(x) = (\alpha / \sigma) ((\alpha + x - \mu) / (\sigma)) ^ -(\alpha+1)
#' }
#'
#' \deqn{\alpha > 0, \sigma > 0, x > \mu}{\alpha > 0, \sigma > 0, x > \mu}
#'
#'
#' PARETO TYPE III:  dpar3(sigma, mu, gamma)
#'
#' \deqn{
#'   p(x) = \frac{\frac{x-\mu}{\sigma}^{\frac{1}{\gamma}-1} \left(\frac{x-\mu}{\sigma}^{\frac{1}{\gamma}} +1\right)^{-2}}{\gamma \sigma}
#' }{
#'   p(x) = (((x-\mu)/\sigma)^(1/\gamma -1) (((x-\mu)/\sigma)^(1/\gamma) +1)^-2) / (\gamma \sigma)
#' }
#'
#' \deqn{\sigma > 0, \gamma > 0, x > \mu}{\sigma > 0, \gamma > 0, x > \mu}
#'
#'
#' PARETO TYPE IV:  dpar4(alpha, sigma, mu, gamma)
#'
#' \deqn{
#'   p(x) = \frac{\alpha \frac{x-\mu}{\sigma}^{\frac{1}{\gamma}-1} \left(\frac{x-\mu}{\sigma}^{\frac{1}{\gamma}} +1\right)^{-\left(\alpha+1\right)}}{\gamma \sigma}
#' }{
#'   p(x) = (\alpha((x-\mu)/\sigma)^(1/\gamma -1) (((x-\mu)/\sigma)^(1/\gamma) +1)^-(\alpha+1)) / (\gamma \sigma)
#' }
#'
#' \deqn{\alpha > 0, \sigma > 0, \gamma > 0, x > \mu}{\alpha > 0, \sigma > 0, \gamma > 0, x > \mu}
#'
#'
#' LOMAX:  dlomax(alpha, sigma)
#'
#' \deqn{
#'   p(x) = \frac{\alpha}{\sigma} \left(1 + \frac{x}{\sigma}\right)^{-\left(\alpha+1\right)}
#' }{
#'   p(x) = (\alpha / \sigma) (1 + (x / \sigma)) ^ -(\alpha+1)
#' }
#'
#' \deqn{\alpha > 0, \sigma > 0, x > 0}{\alpha > 0, \sigma > 0, x > 0}
#'
#'
#' GENERALISED PARETO:  dgenpar(sigma, mu, xi)
#'
#' \deqn{
#'   p(x) = \frac{1}{\sigma} \left(1 + \xi \left(\frac{x-\mu}{\sigma}\right)\right)^{-\left(\frac{1}{\xi}+1\right)}
#' }{
#'   p(x) = (1 / \sigma) (1 + \xi ((x-\mu) /  sigma))^-(1/\xi + 1)
#' }
#'
#' For \eqn{\xi=0}{\xi=0}:
#'
#'   \deqn{
#'     p(x) = \frac{1}{\sigma} e^{\frac{-\left(x-\mu\right)}{\sigma}}
#'   }{
#'     p(x) = (1 / \sigma) e^(-(x-\mu)/\sigma)
#'   }
#'
#' \deqn{\sigma > 0, x > \mu}{\sigma > 0, x > \mu}
#'
#'
#' DUMOUCHEL:  dmouch(sigma)
#'
#' \deqn{
#'   p(x) = \frac{\sigma}{\left(x+\sigma\right)^2}
#' }{
#'   p(x) = \sigma / ((x + \sigma)^2)
#' }
#'
#' \deqn{\sigma > 0, x > 0}{\sigma > 0, x > 0}
#'
#'
#' HALF CAUCHY:  dhalfcauchy(sigma)
#'
#' \deqn{
#'   p(x) = \frac{2 \sigma}{\pi \left(x^2+\sigma^2\right)}
#' }{
#'   p(x) = 2 \sigma / (\pi (x^2 + \sigma^2))
#' }
#'
#' \deqn{\sigma > 0, x > 0}{\sigma > 0, x > 0}
#'
#' For an easier to read version of these PDF equations, see the userguide vignette.
#'
#' @param fail should the function fail using stop() if the module cannot be loaded?
#' @param silent if !fail, the function will by default print a diagnostic message if the module cannot be loaded - the silent option suppresses this message.
#'
#' @return Invisibly returns TRUE if able to (un)load the module, or FALSE otherwise
#' @keywords methods
#' @seealso
#' \code{\link{runjags-class}}, \code{\link[rjags]{load.module}}
#'
#' @references
#' Denwood, M.J. 2016. runjags: An R Package Providing Interface Utilities, Model Templates, Parallel Computing Methods and Additional Distributions for MCMC Models in JAGS. J. Stat. Softw. 71. doi:10.18637/jss.v071.i09.
#'
#' @examples
#' # Load the module for use with any rjags model:
#' available <- load.runjagsmodule(fail=FALSE)
#' if(available){
#' # A simple model to sample from a Lomax distribution.
#' # (Requires the rjags or rjparallel methods)
#' m <- "model{
#'  L ~ dlomax(1,1)
#' 		}"
#' results <- run.jags(m, monitor="L", sample=1000, method="rjags", modules="runjags")
#' }

#' @rdname load.runjagsmodule
#' @export
load.runjagsmodule <- function(fail=TRUE, silent=FALSE){

	intfun <- function(){
		# Make sure this is not the module-less version from sourceforge:
		if(runjagsprivate$modulelocation=='')
			return('The internal runjags module is not installed - please reinstall the full version of the package from CRAN, or alternatively you can download a standalone version of the JAGS module from the sourceforge page at http://sourceforge.net/projects/runjags/')

		# Also makes sure JAGS is installed:
		if(!loadandcheckrjags(FALSE))
			return("The rjags package is required to use the internal runjags module - alternatively you can download a standalone version of the JAGS module from the sourceforge page at http://sourceforge.net/projects/runjags/")

		# Check the JAGS major version is as expected:
		if(packageVersion('rjags')$major < runjagsprivate$minjagsmajor)
			return(paste('JAGS version ', runjagsprivate$minjagsmajor, '.x.x to ', runjagsprivate$maxjagsmajor, '.x.x is required for this version of the runjags module - please update JAGS and rjags',sep=''))
		if(packageVersion('rjags')$major > runjagsprivate$maxjagsmajor)
			return(paste('This version of the runjags module was designed for JAGS version ', runjagsprivate$minjagsmajor, '.x.x to ', runjagsprivate$maxjagsmajor, '.x.x - please update the runjags package', sep=''))

		success <- try(rjags::load.module('runjags',runjagsprivate$modulelocation))

		if(inherits(success, 'try-error')){

			rvers <- paste('version ', R.version$major, sep='')
			if(grepl('mac.binary', .Platform$pkgType, fixed=TRUE)){
				# A specific error may be because of SL vs Mavericks version on OS X for JAGS version 3.4:
				mavericks <- grepl('mavericks', .Platform$pkgType)
				if(mavericks)
					rvers <- paste(rvers, ' - Mavericks', sep='')
				else
					rvers <- paste(rvers, ' - Snow Leopard', sep='')
			}

			return(paste("The internal runjags module could not be loaded - perhaps the package was not built using the same versions of R [", rvers, "] and JAGS [version ", testjags(silent=TRUE)$JAGS.version, "] as available on this system?", sep=''))

		}
		return(TRUE)
	}

	retval <- intfun()

	if(retval==TRUE){
		invisible(TRUE)
	}else{
		if(fail)
			stop(retval)

		if(!silent)
			swcat(retval,'\n',sep='')

		invisible(FALSE)
	}
}

#' @rdname load.runjagsmodule
#' @export
unload.runjagsmodule <- function(){

	if(!loadandcheckrjags(FALSE))
		stop("The rjags package is required to use the internal runjags module - alternatively you can download a standalone version of the JAGS module from the sourceforge page at http://sourceforge.net/projects/runjags/")

	suppressWarnings(success <- try(rjags::unload.module('runjags')))

	if(inherits(success, 'try-error')){
		warning("There was a problem unloading the internal runjags module - if you installed this package from CRAN, please file a bug report to the package author")
		invisible(FALSE)
	}else{
		invisible(TRUE)
	}
}

#' @rdname load.runjagsmodule
#' @export
load.runJAGSmodule <- load.runjagsmodule

#' @rdname load.runjagsmodule
#' @export
unload.runJAGSmodule <- unload.runjagsmodule


# These utility functions are NOT exported, and are primarily used for unit testing.
# Availability and/or operation of these functions may change without warning.

dynloadmodule <- function(){

	# Sets environmental variables we need for Windows:
	if(.Platform$OS.type=='windows'){
		if(!loadandcheckrjags(FALSE))
			stop('The rjags package is required to load the internal dynlib')
	}

	if(runjagsprivate$modulelocation==''){
		warning('The runjags module has not been installed with this version of the package - try again using the CRAN binary')
		invisible(FALSE)
	}

	# Check the JAGS major version is as expected:
	if(testjags(silent=TRUE)$JAGS.major < runjagsprivate$minjagsmajor)
		return(paste('JAGS version ', runjagsprivate$minjagsmajor, '.x.x to ', runjagsprivate$maxjagsmajor, '.x.x is required for this version of the runjags module - please update JAGS and rjags',sep=''))
	if(testjags(silent=TRUE)$JAGS.major > runjagsprivate$maxjagsmajor)
		return(paste('This version of the runjags module was designed for JAGS version ', runjagsprivate$minjagsmajor, '.x.x to ', runjagsprivate$maxjagsmajor, '.x.x - please update the runjags package', sep=''))

	# Find and load the runjags shared library (only required for these tests and using the rjags call 'load.modue()' so NOT loaded at runtime):
	slibpath <- file.path(runjagsprivate$modulelocation, paste('runjags', .Platform$dynlib.ext, sep=''))
	swcat("Loading shared library from:  ", slibpath, "\n", sep="")
	success <- try(dyn.load(slibpath))

	if(inherits(success, 'try-error')){

		rvers <- paste('version ', R.version$major, sep='')
		if(grepl('mac.binary', .Platform$pkgType, fixed=TRUE)){
			# A specific error may be because of SL vs Mavericks version on OS X for JAGS version 3.4:
			mavericks <- grepl('mavericks', .Platform$pkgType)
			if(mavericks)
				rvers <- paste(rvers, ' - Mavericks', sep='')
			else
				rvers <- paste(rvers, ' - Snow Leopard', sep='')
		}

		return(paste("The runjags dynlib could not be loaded - perhaps the package was not built using the same versions of R [", rvers, "] and JAGS [version ", testjags(silent=TRUE)$JAGS.version, "] as available on this system?", sep=''))

	}

	runjagsprivate$dynlibname <- success
	invisible(TRUE)

}

dynunloadmodule <- function(){

	if(is.null(runjagsprivate$dynlibname)){
		warning('Unable to load the dynlib as it has not been loaded')
		invisible(FALSE)
	}
	# Find and unload the runjags shared library (only required for these tests and using the rjags call 'load.modue()' so NOT loaded at runtime):
	slibpath <- system.file("libs", paste(.Platform$r_arch, if(.Platform$r_arch!="") "/" else "", if(.Platform$OS.type=="unix") "runjags.so" else "runjags.dll", sep=""), package="runjags")
	swcat("Unloading shared library from:  ", slibpath, "\n", sep="")
	success <- try(dyn.unload(slibpath))
	if(inherits(success, 'try-error'))
		warning("The internal dynlib could not be unloaded")

	runjagsprivate$dynlibname <- NULL
	invisible(TRUE)
}

userunjagsmodule <- function(distribution, funtype, parameters, x, uselog=FALSE, lower=TRUE){

	if(is.null(runjagsprivate$dynlibname)){
		stopifnot(dynloadmodule())
	}

	if(!is.character(distribution)) stop("The distribution type must be one of par1, par2, par3, par4, lomax, mouch, genpar or halfcauchy")
	disttype <- switch(distribution, par1=1, par2=2, par3=3, par4=4, lomax=5, mouch=6, genpar=7, hcauchy=8, halfcauchy=8, 0)
	if(disttype==0) stop("The distribution type must be one of par1, par2, par3, par4, lomax, mouch, genpar or halfcauchy")

	if(!is.character(funtype)) stop("The function type must be one of d, p and q")
	dpqr <- switch(funtype, d=1, p=2, q=3, r=4, 0)
	if(dpqr==0) stop("The function type must be one of d, p and q")
	if(dpqr==4) stop("The function type must be one of d, p and q - r is not available")

	npars <- length(parameters)
	N <- length(x)
	values <- numeric(N)

	# Having problems with bools on sparc, so use ints here:
	output <- .C("testrunjags",disttype=as.integer(disttype),dpqr=as.integer(dpqr),uselog=as.integer(uselog),lower=as.integer(lower),N=as.integer(N), x=as.double(x),npars=as.integer(npars),parameters=as.double(parameters),values=as.double(values),status=integer(1),PACKAGE='runjags')

	if(output$status==1) stop("Incorrect number of parameters provided")
	if(output$status==2) stop("Unrecognised distribution type")
	if(output$status==3) stop("Invalid parameter values provided")
	if(output$status==4) stop("Function type not valid")
	if(output$status==5) stop("Function type not valid")

	return(output$values)

}
