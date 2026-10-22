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
#' # A simple model to sample from a Lomax distribution.
#' # (Requires the rjags or rjparallel methods)
#' m <- "model{
#'  L ~ dlomax(1,1)
#' 		}"
#' results <- run.jags(m, monitor="L", method="rjags")

#' @rdname load.runjagsmodule
#' @export
load.runjagsmodule <- function(fail=TRUE, silent=FALSE){
  load.pkg.module(.jags_pkg_env$pkgname, .jags_pkg_env$libname, .jags_pkg_env$pkg_compile_flags)
}

#' @rdname load.runjagsmodule
#' @export
unload.runjagsmodule <- function(){
  unload.module(.jags_pkg_env$pkgname)
}

#' @rdname load.runjagsmodule
#' @export
load.runJAGSmodule <- load.runjagsmodule

#' @rdname load.runjagsmodule
#' @export
unload.runJAGSmodule <- unload.runjagsmodule


userunjagsmodule <- function(distribution, funtype, parameters, x, uselog=FALSE, lower=TRUE){

  if(!.jags_pkg_env$dyn_loaded){
    stop("Unable to call userunjagsmodule as the package dynlib is not loaded")
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
