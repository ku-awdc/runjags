#' @keywords internal
"_PACKAGE"

#' @name runjags
#' @aliases runjags runjags-package runjagspackage
#' @title Interface Utilities, Model Templates, Parallel Computing Methods and Additional Distributions for MCMC Models in JAGS
#' @description
#' User-friendly interface utilities for MCMC models via Just Another Gibbs Sampler, facilitating the use of parallel (or distributed) processors for multiple chains, automated control of convergence and sample length diagnostics, and evaluation of the performance of a model using drop-k validation or against simulated data. Template model specifications can be generated using a standard lme4-style formula interface to assist users less familiar with the BUGS syntax.  A JAGS extension module provides additional distributions including the Pareto family of distributions, the DuMouchel prior and the half-Cauchy prior.
#' @details
#' Just Another Gibbs Sampler (JAGS) is a program which allows analysis of Bayesian models using Markov chain Monte Carlo (MCMC) simulation, and was developed by Martyn Plummer to be an alternative to BUGS that ran on UNIX systems as well as Windows systems. This package is intended to provide additional functions to help automate the process of running models, including convergence diagnostics, collation and plotting of results, and convinience wrappers for running models (either individually or for multiple data sets) over parallel processors and distributed computing clusters.
#'
#' The package also includes a JAGS extension module providing additional distributions - for more details see the runjags vignettes (links in the examples below).  A standalone version of this JAGS module (as well as a version of the runjags package without this module included) is available from the runjags sourceforge page at: \url{https://sourceforge.net/projects/runjags/}
#'
#' @examples
#' 	\dontrun{
#' 	# A quick-start vignette:
#' 	vignette('quickjags', package='runjags')
#'
#' 	# A more comprehensive user guide:
#' 	vignette('userguide', package='runjags')
#'
#'	# For information on how to cite runjags:
#'	citation('runjags')
#'	}
#'
#' @keywords methods
#' @seealso
#' \code{\link{run.jags}} and \code{\link{extend.jags}} for basic model runs
#' \code{\link{runjags-class}} for S3 methods relating to runjags objects, incluing conversion to/from jags objects (for compatibility with the rjags package)
#' \code{\link{runjags.options}} for ways to set default options for runjags functions
#' \code{\link[rjags]{jags.model}} in the rjags package for fine control over the JAGS libraries
#'
#' @references
#' Denwood, M.J. 2016. runjags: An R Package Providing Interface Utilities, Model Templates, Parallel Computing Methods and Additional Distributions for MCMC Models in JAGS. J. Stat. Softw. 71. doi:10.18637/jss.v071.i09.

# The following block is used by usethis to automatically manage
# roxygen namespace tags. Modify with care!
## usethis namespace: start
## usethis namespace: end
NULL
