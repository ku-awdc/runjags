\name{xgrid.run.jags}

\alias{xgrid.autorun.jags}
\alias{xgrid.autorun.JAGS}
\alias{xgrid.run.jags}
\alias{xgrid.run.JAGS}
\alias{xgrid.submit.jags}
\alias{xgrid.submit.JAGS}
\alias{xgrid.extend.jags}
\alias{xgrid.extend.JAGS}
\alias{xgrid.autoextend.jags}
\alias{xgrid.autoextend.JAGS}
\alias{xgrid.results.jags}
\alias{xgrid.results.JAGS}
\alias{xgrid.autorun.jagsfile}
\alias{xgrid.autorun.JAGSfile}
\alias{xgrid.run.jagsfile}
\alias{xgrid.run.JAGSfile}
\alias{xgrid.submit.jagsfile}
\alias{xgrid.submit.JAGSfile}


\title{Run a JAGS Model using an Apple Xgrid distributed computing cluster from Within R}

\description{

Extends the functionality of the run.jags family of
functions to use with Apple Xgrid distributed computing clusters.  Jobs
can either be run synchronously using xgrid.(auto)run.jags in
which case the process will wait for the model to complete before
returning the results, or asynchronously using xgrid.submit.jags
in which case the process will terminate on submission of the job and
results are retrieved at a later time using xgrid.results.jags.  The
latter function can also be used to check the progress of incomplete
simulations without stopping or retrieving the full job.  Access to an
Xgrid cluster with JAGS (although not necessarily R) installed is
required.  Due to the dependance on Xgrid software to perform the
underlying submission and retrieval of jobs, these functions can only be
used on machines running Mac OS X.  Further details of required
environmental variables and the optional mgrid script to enable
multi-task jobs can be found in the details section.

*Note*  Apple has discontinued Xgrid from Mac OS 10.8 onwards, and
these functions are deprecated as of runjags version 2.
}


\usage{

xgrid.run.jags(model, max.threads=Inf, JAGSversion=">=2.0.0", 
	email=NA, profiling=TRUE, cpuarch=NA, minosversion=NA, 
	queueforserver=FALSE, hostnode=NA, forcehost=FALSE,
	ramrequired=10, jobname=NA, cleanup=TRUE, 
	showprofiles=FALSE, jagspath='/usr/local/bin/jags', 
	mgridpath=system.file("xgrid","mgrid.sh", package="runjags"),
	hostname=Sys.getenv("XGRID_CONTROLLER_HOSTNAME"),
	password=Sys.getenv("XGRID_CONTROLLER_PASSWORD"), ...)

xgrid.autorun.jags(model, max.threads=Inf, JAGSversion=">=2.0.0", 
	email=NA, profiling=TRUE, cpuarch=NA, minosversion=NA, 
	queueforserver=FALSE, hostnode=NA, forcehost=FALSE,
	ramrequired=10, jobname=NA, cleanup=TRUE, 
	showprofiles=FALSE, jagspath='/usr/local/bin/jags', 
	mgridpath=system.file("xgrid","mgrid.sh", package="runjags"),
	hostname=Sys.getenv("XGRID_CONTROLLER_HOSTNAME"),
	password=Sys.getenv("XGRID_CONTROLLER_PASSWORD"), ...)

xgrid.extend.jags(runjags.object, max.threads=Inf, JAGSversion=">=2.0.0", 
	email=NA, profiling=TRUE, cpuarch=NA, minosversion=NA, 
	queueforserver=FALSE, hostnode=NA, forcehost=FALSE,
	ramrequired=10, jobname=NA, cleanup=TRUE, 
	showprofiles=FALSE, jagspath='/usr/local/bin/jags', 
	mgridpath=system.file("xgrid","mgrid.sh", package="runjags"),
	hostname=Sys.getenv("XGRID_CONTROLLER_HOSTNAME"),
	password=Sys.getenv("XGRID_CONTROLLER_PASSWORD"), ...)

xgrid.autoextend.jags(runjags.object, max.threads=Inf, JAGSversion=">=2.0.0", 
	email=NA, profiling=TRUE, cpuarch=NA, minosversion=NA, 
	queueforserver=FALSE, hostnode=NA, forcehost=FALSE,
	ramrequired=10, jobname=NA, cleanup=TRUE, 
	showprofiles=FALSE, jagspath='/usr/local/bin/jags', 
	mgridpath=system.file("xgrid","mgrid.sh", package="runjags"),
	hostname=Sys.getenv("XGRID_CONTROLLER_HOSTNAME"),
	password=Sys.getenv("XGRID_CONTROLLER_PASSWORD"), ...)

xgrid.submit.jags(model, max.threads=Inf, JAGSversion=">=2.0.0", 
	email=NA, profiling=TRUE, cpuarch=NA, minosversion=NA, 
	queueforserver=FALSE, hostnode=NA, forcehost=FALSE,
	ramrequired=10, jobname=NA, jagspath='/usr/local/bin/jags',
	mgridpath=system.file("xgrid", "mgrid.sh", package="runjags"),
	hostname=Sys.getenv("XGRID_CONTROLLER_HOSTNAME"),
	password=Sys.getenv("XGRID_CONTROLLER_PASSWORD"), ...)

xgrid.results.jags(background.runjags.object, wait=TRUE, cleanup=TRUE)

}



\arguments{

\item{model}{a JAGS model, as would be provided to the
\code{\link{run.jags}} function.}

\item{runjags.object}{an object of class runjags, as would be provided
to the \code{\link{extend.jags}} function.}

\item{background.runjags.object}{an object of class runjags-bginfo,
returned from the xgrid.submit.jags function.}

\item{max.threads}{the maximum number of tasks to split the job into.}

\item{JAGSversion}{the required JAGS version for worker nodes to be
given tasks - may include '=' or '>=' to signify exact or minimum
version requirements.}

\item{email}{an email address to be used to notify of job status.}

\item{profiling}{option to use ART ranking to select the most suitable
host nodes preferentially.}

\item{cpuarch}{option to restrict the job to 'ppc' or 'intel' nodes.}

\item{minosversion}{option to restrict the job to nodes running a
minimum Mac OS version.}

\item{queueforserver}{option to restrict the job to nodes considered to
be Server machines.}

\item{hostnode}{option to prefer (or restrict to if forcehost==TRUE)
running the job on the specified nodes - must be provided as a single
character string with the colon character (:) separating node names.}

\item{forcehost}{option to restrict the job to only nodes specified by
'hostnode'.}

\item{ramrequired}{the minimum amount of free RAM (obtained using an
approximation) for each node to be assigned a task.}

\item{jobname}{the name to give the job on Xgrid (optional).}

\item{cleanup}{option to remove the job from Xgrid after completion.}

\item{showprofiles}{option to show the node scores based on the ART
ranking used.}

\item{jagspath}{the path to JAGS on the host nodes.}

\item{mgridpath}{the path to the local mgrid script - default uses the
version installed with the runjags package.}

\item{hostname}{the hostname of the Xgrid server to connect to.}

\item{password}{the password for the Xgrid server given by hostname.}

\item{wait}{option to wait for the Xgrid job to finish if it has not
already done so.}

\item{...}{other options to be passed to the underlying
\code{\link{run.jags}} family functions as if the model were being run
locally.}

}



\details{

These functions allow JAGS models to be run on Xgrid distributed
computing clusters from within R using the same syntax as required to
run the models locally.  All the functionality could be replicated by
saving all necessary objects to files and using the Xgrid command line
utility to submit and retrieve the job manually; these functions merely
provide the convenience of not having to do this manually.  Xgrid
support is only available on Mac OS X machines running OS X 10.5-10.7
(Xgrid support was discontinued in Mac OS X 10.8).  

The xgrid controller hostname and password can also be set as environmental
variables. The command line version of R knows about environmental
variables set in the .profile file, but unfortunately the GUI version
does not and requires them to be set from within R using:


Sys.setenv(XGRID_CONTROLLER_HOSTNAME="<hostname>")

Sys.setenv(XGRID_CONTROLLER_PASSWORD="<password>")

(These lines could be copied into your .Rprofile file for a 'set and
forget' solution)


Note that the runjags package also contains a utility shell script
called 'mgrid' that enhances the capabilities of Xgrid substantially -
to install this from the command line navigate to the folder given by
system.file("xgrid", package="runjags") and from the terminal type 'sudo
cp mgrid.sh /usr/local/bin/mgrid (or similar) to make the script visible
in your search path.  Help on the mgrid script can then be obtained by
typing 'mgrid' (with no arguments) at the command line.

}



\value{
Equivalent to that of the \code{\link{run.jags}} family of functions.

}



\seealso{

\code{\link{run.jags}}, \code{\link{autorun.jags}} and
\code{\link{runjags-class}} for more information on JAGS models, 
including operations on parallel processors

\code{\link{run.jags.study}} for functions to execute JAGS model
validation exercises over parallel processors

}


\keyword{methods}