\name{xgrid.run}

\alias{xgrid.run}
\alias{xgrid.submit}
\alias{xapply}
\alias{xgrid.results}
\alias{xgrid.jobs}
\alias{xgrid.delete}

\title{Remote execution of user-specified R functions on Apple Xgrid distributed computing clusters}
\description{
   Allows arbitrary R code to be executed on Apple Xgrid distributed
   computing clusters and the results returned to the R session of the
   user.  Jobs can either be run synchronously (the process will wait
   for the model to complete before returning the results) or
   asynchronously (the process will terminate on submission of the job
   and results are retrieved at a later time).  Access to an Xgrid
   cluster with R (along with all packages required by the function)
   installed is required.  Due to the dependance on Xgrid software to
   perform the underlying submission and retrieval of jobs, these
   functions can only be used on machines running Mac OS X.  
   
   The two utility functions xgrid.jobs and xgrid.delete allow the
   currently running jobs to be examined and deleted from inside R.
   
   *Note*  Apple has discontinued Xgrid from Mac OS 10.8 onwards, and
   these functions are deprecated as of runjags version 2.

   
}
\usage{


xgrid.run(f=function(iteration){}, niters=1, object.list=list(),
	file.list=character(0), max.threads=100, arguments=as.list(1:niters),
	Rversion="", packages=list(), artfun=function() writeLines("1"),
	email=NA, profiling=TRUE, cpuarch=NA, minosversion=NA,
	queueforserver=FALSE, hostnode=NA, forcehost=FALSE, ramrequired=10,
	jobname=NA, cleanup=TRUE, showprofiles=FALSE, Rpath='/usr/bin/R',
	Rbuild='64', max.filesize="1GB", 
	mgridpath=system.file("xgrid", "mgrid.sh", package="runjags"),
	hostname=Sys.getenv("XGRID_CONTROLLER_HOSTNAME"),
	password=Sys.getenv("XGRID_CONTROLLER_PASSWORD"), tempdir=FALSE,
	keep.files=FALSE, show.output=TRUE, threads=min(niters, max.threads), ...)

xgrid.submit(f=function(iteration){}, niters=1, object.list=list(),
	file.list=character(0), max.threads=100, arguments=as.list(1:niters),
	Rversion="", packages=list(), artfun=function() writeLines("1"),
	email=NA, profiling=TRUE, cpuarch=NA, minosversion=NA,
	queueforserver=FALSE, hostnode=NA, forcehost=FALSE, ramrequired=10,
	jobname=NA, Rpath='/usr/bin/R', Rbuild='64', max.filesize="1GB",
	mgridpath=system.file("xgrid", "mgrid.sh", package="runjags"),
	hostname=Sys.getenv("XGRID_CONTROLLER_HOSTNAME"),
	password=Sys.getenv("XGRID_CONTROLLER_PASSWORD"), show.output=TRUE,
	separate.jobs=FALSE, threads=min(niters, max.threads), ...)

xgrid.results(jobinfo, wait=TRUE, partial.retrieve=!wait,
	cleanup=!partial.retrieve, show.output=TRUE)

xgrid.jobs(comment=FALSE, user=FALSE, jobs=10,
	mgridpath=system.file("xgrid", "mgrid.sh", package="runjags"),
	hostname=Sys.getenv("XGRID_CONTROLLER_HOSTNAME"),
	password=Sys.getenv("XGRID_CONTROLLER_PASSWORD"))

xgrid.delete(jobinfo, keep.files=FALSE)

xapply(X, FUN, method.options=list(), ...)

}

\arguments{

\item{f}{the function to be iterated over on Xgrid.  This must take at
least 1 argument, the first of which represents the value of the
'arguments' list to be passed to the function for that iteration, which
is the iteration number unless 'arguments' (or 'X' for xapply) is
specified.  Any other arguments to be passed to the function can be
supplied as additional arguments to xgrid.run/xgrid.submit/xapply. The
value(s) of interest should be returned by this function (an object of
any class is permissable).  No default.}

\item{niters}{the total number of iterations over which to evaluate the
function f.  This can be less than the number of threads, in which case
multiple iterations are evaluated serially as part of the same task.  No
default.}

\item{object.list}{a named list of objects that will be copied to the
global environment on Xgrid and so will be visible inside the function.
Alternatively, this can be a character vector of objects, that will be
looked for in the global environment, rather than a named list.  All
other objects in the current working directory will not be visible when
the function is evaluated. THIS INCLUDES LIBRARIES WHICH MUST BE
RE-CALLED WITHIN THE FUNCTION BEFORE USE.  In order to use functions
within an R library it is therefore necessary for the required library
to be installed on the Xgrid nodes on which the job will be run.  If not
all nodes have the required libraries installed, you can use an ART
script to ensure the job is sent only to machines that do (see the
example provided below), or you can use mgrid to manually request
certain nodes using the '-f -h <nodename>' options. Alternatively, text
files containing R code can be included in the 'file.list' argument and
source()d within the function. Default blank list (no objects copied).}

\item{file.list}{a vector of filenames representing files in the current
working directory that will be copied to the working directory of the
executed function.  This allows R code to be source()d, datasets to be
loaded, and compiled code to be dynamically linked within the function,
among other things.  Default none.}

\item{max.threads}{the maximum number of tasks (or jobs) to split into.}

\item{arguments}{a list of values to be passed as the first argument to
the function, with each element of the list specifying the value at that
iteration.  Default is as.list(1:niters) which passes only the iteration
number to the function.}

\item{Rversion}{the required R version for worker nodes to be given
tasks - may include '=' or '>=' to signify exact or minimum version
requirements.}

\item{packages}{a list of R packages that must be installed on host
nodes for them to be used.}

\item{artfun}{an optional user-specified R function to determine the
suitability of nodes in an ART script - must either cat() 1 (indicating
suitable) or 0 (indicating unsuitable) to stdout.}

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

\item{Rpath}{the path to the R executable on the xgrid machines. If not
all machines on the xgrid cluster have R (or a required package)
installed then it is possible to use an ART script to ensure the job is
sent to only machines that do - see the examples section for details. 
Default '/usr/bin/R' (this is the default install location for R).}

\item{Rbuild}{the preferred binary of R to invoke.  '64' results in
'{Rpath}64' (if it exists), '32' in '{Rpath}32' (if it exists) and ''
(or either of '32' or '64' if they are not found) results in {Rpath}.
Notice that this indicates a preference, not a certainty - if the
indicated build is not avalable then another will be used.  Also note
that specifying '64' may be ignored for PPC nodes depending on what
version of R they are running (you can ensure only intel nodes are used
with mgrid using sub.options='-c intel').  Default ''.}

\item{max.filesize}{the maximum total size of the objects produced by
the function for each thread if xgrid.method=separatejobs, or for the
entire job if xgrid.method=separatetasks.  This is a failsafe designed
to prevent attempted transfer of huge files bringing the xgrid
controller down.  If the maximum size is exceeded for a thread or job
then the results are erased for all iterations within that thread or
job, and the job will likely have to be re-submitted.  If each chain is
likely to return a large amount of information, then 'separatejobs'
should be used because jobs are retrieved individually which reduces the
chances of overloading the Xgrid controller.  The object.list is also
checked to ensure it complies with the maximum size, but the file.list
and any objects saved to the working directory by the function are NOT
automatically cheked.  Units can be provided as either "MB" or "GB". 
Default "1GB".}

\item{mgridpath}{the path to the local mgrid script - default uses the
version installed with the runjags package.}

\item{hostname}{the hostname of the Xgrid server to connect to.}

\item{password}{the password for the Xgrid server given by hostname.}

\item{tempdir}{for xgrid.run, option to use the temporary directory as
specified by the system rather than creating files in the working
directory. Any files created in the temporary directory are removed when
the function exits. A temporary directory cannot be used for
xgrid.submit. Default TRUE when running the job synchronously.}

\item{keep.files}{option to keep the folder with files needed to run the
job rather than deleting it when the job is deleted from Xgrid.  This
may be useful for attempting to bug fix failing jobs.  Default FALSE.}

\item{show.output}{option to print the output of the function (obtained
using cat, writeLine or print for example) at each iteration after
retrieving the job(s) from xgrid.  If FALSE, the output is suppressed. 
Default TRUE.}

\item{separate.jobs}{option to submit multiple jobs to Xgrid, to help
with file size constraints (see the entry for 'threads' below).}

\item{threads}{the number of threads (either jobs if separate.jobs==TRUE
or tasks otherwise) to generate for the job. Each thread is sent to a
separate node for execution, so the more threads there are the faster
the job will finish (unless the number of threads exceeds the number of
available nodes).  A very large number of threads may cause problems
with the Xgrid controller, hence the ability to set fewer threads than
iterations.  Functions that return objects of a very large size should
use a large number of threads and use the xgrid.method 'separatejobs' to
minimise the total size of objects returned by each xgrid job.}

\item{...}{additional arguments to be passed to the function provided by
f.}

\item{jobinfo}{the output of a call to xgrid.submit.}

\item{wait}{option to wait for the Xgrid job to complete if it has not
done so already.}

\item{partial.retrieve}{for xgrid.results, option to retrieve results of
partially completed jobs.  By default makes cleanup FALSE. Default
TRUE.}

\item{comment}{option to display any comments relevant to the Xgrid jobs
running.}

\item{user}{option to display information on the user that submitted
each Xgrid job.}

\item{jobs}{the number of (most recent) jobs to display information
for.}

\item{X}{for xapply, a vector (atomic or list) over which to apply the
function provided.  Equivalent to 'arguments' for xgrid.run, with niters
= length(X).}

\item{FUN}{for xapply, the function to be passed to xgrid.run as 'f'.}

\item{method.options}{for xapply, any arguments (with the exception of
'f', 'niters' and 'arguments' which are ignored) to be passed to
xgrid.run.}


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



\value{For xgrid.submit, a list containing the jobname (which will be
required by xgrid.results to retrieve the job) and the job ID(s) for use
with the xgrid command line facilities.  For xgrid.run and
xgrid.results, the output of the function over all iterations is
returned as a list, with each element of the list representing the
results at each iteration.  If the function returned an error, then the
error will be held in the list as the return value at the iteration that
returned the error.  If the function returns an object that exceeds the
'max.filesize' when combined with the results for other iterations in
that job (or greater than max.filesize/threads for multi-task jobs), the
results for that thread are replaced with an error message (this is to
prevent the xgrid controller crashing due to transferring large files).
The xapply function returns as xgrid.run (or xgrid.submit if
xgrid.options=list(submitandstop=TRUE) in which case the results can be
retrieved using xgrid.results).

}



\seealso{

\code{\link[parallel]{mclapply}} and \code{\link[parallel]{parLapply}}
in the parallel package for parallel execution of code over multiple
local (or remote) cores.

}


\keyword{methods}