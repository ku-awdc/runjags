# runjags
An R package implementing interface utilities, model templates, parallel computing methods and additional distributions for MCMC models in JAGS

This package requires JAGS (https://sourceforge.net/projects/mcmc-jags/) at both compile time and run time: compiler flags are obtained directly from the rjags package, which ensures that JAGS is compiled against the same JAGS installation as rjags (and this is re-checked at run time when loading the JAGS extension module contained within runjags). Note that installing a different major version of rjags will require the runjags package to be re-compiled.
