## To download and check reverse dependencies for runjags

update.packages(ask=FALSE)
pkgs <- reverse_dependencies_with_maintainers('runjags', which=c('Depends','Imports','Suggests'))[,'Package']
install.packages(pkgs, repos="https://cran.rstudio.com/", type="binary", dependencies=TRUE)
install.packages("INLA",repos=c(getOption("repos"),INLA="https://inla.r-inla-download.org/R/stable"), dep=TRUE)
BiocManager::install("Icens")
# etc: BiocManager::install("Icens")

wd <- getwd()
setwd("~/Desktop")
unlink('totest', recursive=TRUE)
dir.create('totest')
pkgv <- download.packages(pkgs, repos="https://cran.rstudio.com/", file.path(getwd(), 'totest'), type='source')[,2]

setwd('totest')
unlink(paste0(pkgs, ".Rcheck"), recursive=TRUE)
for(p in pkgv){
	print(p)
	system(paste0("R CMD check ", p, ""))
	print("")
	print("")
	print("")
}
setwd(wd)
