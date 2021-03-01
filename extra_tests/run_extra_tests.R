## Make sure required packages are installed and updated:

install.packages(c("coda","rjags","modeest","testthat","knitr","rmarkdown","spelling"))

library("runjags")

## Dependent functions:

reverse_dependencies_with_maintainers <-
  function(packages, which = c("Depends", "Imports", "LinkingTo", "Suggests"),
           recursive = FALSE)
  {
    require("tools")

    contrib.url(getOption("repos")["CRAN"], "source") # trigger chooseCRANmirror() if required
    description <- sprintf("%s/web/packages/packages.rds",
                           getOption("repos")["CRAN"])
    con <- if(substring(description, 1L, 7L) == "file://")
      file(description, "rb")
    else
      url(description, "rb")
    on.exit(close(con))
    db <- readRDS(gzcon(con))
    rownames(db) <- NULL

    rdepends <- package_dependencies(packages, db, which,
                                     recursive = recursive,
                                     reverse = TRUE)
    rdepends <- sort(unique(unlist(rdepends)))
    pos <- match(rdepends, db[, "Package"], nomatch = 0L)

    db[pos, c("Package", "Version", "Maintainer")]
  }

check_reverse_dependencies <- function(package='runjags', dir=paste0('checkrevdeps_',package), which = c("Depends", "Imports", "LinkingTo", "Suggests")){

  stopifnot(length(package)==1)

  if(dir.exists(dir)){
    unlink(dir, recursive=TRUE)
  }

  dir.create(dir)
  setwd(dir)
  dir.create('sources')

  pkgs <- reverse_dependencies_with_maintainers(package, which=which, recursive=FALSE)

  # Install packages with dependencies:
  install.packages(pkgs[,'Package'], dependencies=TRUE)

  # Download the packages to check:
  download.packages(pkgs[,'Package'], destdir='_sources')
  # And check each in a separate shell:
  for(f in list.files('sources')){
    system(paste0('R CMD check -o _sources ', file.path('_sources',f), ' > ', f, '.txt 2>&1 &'))
  }

  setwd('../')
}


## Check reverse dependencies in separate shells:
check_reverse_dependencies()
warning("Check reverse dependencies manually")
## TODO: print a message to say which did not pass check cleanly


## Run extended tests:
source("/Documents/GitHub/runjags/extra_tests/ExtendedTests.R")

