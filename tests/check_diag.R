library("runjags")

# Try to get the same JAGS executable as used by rjags:
if(.Platform$OS.type=="unix"){
  jagspath <- if(nzchar(.Platform$r_arch)){
    file.path(rjags::pkg.compile.flags("JAGS_ROOT", print=FALSE), .Platform$r_arch, "bin", "jags")
  }else{
    file.path(rjags::pkg.compile.flags("JAGS_ROOT", print=FALSE), "bin", "jags")
  }
}else{
  # This will use the same version as for rjags:
  jagspath <- findjags()
}
runjags.options(jagspath=jagspath, repeatable.methods=TRUE)

# Only do numerical equivalence checks if versions match (they may not on macOS with rjags_4-18 from CRAN due to forwarding of /usr/local/bin/jags to /opt/jags/versions/current):
jv <- testjags()
test_equiv <- (jv$JAGS.major == jv$rjags.major)

# Only run this test for JAGS 4.x and >= 5.0.2 due to a bug in 5.0.0/5.0.1:
if(requireNamespace("rjags") && !(rjags::jags.version() %in% c("5.0.0","5.0.1"))){

  # Simulate the data
  X <- 1:100
  Y <- rnorm(length(X), 2*X + 10, 1)
  Y[1] <- 300

  # Model in the JAGS format
  model <- "model {
    for(i in 1 : N){
	    Y[i] ~ dnorm(true.y[i], precision);
	    true.y[i] <- (m * X[i]) + c
    }
    m ~ dunif(-1000,1000)
    c ~ dunif(-1000,1000)
    precision ~ dexp(1)
  }"

  # Data and initial values in a named list format,
  # with explicit control over the random number
  # generator used for each chain (optional):
  data <- list(X=X, Y=Y, N=length(X))
  inits1 <- list(m=1, c=1, precision=1,
                 .RNG.name="base::Super-Duper", .RNG.seed=1)
  inits2 <- list(m=0.1, c=10, precision=1,
                 .RNG.name="base::Wichmann-Hill", .RNG.seed=2)

  # Check without any dic/ped:
  ri <- run.jags(model=model, monitor=c("m", "c", "precision"), burnin=1000, sample=1000, data=data, n.chains=2, method="int", inits=list(inits1,inits2)) |> extend.jags(sample=1000)
  rr <- run.jags(model=model, monitor=c("m", "c", "precision"), burnin=1000, sample=1000, data=data, n.chains=2, method="rjags", inits=list(inits1,inits2)) |> extend.jags(sample=1000)
  stopifnot(identical(NA, ri$pd), identical(NA, rr$pd), identical(NA, ri$deviance.table), identical(NA, rr$deviance.table), identical(NA, ri$deviance.sum), identical(NA, rr$deviance.sum))

  # Check just dic (which also implies ped):
  ri <- run.jags(model=model, monitor=c("m", "c", "precision", "dic"), burnin=1000, sample=1000, data=data, n.chains=2, method="int", inits=list(inits1,inits2))
  rr <- run.jags(model=model, monitor=c("m", "c", "precision", "dic"), burnin=1000, sample=1000, data=data, n.chains=2, method="rjags", inits=list(inits1,inits2))
  stopifnot(identical(NA, ri$pd), identical(NA, rr$pd), !identical(NA, ri$deviance.table), !identical(NA, rr$deviance.table), !identical(NA, ri$deviance.sum), !identical(NA, rr$deviance.sum))
  if(test_equiv){
    stopifnot(abs(ri$deviance.table - rr$deviance.table) < 1e-3)
    stopifnot(abs(ri$deviance.sum - rr$deviance.sum) < 1e-3)
  }

  # Check just ped:
  ri <- run.jags(model=model, monitor=c("m", "c", "precision", "ped"), burnin=1000, sample=1000, data=data, n.chains=2, method="int", inits=list(inits1,inits2)) |> extend.jags(sample=1000)
  rr <- run.jags(model=model, monitor=c("m", "c", "precision", "ped"), burnin=1000, sample=1000, data=data, n.chains=2, method="rjags", inits=list(inits1,inits2)) |> extend.jags(sample=1000)
  stopifnot(identical(NA, ri$pd), identical(NA, rr$pd), !identical(NA, ri$deviance.table), !identical(NA, rr$deviance.table), !identical(NA, ri$deviance.sum), !identical(NA, rr$deviance.sum))

  # Check pd, popt, full.pd one at a time:
  for(em in c("pd","popt","full.pd")){
    ri <- run.jags(model=model, monitor=c("m", "c", "precision", em), burnin=1000, sample=1000, data=data, n.chains=2, method="int", inits=list(inits1,inits2))
    rr <- run.jags(model=model, monitor=c("m", "c", "precision", em), burnin=1000, sample=1000, data=data, n.chains=2, method="rjags", inits=list(inits1,inits2))
    if(em=="full.pd"){
      if(test_equiv) stopifnot(!identical(NA, ri$pd), !identical(NA, rr$pd), abs(ri$pd - rr$pd) < 1e-3)
    }else{
      stopifnot(identical(NA, ri$pd), identical(NA, rr$pd))
    }
    ri <- extend.jags(ri, sample=1000)
    rr <- extend.jags(rr, sample=1000)
  }

  # Check DIC/PED estimates compared to rjags directly:
  rr <- run.jags(model=model, monitor=c("m", "c", "precision", "ped", "dic"), burnin=1000, sample=0, data=data, n.chains=2, method="rjags", inits=list(inits1,inits2))
  jm1 <- as.jags(rr)
  jm2 <- as.jags(rr)
  rm <- extend.jags(rr, sample=1000)

  dic <- rjags::dic.samples(jm1, 1000, type="pD")
  if(test_equiv){
    stopifnot(abs((sum(dic$deviance) + sum(dic$penalty)) - sum(rm$deviance.sum[-3])) < sqrt(.Machine$double.eps))
  }

  ped <- rjags::dic.samples(jm2, 1000, type="popt")
  if(test_equiv){
    stopifnot(abs((sum(ped$deviance) + sum(ped$penalty)) - sum(rm$deviance.sum[-2])) < sqrt(.Machine$double.eps))
  }
}
