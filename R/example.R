example_runjags <- function(...){
	
	X <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
	Y <- c(10.8090157039524, 13.9434806085521, 15.787689123995, 16.9569401422281, 22.0824675991525, 21.2058041795089, 24.403335735507, 27.6592408754351, 28.6753194874265, 28.9965911129099)

	model <- "model { 
	for(i in 1 : N){ 
		Y[i] ~ dnorm(true.y[i], precision);
		true.y[i] <- (m * X[i]) + c
	} 
	m ~ dunif(-1000,1000)
	c ~ dunif(-1000,1000) 
	precision ~ dexp(1)
	}"

	data <- list(X=X, Y=Y, N=length(X))
	inits1 <- list(m=1, c=1, precision=1,
	.RNG.name="base::Super-Duper", .RNG.seed=1)
	inits2 <- list(m=0.1, c=10, precision=1,
	.RNG.name="base::Wichmann-Hill", .RNG.seed=2)

	out <- run.jags(model=model, monitor=c("m", "c", "precision"), 
	data=data, n.chains=2, inits=list(inits1,inits2), ...)
	
	return(out)	
	
}