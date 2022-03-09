library("runjags")
set.seed(2022-03-09)

N <- 600
status <- rbinom(N, 1, rep(c(0.25,0.5,0.75), each=N/3))
testdata <- data.frame(Population = rep(1:3, each=N/3), Test1 = rbinom(N, 1, status*0.75 + (1-status)*0.05), Test2 = rbinom(N, 1, status*0.75 + (1-status)*0.05), Test3=rbinom(N, 1, status*0.75 + (1-status)*0.05))

template_huiwalter(testdata, outfile="huiwalter_model.txt", covariance=TRUE, cov_as_cor=TRUE)
results <- run.jags("huiwalter_model.txt")
#unlink("huiwalter_model.txt")
results

template_huiwalter(testdata, outfile="huiwalter_model.txt", covariance=TRUE, cov_as_cor=FALSE)
results <- run.jags("huiwalter_model.txt")
#unlink("huiwalter_model.txt")
results

template_huiwalter(testdata, outfile="huiwalter_model.txt", covariance=FALSE, cov_as_cor=TRUE)
results <- run.jags("huiwalter_model.txt")
#unlink("huiwalter_model.txt")
results

template_huiwalter(testdata, outfile="huiwalter_model.txt", covariance=FALSE, cov_as_cor=FALSE)
results <- run.jags("huiwalter_model.txt")
#unlink("huiwalter_model.txt")
results
