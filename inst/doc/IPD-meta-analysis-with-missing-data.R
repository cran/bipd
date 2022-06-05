## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(comment = "#>", message=FALSE, tidy.opts=list(width.cutoff=60), tidy=TRUE, collapse = TRUE, warning = FALSE) 

## -----------------------------------------------------------------------------
#install.packages("bipd")
#or devtools::install_github("MikeJSeo/bipd")
library(bipd)
set.seed(1)
simulated_dataset <- generate_sysmiss_ipdma_example(Nstudies = 10, Ncov = 5, sys_missing_prob = 0, magnitude = 0.5, heterogeneity = 0.1)

simulated_dataset_missing <- simulated_dataset
randomindex <- sample(c(TRUE,FALSE), dim(simulated_dataset_missing)[1], replace = TRUE, prob = c(0.2, 0.8))
randomindex2 <- sample(c(TRUE,FALSE), dim(simulated_dataset_missing)[1], replace = TRUE, prob = c(0.1, 0.9))
simulated_dataset_missing[randomindex,c("x1")] <- NA
simulated_dataset_missing[randomindex2,c("x3")] <- NA
head(simulated_dataset_missing)

## ---- warning = FALSE, message = FALSE, results = 'hide', comment = FALSE-----
library(miceadds) #for multilevel datasets without systematically missing predictors
imputation <- ipdma.impute(simulated_dataset_missing, covariates = c("x1", "x2", "x3", "x4", "x5"), typeofvar = c("continuous", "binary", "binary", "continuous", "continuous"), interaction = TRUE, studyname = "study", treatmentname = "treat", outcomename = "y", m = 5)  

## -----------------------------------------------------------------------------
multiple.imputations <- imputation$imp.list
for(ii in 1:length(multiple.imputations)){
  
  current.data <- multiple.imputations[[ii]]
  
  X <- with(current.data, apply(current.data[,c("x1", "x2", "x3", "x4", "x5")],2, function(x) as.numeric(x)))
  
  ipd <- with(current.data, ipdma.model.onestage(y = y, study = study, treat = treat, X = X, response = "normal", shrinkage = "none"))
  
  #Run only 100 iterations for demonstration
  samples <- ipd.run(ipd, pars.save = c("beta", "gamma", "delta"), n.chains = 3, n.burnin = 100, n.iter = 100) 

  if(ii == 1){
    final.result <- samples
  } else{
    final.result <- add.mcmc(final.result, samples)
  }
}

## -----------------------------------------------------------------------------
summary(final.result)

## -----------------------------------------------------------------------------
X <- as.matrix(apply(simulated_dataset[, c("x1", "x2", "x3", "x4", "x5")], 2, as.numeric))
# calculate overall mean
overall_mean <- apply(X, 2, mean, na.rm = TRUE)
overall_sd <- apply(X, 2, sd)

treatment.effect(ipd, samples, newpatient = c(0.5, 1, 1, -0.5, 0.5), scale_mean = overall_mean, scale_sd = overall_sd)

