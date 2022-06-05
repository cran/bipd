## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(comment = "#>", message=FALSE, tidy.opts=list(width.cutoff=60), tidy=TRUE, collapse = TRUE, warning = FALSE) 

## -----------------------------------------------------------------------------
#devtools::install_github("MikeJSeo/bipd")
library(bipd) 

##load in data
ds <- generate_ipdma_example(type = "continuous")
ds2 <- generate_ipdma_example(type = "binary")
head(ds)

## -----------------------------------------------------------------------------
# continuous outcome
ipd <- with(ds, ipdma.model.onestage(y = y, study = studyid, treat = treat, X = cbind(z1, z2), response = "normal", shrinkage = "none"))

## -----------------------------------------------------------------------------
cat(ipd$code)

## -----------------------------------------------------------------------------
samples <- ipd.run(ipd, n.chains = 3, n.burnin = 500, n.iter = 5000)

samples <- samples[,-3] #remove delta[1] which is 0
summary(samples)
#plot(samples) #traceplot and posterior of parameters
#coda::gelman.plot(samples) #gelman diagnostic plot

## -----------------------------------------------------------------------------
treatment.effect(ipd, samples, newpatient = c(1,0.5))

## -----------------------------------------------------------------------------
ipd <- with(ds, ipdma.model.onestage(y = y, study = studyid, treat = treat, X = cbind(z1, z2), response = "normal", shrinkage = "laplace"))
samples <- ipd.run(ipd, pars.save = c("beta", "gamma", "delta", "lambda", "tt"), n.chains = 3, n.burnin = 500, n.iter = 5000)
summary(samples)

## -----------------------------------------------------------------------------
ipd <- with(ds2, ipdma.model.onestage(y = y, study = studyid, treat = treat, X = cbind(w1, w2), response = "binomial", shrinkage = "SSVS"))
samples <- ipd.run(ipd, pars.save = c("beta", "gamma", "delta", "Ind", "eta"), n.chains = 3, n.burnin = 500, n.iter = 5000)
summary(samples)
treatment.effect(ipd, samples, newpatient = c(1,0.5)) # binary outcome reports odds ratio

## -----------------------------------------------------------------------------
##load in data
ds <- generate_ipdnma_example(type = "continuous")
ds2 <- generate_ipdnma_example(type = "binary")
head(ds)

## -----------------------------------------------------------------------------
# continuous outcome
ipd <- with(ds, ipdnma.model.onestage(y = y, study = studyid, treat = treat, X = cbind(z1, z2), response = "normal", shrinkage = "none"))
cat(ipd$code)
samples <- ipd.run(ipd,  pars.save = c("beta", "gamma", "delta"), n.chains = 3, n.burnin = 500, n.iter = 5000)
summary(samples)
treatment.effect(ipd, samples, newpatient = c(1,0.5))

## -----------------------------------------------------------------------------
# SSVS
ipd <- with(ds, ipdnma.model.onestage(y = y, study = studyid, treat = treat, X = cbind(z1, z2), response = "normal", shrinkage = "SSVS"))
samples <- ipd.run(ipd,  pars.save = c("beta", "gamma", "delta", "Ind", "eta"), n.chains = 3, n.burnin = 500, n.iter = 5000)
summary(samples)
treatment.effect(ipd, samples, newpatient = c(1,0.5))
# Bayesian LASSO  
# ipd <- with(ds, ipdnma.model.onestage(y = y, study = studyid, treat = treat, X = cbind(z1, z2), response = "normal", shrinkage = "laplace", lambda.prior = list("dgamma",2,0.1)))
#samples <- ipd.run(ipd, pars.save = c("beta", "gamma", "delta", "lambda", "tt"), n.chains = 3, n.burnin = 500, n.iter = 5000)

