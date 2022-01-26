## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(comment = "#>", message=FALSE, tidy.opts=list(width.cutoff=60), tidy=TRUE, collapse = TRUE, warning = FALSE) 

## -----------------------------------------------------------------------------
#install.packages("bipd")
#or devtools::install_github("MikeJSeo/bipd")
library(bipd)
set.seed(1)
simulated_dataset <- generate_sysmiss_ipdma_example(Nstudies = 10, Ncov = 5, sys_missing_prob = 0.3, magnitude = 0.2, heterogeneity = 0.1)
head(simulated_dataset)

## -----------------------------------------------------------------------------
missP <- findMissingPattern(simulated_dataset, covariates = c("x1", "x2", "x3", "x4", "x5"), typeofvar = c("continuous", "binary", "binary", "continuous", "continuous"), studyname = "study",  treatmentname = "treat", outcomename = "y")

## -----------------------------------------------------------------------------
missP$missingpercent
missP$sys_covariates
missP$spor_covariates

## -----------------------------------------------------------------------------
simulated_dataset2 <- simulated_dataset
randomindex <- sample(c(TRUE,FALSE), dim(simulated_dataset)[1], replace = TRUE, prob = c(0.1, 0.9))
simulated_dataset2[randomindex,"x1"] <- NA
missP2 <- findMissingPattern(simulated_dataset2, covariates = c("x1", "x2", "x3", "x4", "x5"), typeofvar = c("continuous", "binary", "binary", "continuous", "continuous"), studyname = "study",  treatmentname = "treat", outcomename = "y")
missP2$missingpercent
missP2$sys_covariates
missP2$spor_covariates

## ---- warning = FALSE, message = FALSE, results = 'hide', comment = FALSE-----
library(mice) #for datasets with only one study level
library(miceadds) #for multilevel datasets without systematically missing predictors
library(micemd) #for multilevel datasets with systematically missing predictors.

## -----------------------------------------------------------------------------
imputation <- ipdma.impute(simulated_dataset, covariates = c("x1", "x2", "x3", "x4", "x5"), typeofvar = c("continuous", "binary", "binary", "continuous", "continuous"), interaction = TRUE, studyname = "study", treatmentname = "treat", outcomename = "y", m = 5)    

## -----------------------------------------------------------------------------
imputation$meth
imputation$pred

## -----------------------------------------------------------------------------
ls(imputation)

## -----------------------------------------------------------------------------
length(imputation$imp.list)

## ---- warning = FALSE, message = FALSE, results = 'hide', comment = FALSE-----
imputation2 <- ipdma.impute(simulated_dataset2, covariates = c("x1", "x2", "x3", "x4", "x5"), typeofvar = c("continuous", "binary", "binary", "continuous", "continuous"), sys_impute_method = "2l.glm", interaction = FALSE, studyname = "study", treatmentname = "treat", outcomename = "y", m = 5)  

## -----------------------------------------------------------------------------
imputation2$meth
imputation2$pred

## ---- warning = FALSE, message = FALSE, results = 'hide', comment = FALSE-----
meth <- imputation2$meth
meth["x1"] <- "2l.norm"
imputation2 <- ipdma.impute(simulated_dataset2, covariates = c("x1", "x2", "x3", "x4", "x5"), typeofvar = c("continuous", "binary", "binary", "continuous", "continuous"), sys_impute_method = "2l.glm", interaction = FALSE, studyname = "study", treatmentname = "treat", outcomename = "y", m = 5, meth = meth) 

## ---- warning = FALSE, message = FALSE, results = 'hide', comment = FALSE-----
imputation2 <- ipdma.impute(simulated_dataset2, covariates = c("x1", "x2", "x3", "x4", "x5"), typeofvar = c("continuous", "binary", "binary", "continuous", "continuous"), sys_impute_method = "pmm", interaction = FALSE, studyname = "study", treatmentname = "treat", outcomename = "y", m = 5) 

## -----------------------------------------------------------------------------
imputation2$meth

