##############################################################################################
# 0. Loading functions
##############################################################################################
# rm(list=ls())
#=============================================================================================
# Mac
#=============================================================================================
# path_OS = "/Users/Ido/"
#============================================================================================
# Windows
#============================================================================================
# path_OS = "C:/Users/lleii/"
#============================================================================================
require(tidyverse)
require(dplyr)
require(clipr)
require(fda)
list.files(paste0(path_OS, "Dropbox/Github/Rpkgs/ADNIprep/R"), full.names = T) %>% walk(source)
list.files(paste0(path_OS, "Dropbox/Github/Rpkgs/StatsR/R"), full.names = T) %>% walk(source)
list.files(paste0(path_OS, "Dropbox/Github/Rpkgs/refineR/R"), full.names = T) %>% walk(source)
#=============================================================================================







#===============================================================================
# Path
#===============================================================================
path_Data_Subject = paste0(path_OS, "Dropbox/Data/ADNI___RS.fMRI___Subjects.Lists")
path_Data_BOLD = paste0(path_OS, "Dropbox/Data/ADNI___RS.fMRI___BOLD.Signals")
path_Data_FC = paste0(path_OS, "Dropbox/Data/ADNI___RS.fMRI___Functional.Connectivity")
path_Data_SB_FDA = paste0(path_OS, "Dropbox/Data/ADNI___RS.fMRI___SB___Functional.Data")
path_Data_SB_FDA_Euclidean = list.files(path_Data_SB_FDA, full.names = T, pattern = "")
path_SubjectsList = path_Data_SB_FDA_Euclidean %>% list.files(full.names=T, pattern = "Subjects_List") %>% list.files(full.names=T)

# path save
path_Save_All = paste0(path_Data_SB_FDA_Euclidean, "/", "SB___All")
path_Save_CNnMCI = paste0(path_Data_SB_FDA_Euclidean, "/", "CNnMCI")
path_Save_CNnAD = paste0(path_Data_SB_FDA_Euclidean, "/", "CNnAD")
path_Save_MCInAD = paste0(path_Data_SB_FDA_Euclidean, "/", "MCInAD")









#===============================================================================
# Load Subjects
#===============================================================================
Subjects_List = readRDS(path_SubjectsList)










#===============================================================================
# Load FC Data
#===============================================================================
path_Data_1 = paste0(path_Data_SB_FDA, "/ROI___AAL3___Static___Pearson___FisherZ___Combined.by.Each.Region___Sorted.by.Voxel.Euclidean.Distance/Sorted_FC") %>% list.files(full.names=T, pattern="Sorted.FC.by.Dist___FunImgARCWSF.rds")
path_Data_2 = paste0(path_Data_SB_FDA, "/ROI___AAL3___Static___Pearson___FisherZ___Combined.by.Each.Region___Sorted.by.Voxel.Euclidean.Distance/Sorted_FC") %>% list.files(full.names=T, pattern="Sorted.FC.by.Dist___FunImgARglobalCWSF.rds")
Data_1 = readRDS(path_Data_1)
Data_2 = readRDS(path_Data_2)
RID_FC = Data_1[[1]] %>% colnames









#===============================================================================
# Smoothing function
#===============================================================================
# Define a function
Smoothing_Function = function(RID, FC, File_Name, Preprocessing_Pipeline, lambdas, path_Export){
  Brain_Regions = names(FC)
  
  Results.list = lapply(seq_along(Brain_Regions), function(i){
    # Raw data
    y_raw = FC[[i]][,-1]
    x_raw = FC[[i]][,1]
    
    
    # Exclude rows with all non-NA values
    y = y_raw[y_raw %>% complete.cases,]
    x = x_raw[x_raw %>% complete.cases]
    
    # RID intersection
    if(!is.null(RID)){
      y = y[, colnames(y) %in% RID]  
    }
    
    
    
    # Bspline Options
    Bspline = list(y = y,
                   x = x,
                   range_vals = c(min(x), max(x)),
                   nbasis = NULL,
                   norder = 4,
                   breaks = x,
                   labmdas = lambdas,
                   m_int2Lfd = 2,
                   argvals = x)
    
  
    
    # Smoothing
    FDA___Smoothing(Bspline = Bspline, 
                    path_Export = paste0(path_Export, "/Smoothing/", Preprocessing_Pipeline, "___", File_Name), 
                    file.name = Brain_Regions[i])
    
    
  }) %>% setNames(Brain_Regions)
  
  
  return(Results.list)
}








#===============================================================================
# Smoothing : FunImgARCWSF
#===============================================================================
RID.list = list(NULL, 
                Subjects_List$RID,
                Subjects_List %>% dplyr::filter(DEMO___DIAGNOSIS_NEW %in% c("AD", "CN")) %>% dplyr::select(RID) %>% unlist,
                Subjects_List %>% dplyr::filter(DEMO___DIAGNOSIS_NEW %in% c("AD", "MCI")) %>% dplyr::select(RID) %>% unlist,
                Subjects_List %>% dplyr::filter(DEMO___DIAGNOSIS_NEW %in% c("MCI", "CN")) %>% dplyr::select(RID) %>% unlist)
File_Names = c("1.FC_Full_without_SubjectsList_Intersection", 
               "2.FC_Full_with_SubjectsList_Intersection",
               "3.FC_Only_AD_n_CN",
               "4.FC_Only_AD_n_MCI",
               "5.FC_Only_MCI_n_CN")


# Non-global
Smoothing_Non.list = lapply(seq_along(RID.list), function(k){
  Smoothing_Function(RID = RID.list[[k]],
                     FC = Data_1, 
                     Preprocessing_Pipeline = "FunImgARCWSF", 
                     path_Export = path_Data_SB_FDA_Euclidean, 
                     File_Name = File_Names[k],
                     lambdas = exp(seq(-5, -4, 0.1))) %>% suppressWarnings()
})


# Global
Smoothing_Global.list = lapply(seq_along(RID.list), function(k){
  Smoothing_Function(RID = RID.list[[k]],
                     FC = Data_2, 
                     Preprocessing_Pipeline = "FunImgARglobalCWSF", 
                     path_Export = path_Data_SB_FDA_Euclidean, 
                     File_Name = File_Names[k],
                     lambdas = exp(seq(-5, -4, 0.1))) %>% suppressWarnings()
})











#===============================================================================
# Combining Results
#===============================================================================
path_Smoothing = list.files(path_Data_SB_FDA_Euclidean, full.names = T, pattern = "Smoothing")
path_Smoothing_Folders = path_Smoothing %>% list.files(full.names=T)
Folders = path_Smoothing %>% list.files(full.names=F)

for(n in 1:length(Folders)){
  path_nth_Each_Brain_Regions = list.files(path_Smoothing_Folders[n], full.name=T, pattern = "\\.rds")
  
  nth_Each_Brain_Regions = list.files(path_Smoothing_Folders[n], full.name=F, pattern = "\\.rds")
  
  nth_Smoothed_Data = lapply(path_nth_Each_Brain_Regions, readRDS) %>% setNames(sub("\\.rds$", "", basename(nth_Each_Brain_Regions)))
  
  saveRDS(nth_Smoothed_Data, paste0(path_Smoothing, "/" , Folders[n], ".rds"))
}























ordinalNet  = function(x, y, 
                       alpha = 1, 
                       standardize = TRUE, 
                       penaltyFactors = NULL, 
                       positiveID = NULL, 
                       family = c("cumulative", "sratio", "cratio",  "acat"), 
                       reverse = FALSE, 
                       link = c("logit", "probit", "cloglog", "cauchit"), 
                       customLink = NULL, 
                       parallelTerms = TRUE, 
                       nonparallelTerms = FALSE, 
                       parallelPenaltyFactor = 1, 
                       lambdaVals = NULL, 
                       nLambda = 20, 
                       lambdaMinRatio = 0.01,
                       includeLambda0 = FALSE, 
                       alphaMin = 0.01, 
                       pMin = 1e-08, 
                       stopThresh = 1e-08, 
                       threshOut = 1e-08, 
                       threshIn = 1e-08, 
                       maxiterOut = 100, 
                       maxiterIn = 100, 
                       printIter = FALSE, 
                       printBeta = FALSE, 
                       warn = TRUE, 
                       keepTrainingData = TRUE){
  #=============================================================================
  # Arguments
  #=============================================================================
  family <- match.arg(family)
  link <- match.arg(link)
  args <- as.list(environment())
  
  if (!keepTrainingData) 
    args$x <- args$y <- NULL
  if (!is.matrix(x)) 
    stop("x should be a matrix.")
  if (any(is.na(x))) 
    stop("x must not contain missing values.")
  if (any(abs(x) == Inf)) 
    stop("x must not contain infinite values.")
  if (!is.factor(y) && !is.matrix(y)) 
    stop("y should be a factor or matrix.")
  if (any(is.na(y))) 
    stop("y must not contain missing values.")
  yMat <- if (is.matrix(y)) 
    y
  else yFactorToMatrix(y)
  wts <- attr(yMat, "wts") <- rowSums(yMat)
  wtsum <- attr(yMat, "wtsum") <- sum(wts)
  nVar <- ncol(x)
  nLev <- ncol(yMat)
  if (reverse) 
    yMat <- yMat[, nLev:1]
  if (nrow(x) != nrow(yMat)) 
    stop("x and y dimensions do not match.")
  if (alpha < 0 || alpha > 1) 
    stop("alpha should be a number such that 0 <= alpha <= 1.")
  if (!is.null(penaltyFactors) && length(penaltyFactors) != 
      nVar) 
    stop(paste0("penaltyFactors should be a numeric vector of length equal to the number ", 
                "of variables in x. Set penaltyFactor=NULL to penalize each variable equally."))
  if (!is.null(penaltyFactors) && any(penaltyFactors < 0)) 
    stop("penaltyFactors values should be nonnegative.")
  if (!is.null(positiveID) && length(positiveID) != nVar) 
    stop(paste0("positiveID should be a logical vector of length equal to the number ", 
                "of variables in x. Set positiveID=NULL for no positive restrictions."))
  if (!is.null(positiveID) && any(!is.logical(positiveID))) 
    stop("positiveID values should be logical.")
  if (!is.null(lambdaVals) && any(lambdaVals < 0)) 
    stop("lambdaVals values should be nonnegative.")
  if (is.null(lambdaVals) && nLambda < 1) 
    stop("nLambda should be >= 1.")
  if (is.null(lambdaVals) && lambdaMinRatio <= 0) 
    stop("lambdaMinRatio should be strictly greater than zero.")
  if (alpha < alphaMin && alphaMin <= 0) 
    stop("alphaMin should be strictly greater than zero.")
  if (length(parallelPenaltyFactor) > 1) 
    stop("parallelPenaltyFactor should be a single value.")
  if (parallelTerms && parallelPenaltyFactor < 0) 
    stop("parallelPenaltyFactor should be >= 0.")
  if (!parallelTerms && parallelPenaltyFactor != 1) 
    warning("parallelPenaltyFactor is not used when parallelTerms=FALSE.")
  if (!parallelTerms && !nonparallelTerms) 
    stop("parallelTerms and nonparallelTerms cannot both be FALSE.")
  if (warn && family == "cumulative" && nonparallelTerms) {
    warning(paste0("For out-of-sample data, the cumulative probability model with ", 
                   "nonparallelTerms=TRUE may predict cumulative probabilities that are not ", 
                   "monotone increasing."))
  }
  if (!is.null(customLink)) {
    link <- customLink
    message("customLink should be a list containing:\n\n                  $linkfun := vectorized link function with domain (0, 1)\n\n                  $linkinv := vectorized inverse link with domain (-Inf, Inf)\n\n                  $mu.eta  := vectorized Jacobian of linkinv\n\n                The customLink argument is not checked, so user should be cautious\n                using it.")
  }
  if ((parallelPenaltyFactor != 1) && !parallelTerms) 
    warning("parallelPenaltyFactor is not used when parallelTerms = FALSE.")
  linkfun <- makeLinkfun(family, link)
  xMeans <- colMeans(x)
  if (standardize) {
    xSD <- sqrt(rowSums(wts * (t(x) - xMeans)^2)/wtsum)
    xSD[xSD == 0] <- 1
    xStd <- t((t(x) - xMeans)/xSD)
  }
  else {
    xStd <- t(t(x) - xMeans)
  }
  xList <- makexList(xStd, nLev, parallelTerms, nonparallelTerms)
  if (is.null(penaltyFactors)) 
    penaltyFactors <- rep(1, nVar)
  penaltyFactorsIntercept <- rep(0, nLev - 1)
  penaltyFactorsParallel <- if (parallelTerms) 
    penaltyFactors * parallelPenaltyFactor
  else NULL
  penaltyFactorsNonparallel <- if (nonparallelTerms) 
    rep(penaltyFactors, nLev - 1)
  else NULL
  penaltyFactors <- c(penaltyFactorsIntercept, penaltyFactorsParallel, 
                      penaltyFactorsNonparallel)
  if (is.null(positiveID)) 
    positiveID <- rep(FALSE, nVar)
  positiveID <- c(rep(FALSE, nLev - 1), rep(positiveID, parallelTerms + 
                                              nonparallelTerms * (nLev - 1)))
  yFreq <- colSums(yMat)/wtsum
  interceptStart <- linkfun$g(yFreq[-nLev])
  interceptStart <- pmin(100, pmax(-100, interceptStart))
  noninterceptStart <- rep(0, nVar * (parallelTerms + nonparallelTerms * 
                                        (nLev - 1)))
  
  #=============================================================================
  # initial
  #=============================================================================
  betaStart <- c(interceptStart, noninterceptStart)
  
  
  
  
  #=============================================================================
  # define getLoglik
  #=============================================================================
  getLoglik = function (pMat, yMat) 
  {
    pkplusone <- 1 - rowSums(pMat)
    pMatFull <- cbind(pMat, pkplusone)
    if (any(pMatFull < 0)) 
      return(-Inf)
    llMat <- yMat * log(pMatFull)
    llMat[yMat == 0] <- 0
    llik <- sum(llMat)
    llik
  }
  
  
  
  #=============================================================================
  # Define cdOut
  #=============================================================================
  cdOut = function (betaHat, lambdaIndex, lambdaNum, lambdaMod, xList, 
                    xMat, yMat, alpha, positiveID, 
                    linkfun, 
                    pMin, 
                    threshOut, 
                    threshIn, 
                    maxiterOut, 
                    maxiterIn, 
                    printIter, 
                    printBeta)
  {
    nObs <- nrow(yMat)
    wts <- if (is.null(attr(yMat, "wts"))) 
      rowSums(yMat)
    else attr(yMat, "wts")
    wtsum <- if (is.null(attr(yMat, "wtsum"))) 
      sum(wts)
    else attr(yMat, "wtsum")
    nLev <- ncol(yMat)
    nVar <- ncol(xMat)
    betaNonzeroIndex <- which(betaHat != 0)
    etaMat <- matrix(xMat[, betaNonzeroIndex, drop = FALSE] %*% 
                       betaHat[betaNonzeroIndex], nrow = nObs, byrow = TRUE)
    pMat <- do.call(rbind, lapply(1:nrow(etaMat), function(i) linkfun$h(etaMat[i, 
    ])))
    loglik <- getLoglik(pMat, yMat)
    penalty <- getPenalty(betaHat, lambdaMod, alpha)
    
    
    
    # 목적함수 정의
    obj <- -loglik/wtsum + penalty
    conv <- FALSE
    iterOut <- 0
    while (!conv && iterOut < maxiterOut) {
      iterOut <- iterOut + 1
      si <- getScoreInfo(xList, yMat, pMat, pMin, linkfun)
      score <- si$score
      info <- si$info
      betaHatOld <- betaHat
      cdInResult <- cdIn(wtsum, betaHat, score, info, alpha, 
                         lambdaMod, positiveID, threshIn, maxiterIn)
      betaHat <- cdInResult$betaHat
      iterIn <- cdInResult$iterIn
      betaNonzeroIndex <- which(betaHat != 0)
      etaMat <- matrix(xMat[, betaNonzeroIndex, drop = FALSE] %*% 
                         betaHat[betaNonzeroIndex], nrow = nObs, byrow = TRUE)
      pMat <- do.call(rbind, lapply(1:nrow(etaMat), function(i) linkfun$h(etaMat[i, 
      ])))
      loglikOld <- loglik
      objOld <- obj
      loglik <- getLoglik(pMat, yMat)
      penalty <- getPenalty(betaHat, lambdaMod, alpha)
      obj <- -loglik/wtsum + penalty
      nhalf <- 0
      while (obj > objOld && nhalf < 10) {
        nhalf <- nhalf + 1
        betaHat <- (betaHat + betaHatOld)/2
        betaNonzeroIndex <- which(betaHat != 0)
        etaMat <- matrix(xMat[, betaNonzeroIndex, drop = FALSE] %*% 
                           betaHat[betaNonzeroIndex], nrow = nObs, byrow = TRUE)
        pMat <- do.call(rbind, lapply(1:nrow(etaMat), function(i) linkfun$h(etaMat[i, 
        ])))
        loglik <- getLoglik(pMat, yMat)
        penalty <- getPenalty(betaHat, lambdaMod, alpha)
        obj <- -loglik/wtsum + penalty
      }
      dif <- (objOld - obj)/(abs(objOld) + 1e-100)
      conv <- dif < threshOut
      objImproved <- obj <= objOld
      if (!objImproved) {
        betaHat <- betaHatOld
        loglik <- loglikOld
      }
      if (printIter) {
        if (iterOut == 1) 
          cat("\nLambda", lambdaIndex, " of ", lambdaNum, 
              "\n")
        cat("outer iteration ", iterOut, ":  ", iterIn, " inner iterations, relative change in objective: ", 
            signif(dif, 2), "\n", sep = "")
      }
      if (printBeta) {
        if (!printIter) {
          if (iterOut == 1) 
            cat("\nLambda", lambdaIndex, " of ", lambdaNum, 
                "\n", sep = "")
          cat("outer iteration ", iterOut, " ", "\n", sep = "")
        }
        cat("betaHat: ", signif(betaHat, 2), "\n\n")
      }
    }
    list(betaHat = betaHat, loglik = loglik, iterOut = iterOut, 
         iterIn = iterIn, dif = dif)
  }
  
  
  
  #=============================================================================
  # mirlsNet
  #=============================================================================
  mirlsNet = function(xList, 
                      yMat, 
                      alpha, 
                      penaltyFactors, 
                      positiveID, 
                      linkfun, 
                      betaStart, 
                      lambdaVals, 
                      nLambda, 
                      lambdaMinRatio, 
                      includeLambda0, 
                      alphaMin, 
                      pMin, 
                      stopThresh, 
                      threshOut, 
                      threshIn, 
                      maxiterOut, 
                      maxiterIn, 
                      printIter, 
                      printBeta){
    wtsum <- if (is.null(attr(yMat, "wtsum"))) 
      sum(yMat)
    else attr(yMat, "wtsum")
    nObs <- nrow(yMat)
    xMat <- do.call(rbind, xList)
    if (!is.null(lambdaVals)) 
      lambdaVals <- sort(lambdaVals, decreasing = TRUE)
    lambdaNum <- if (is.null(lambdaVals)) 
      nLambda + includeLambda0
    else length(lambdaVals)
    fits <- vector("list", length = lambdaNum)
    if (is.null(lambdaVals)) {
      lambdaMod <- ifelse(penaltyFactors == 0, 0, Inf)
      fits[[1]] <- cdOut(betaHat = betaStart, lambdaIndex = 1, 
                         lambdaNum, lambdaMod, xList, xMat, yMat, max(alpha, 
                                                                      alphaMin), positiveID, linkfun, pMin, threshOut, 
                         threshIn, maxiterOut, maxiterIn, printIter, printBeta)
      betaStart <- fits[[1]]$betaHat
      etaMat <- matrix(xMat %*% betaStart, nrow = nObs, byrow = TRUE)
      pMat <- do.call(rbind, lapply(1:nrow(etaMat), function(i) linkfun$h(etaMat[i, 
      ])))
      si <- getScoreInfo(xList, yMat, pMat, pMin, linkfun)
      penID <- penaltyFactors != 0
      lambdaMaxVals <- si$score[penID]/(wtsum * max(alpha, 
                                                    alphaMin) * penaltyFactors[penID])
      lambdaMaxVals[positiveID[penID]] <- pmax(0, lambdaMaxVals[penID & 
                                                                  positiveID])
      lambdaMaxVals <- abs(lambdaMaxVals)
      lambdaMax <- max(lambdaMaxVals)
      lambdaMin <- lambdaMax * lambdaMinRatio
      lambdaVals <- exp(seq(log(lambdaMax), log(lambdaMin), 
                            length.out = nLambda))
      if (includeLambda0) 
        lambdaVals <- c(lambdaVals, 0)
    }
    if (alpha < alphaMin) 
      fits[1] <- list(NULL)
    llik <- if (is.null(fits[[1]])) 
      -Inf
    else fits[[1]]$loglik
    for (i in (1 + !is.null(fits[[1]])):lambdaNum) {
      if ((i > 2) && llikOld != llik && (abs((llikOld - llik)/llikOld) < 
                                         stopThresh)) {
        fits[[i]] <- fits[[i - 1]]
      }
      else {
        lambdaMod <- lambdaVals[i] * penaltyFactors
        lambdaMod <- ifelse(penaltyFactors == 0, 0, lambdaVals[i] * 
                              penaltyFactors)
        fits[[i]] <- cdOut(betaHat = betaStart, lambdaIndex = i, 
                           lambdaNum, lambdaMod, xList, xMat, yMat, alpha, 
                           positiveID, linkfun, pMin, threshOut, threshIn, 
                           maxiterOut, maxiterIn, printIter, printBeta)
        betaStart <- fits[[i]]$betaHat
        llikOld <- llik
        llik <- fits[[i]]$loglik
      }
    }
    iterOut <- sapply(fits, function(x) x$iterOut)
    iterIn <- sapply(fits, function(x) x$iterIn)
    dif <- sapply(fits, function(x) x$dif)
    if (any(iterOut == maxiterOut)) 
      warning(paste0("Reached outer loop iteration limit before convergence ", 
                     "for at least one lambda value. Consider increasing maxiterOut."))
    betaHat <- t(sapply(fits, function(f) f$betaHat))
    loglik <- sapply(fits, function(f) f$loglik)
    list(lambdaVals = lambdaVals, betaHat = betaHat, loglik = loglik, 
         iterOut = iterOut, iterIn = iterIn, dif = dif)
  }
  
  
  
  
  
  
  
  
  
  
  
  #=============================================================================
  # Fitting
  #=============================================================================
  mirlsNetFit <- mirlsNet(xList, 
                          yMat, 
                          alpha, 
                          penaltyFactors, 
                          positiveID, 
                          linkfun, 
                          betaStart, 
                          lambdaVals, 
                          nLambda, 
                          lambdaMinRatio, 
                          includeLambda0, 
                          alphaMin, 
                          pMin, 
                          stopThresh, 
                          threshOut, 
                          threshIn, 
                          maxiterOut, 
                          maxiterIn, 
                          printIter, 
                          printBeta)
  
  
  #=============================================================================
  # Results
  #=============================================================================
  betaHat <- mirlsNetFit$betaHat
  lambdaVals <- mirlsNetFit$lambdaVals
  loglik <- mirlsNetFit$loglik
  iterOut <- mirlsNetFit$iterOut
  iterIn <- mirlsNetFit$iterIn
  dif <- mirlsNetFit$dif
  intercepts0 <- betaHat[, 1:(nLev - 1), drop = FALSE]
  nonintercepts0 <- betaHat[, -(1:(nLev - 1)), drop = FALSE]
  unscaleFact <- if (standardize) 
    xMeans/xSD
  else xMeans
  intAdjust <- matrix(0, nrow = nrow(betaHat), ncol = nLev - 
                        1)
  if (parallelTerms) 
    intAdjust <- intAdjust + (nonintercepts0[, 1:nVar, drop = FALSE] %*% 
                                unscaleFact)[, rep(1, nLev - 1), drop = FALSE]
  if (nonparallelTerms) 
    intAdjust <- intAdjust + sapply(1:(nLev - 1), function(i) {
      nonintercepts0[, (nVar * (i - 1 + parallelTerms) + 
                          1):(nVar * (i + parallelTerms)), drop = FALSE] %*% 
        unscaleFact
    })
  intercepts <- intercepts0 - intAdjust
  nonintercepts <- if (standardize) 
    t(t(nonintercepts0)/xSD)
  else nonintercepts0
  coefs <- cbind(intercepts, nonintercepts)
  catOrder <- if (reverse) 
    nLev:2
  else 1:(nLev - 1)
  interceptNames <- paste0("(Intercept):", catOrder)
  xNames <- if (is.null(colnames(x))) 
    paste0("X", 1:nVar)
  else colnames(x)
  parallelNames <- nonparallelNames <- NULL
  if (parallelTerms) 
    parallelNames <- xNames
  if (nonparallelTerms) 
    nonparallelNames <- paste0(rep(xNames, nLev - 1), ":", 
                               rep(catOrder, each = nVar))
  colnames(coefs) <- c(interceptNames, parallelNames, nonparallelNames)
  nNonzero <- apply(coefs, 1, function(b) sum(b != 0))
  aic <- -2 * loglik + 2 * nNonzero
  bic <- -2 * loglik + log(wtsum) * nNonzero
  loglikNull <- getLoglikNull(yMat)
  devPct <- 1 - loglik/loglikNull
  fit <- list(coefs = coefs, lambdaVals = lambdaVals, loglik = loglik, 
              nNonzero = nNonzero, aic = aic, bic = bic, devPct = devPct, 
              iterOut = iterOut, iterIn = iterIn, dif = dif, nLev = nLev, 
              nVar = nVar, xNames = xNames, args = args)
  class(fit) <- "ordinalNet"
  fit
}









