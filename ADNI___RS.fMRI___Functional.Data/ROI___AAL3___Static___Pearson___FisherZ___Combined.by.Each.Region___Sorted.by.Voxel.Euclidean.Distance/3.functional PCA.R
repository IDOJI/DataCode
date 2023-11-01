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
# WD
#===============================================================================
# path_clip()
# setwd("/Users/Ido/Library/CloudStorage/Dropbox/Github/Papers/Papers___Wrting/___Papers___진행중/Data")
# setwd(paste0(path_OS, "Dropbox/Github/@GitHub___Papers___Writing___PCC FDA/Data"))
# getwd()











#===============================================================================
# Path
#===============================================================================
path_Data_Subject = paste0(path_OS, "Dropbox/Data/#ADNI___RS.fMRI___Subjects.Lists")
path_Data_BOLD = paste0(path_OS, "Dropbox/Data/ADNI___RS.fMRI___BOLD.Signals")
path_Data_FC = paste0(path_OS, "Dropbox/Data/ADNI___RS.fMRI___Functional.Connectivity")
path_Data_SB_FDA = paste0(path_OS, "Dropbox/Data/ADNI___RS.fMRI___SB___Functional.Data")
path_Data_SB_FDA_Euclidean = list.files(path_Data_SB_FDA, full.names = T, pattern = "Euclidean")
path_Data_SB_FDA_Euclidean__fPCA = paste0(path_Data_SB_FDA_Euclidean, "/fPCA")









#===============================================================================
# Loading smoothing Data
#===============================================================================
path_Smoothing = list.files(path_Data_SB_FDA_Euclidean, pattern = "Smoothing___.*\\.rds$", full.names = TRUE)
Smoothing.list = list(readRDS(path_Smoothing[1]), readRDS(path_Smoothing[2]))










#===============================================================================
# functional PCA
#===============================================================================
# Pipeline
Preprocessing_Pipeline = c("FunImgARCWSF", "FunImgARglobalCWSF")


# fPCA
fPCA.list = list()

for(k in 1:length(Preprocessing_Pipeline)){
  
  kth_Smoothing = Smoothing.list[[k]]
  
  kth_Regions = names(kth_Smoothing)
  
  fPCA.list[[k]] = lapply(seq_along(kth_Smoothing), function(i){
    
    FDA___fPCA(fdobj = kth_Smoothing[[i]]$smoothing$fd, 
               threshold = 0.9, 
               path_Export = paste0(path_Data_SB_FDA_Euclidean__fPCA, "___", Preprocessing_Pipeline[k]), 
               file.name = kth_Regions[i])
    
  }) %>% setNames(kth_Regions)
  
  saveRDS(fPCA.list[[k]], paste0(path_Data_SB_FDA_Euclidean, "/fPCA___", Preprocessing_Pipeline[k], ".rds"))
}








#===============================================================================
# Exporting fPCA scores
#===============================================================================
fPCA_1 = fPCA.list[[1]]
fPCA_2 = fPCA.list[[2]]



















