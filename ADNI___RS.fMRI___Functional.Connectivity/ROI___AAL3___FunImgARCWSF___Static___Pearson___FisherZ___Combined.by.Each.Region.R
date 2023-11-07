##############################################################################################
# 0. Loading functions
##############################################################################################
# rm(list=ls())
#=============================================================================================
# Mac
#=============================================================================================
# path_OS = "/Users/Ido/"
# path_External.Drive = "/Volumes/Seagate/"
#============================================================================================
# Windows
#============================================================================================
path_OS = "C:/Users/lleii/"
#============================================================================================
require(tidyverse)
require(dplyr)
require(clipr)
list.files(paste0(path_OS, "Dropbox/Github/Rpkgs/ADNIprep/R"), full.names = T) %>% walk(source)
list.files(paste0(path_OS, "Dropbox/Github/Rpkgs/StatsR/R"), full.names = T) %>% walk(source)
list.files(paste0(path_OS, "Dropbox/Github/Rpkgs/refineR/R"), full.names = T) %>% walk(source)
#=============================================================================================







#===============================================================================
# WD
#===============================================================================
# path_clip()
# setwd("/Users/Ido/Library/CloudStorage/Dropbox/Github/Papers/Papers___Wrting/___Papers___진행중/Data")
setwd(paste0(path_OS, "Dropbox/Github/@GitHub___Papers___Writing___PCC FDA/Data"))
getwd()







#===============================================================================
# Path
#===============================================================================
path_Data_Subject = paste0(path_OS, "Dropbox/Data/#ADNI___RS.fMRI___Subjects.Lists")
path_Data_BOLD = paste0(path_OS, "Dropbox/Data/ADNI___RS.fMRI___BOLD.Signals")
path_Data_FC = paste0(path_OS, "Dropbox/Data/ADNI___RS.fMRI___Functional.Connectivity")








#===============================================================================
# FC among ROI
#===============================================================================
path_FC_1 = paste0(path_Data_FC, "/ROI___AAL3___FunImgARCWSF___Static___Pearson___FisherZ")
path_FC_2 = paste0(path_Data_FC, "/ROI___AAL3___FunImgARglobalCWSF___Static___Pearson___FisherZ")
path_FC_1_Files = list.files(path_FC_1, full.names = T)
path_FC_2_Files = list.files(path_FC_2, full.names = T)

FC_1_Combined.list = lapply(path_FC_1_Files, function(y){
  ith_FC = readRDS(y)
  diag(ith_FC) = NA
  return(ith_FC)
})

FC_2_Combined.list = lapply(path_FC_2_Files, function(y){
  ith_FC = readRDS(y)
  diag(ith_FC) = NA
  return(ith_FC)
})






#===============================================================================
# Extract RID
#===============================================================================
FC_1_Files_RID = list.files(path_FC_1, full.names = F) %>% substr(1,8)
FC_2_Files_RID = list.files(path_FC_2, full.names = F) %>% substr(1,8)







#===============================================================================
# Brain Regions
#===============================================================================
Brain_Regions = FC_1_Combined.list[[1]] %>% names







#===============================================================================
# Combine by ROI
#===============================================================================
Combined_FC_1.df = do.call(rbind, FC_1_Combined.list)
Combined_FC_2.df = do.call(rbind, FC_2_Combined.list)

FC_1_Combined.by.Region = lapply(seq_along(Brain_Regions), function(k){
  kth_Region.mat = matrix(Combined_FC_1.df[,k], nrow=ncol(Combined_FC_1.df), ncol=nrow(Combined_FC_1.df)/ncol(Combined_FC_1.df))  
  colnames(kth_Region.mat) = FC_1_Files_RID
  rownames(kth_Region.mat) = Brain_Regions
  return(kth_Region.mat)
}) %>% setNames(Brain_Regions)


FC_2_Combined.by.Region = lapply(seq_along(Brain_Regions), function(k){
  kth_Region.mat = matrix(Combined_FC_2.df[,k], nrow=ncol(Combined_FC_2.df), ncol=nrow(Combined_FC_2.df)/ncol(Combined_FC_2.df))  
  colnames(kth_Region.mat) = FC_2_Files_RID
  rownames(kth_Region.mat) = Brain_Regions
  return(kth_Region.mat)
}) %>% setNames(Brain_Regions)




#===============================================================================
# Exporting
#===============================================================================
# Exporting path & name
path_Export_1 = paste0(path_FC_1, "___Combined.by.Each.Region")
path_Export_2 = paste0(path_FC_2, "___Combined.by.Each.Region")

# dir
dir.create(path_Export_1, F)
dir.create(path_Export_2, F)

# Export
saveRDS(FC_1_Combined.by.Region, paste0(path_Export_1, "/FC_Combined_by_Regions.rds"))
saveRDS(FC_2_Combined.by.Region, paste0(path_Export_2, "/FC_Combined_by_Regions.rds"))









