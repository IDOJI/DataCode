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
path_Data_SB_FDA = paste0(path_OS, "Dropbox/Data/ADNI___RS.fMRI___SB___Functional.Data")
path_Data_SB_FDA_Euclidean = list.files(path_Data_SB_FDA, full.names = T, pattern = "Euclidean")

# Files
path_Subjects = list.files(path_Data_SB_FDA_Euclidean, full.names=T, pattern = "Subjects") %>% list.files(full.names=T, pattern = "\\.rds$")
path_Scores = list.files(path_Data_SB_FDA_Euclidean, full.names=T, pattern = "Scores") %>% list.files(full.names=T, pattern = "\\.rds$")
path_Export = paste0(path_Data_SB_FDA_Euclidean, "/", "Final_Combined")

# File names
Names_Scores = basename(path_Scores) %>% tools::file_path_sans_ext()






#===============================================================================
# Loading Data
#===============================================================================
Subjects = readRDS(path_Subjects)
Names_Subjects_Lists = names(Subjects)

Scores = lapply(path_Scores, readRDS) %>% setNames(Names_Scores)
Names_Scores = names(Scores)





#===============================================================================
# Combining
#===============================================================================
fs::dir_create(path_Export, recurse = T)

Combined.list = lapply(seq_along(Scores), function(i){
  
  ith_Score_Name = str_split(Names_Scores[i], "___")[[1]][2]
  
  # Subjects list
  if(grep("_NA", ith_Score_Name, ignore.case = T) %>% length > 0){
    ith_Subject_List = Subjects[[which(ith_Score_Name == Names_Subjects_Lists)]]
  }else{
    ith_Subject_List = Subjects[[which(ith_Score_Name == Names_Subjects_Lists)]] %>% dplyr::select(DEMO___DIAGNOSIS_NEW)
  }
  
  
  ith_Scores = Scores[[i]]$fPCA_Scores
  
  ith_VarGroupNums = Scores[[i]]$Features_Group_Nums
  
  
  # Combine
  ith_Combined = cbind(ith_Subject_List, ith_Scores)
  
  ith_Combined.list = list(Combined = ith_Combined, VarGroupNums = ith_VarGroupNums)  
  
  saveRDS(ith_Combined.list, paste0(path_Export, "/", Names_Scores[i], ".rds"))
  
  ith_Combined.list
  
}) %>% setNames(Names_Scores)








