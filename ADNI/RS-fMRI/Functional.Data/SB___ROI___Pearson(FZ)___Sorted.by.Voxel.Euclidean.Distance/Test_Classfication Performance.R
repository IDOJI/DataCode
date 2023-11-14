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

# File names
Names_Subjects = basename(path_Subjects) %>% tools::file_path_sans_ext()
Names_Scores = basename(path_Scores) %>% tools::file_path_sans_ext()







#===============================================================================
# Loading Data
#===============================================================================
Subjects = readRDS(path_Subjects)
Scores = lapply(path_Scores, readRDS) %>% setNames(Names_Scores)



#===============================================================================
# Data setting
#===============================================================================
Selected_Scores = Scores$FunImgARglobalCWSF___3.FC_Only_AD_n_CN
Selected_GroupNum = Selected_Scores$Features_Group_Nums
Selected_Scores = Selected_Scores$fPCA_Scores


Subjects$Subjects_Full












