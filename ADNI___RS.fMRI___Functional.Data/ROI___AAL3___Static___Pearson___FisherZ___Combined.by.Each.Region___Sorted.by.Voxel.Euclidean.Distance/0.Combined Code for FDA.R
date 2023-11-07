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
path_Data_Subject = paste0(path_OS, "Dropbox/Data/ADNI___RS.fMRI___Subjects.Lists/Subjects_Lists_Exported/Final")
path_Data_SB_FDA = paste0(path_OS, "Dropbox/Data/ADNI___RS.fMRI___SB___Functional.Data")
path_Data_SB_FDA_Euclidean = list.files(path_Data_SB_FDA, full.names = T, pattern = "Euclidean")
path_Scores = list.files(path_Data_SB_FDA_Euclidean, pattern="Scores", full.names=T)








#===============================================================================
# Loading Data
#===============================================================================
Subjects = read.csv(list.files(path_Data_Subject, full.names=T)[1]) %>% as_tibble
fPCA_Scores_1 = readRDS(path_Scores[1])
fPCA_Scores_2 = readRDS(path_Scores[2])









#===============================================================================
# Modify Subjects list
#===============================================================================
Subjects_Selected = Subjects %>% 
  dplyr::select(RID, DEMO___DIAGNOSIS_NEW, DEMO___SEX, NFQ___BAND.TYPE) %>% 
  dplyr::rename(DIAGNOSIS = DEMO___DIAGNOSIS_NEW, SEX = DEMO___SEX)

# AD
Subjects_Selected = Subjects_Selected %>% 
  dplyr::mutate(DIAGNOSIS = ifelse(DIAGNOSIS  %in% c("AD(Possible)", "AD(Probable)", "Dementia"), "AD", DIAGNOSIS))

# MCI
Subjects_Selected = Subjects_Selected %>% 
  dplyr::mutate(DIAGNOSIS = ifelse(DIAGNOSIS  %in% c("LMCI", "EMCI"), "MCI", DIAGNOSIS))

# Factorize
Subjects_Selected$DIAGNOSIS = factor(Subjects_Selected$DIAGNOSIS, levels = c("CN", "MCI", "AD"))


# Select SB
Subjects_Selected = Subjects_Selected %>% dplyr::filter(NFQ___BAND.TYPE=="SB") %>% dplyr::select(-NFQ___BAND.TYPE) 

# SEX
Subjects_Selected = Subjects_Selected %>% dplyr::mutate(SEX = ifelse(SEX=="Female", 0, 1))




#===============================================================================
# Modify fPC Score list
#===============================================================================
# Group Numbering
fPCA_Scores_GroupNum_1 = fPCA_Scores_1$Features_Group_Nums
fPCA_Scores_GroupNum_2 = fPCA_Scores_2$Features_Group_Nums


# Scores
fPCA_Scores_1 = fPCA_Scores_1$fPCA_Scores
fPCA_Scores_2 = fPCA_Scores_2$fPCA_Scores
names(fPCA_Scores_2) = paste0("Global___", names(fPCA_Scores_2))




# FunImgARCWSF
Combined_1 = bind_cols(Subjects_Selected, fPCA_Scores_1)
Combined_1 = list(Data = Combined_1, Features_Group_Nums = fPCA_Scores_GroupNum_1)
saveRDS(Combined_1, file = paste0(path_Data_SB_FDA_Euclidean, "/Combined___FunImgARCWSF.rds"))



# FunImgARglobalCWSF
Combined_2 = bind_cols(Subjects_Selected, fPCA_Scores_2)
Combined_2 = list(Data = Combined_2, Features_Group_Nums = fPCA_Scores_GroupNum_2)
saveRDS(Combined_2, file = paste0(path_Data_SB_FDA_Euclidean, "/Combined___FunImgARglobalCWSF.rds"))



# +global (서로 다른 그룹으로 취급)
Combined_3 = bind_cols(Subjects_Selected, fPCA_Scores_1, fPCA_Scores_2)
Combined_3 = list(Data = Combined_3, Features_Group_Nums = c(fPCA_Scores_GroupNum_1, length(fPCA_Scores_GroupNum_1) + fPCA_Scores_GroupNum_2))
saveRDS(Combined_3, file = paste0(path_Data_SB_FDA_Euclidean, "/Combined___FunImgARCWSF + FunImgARglobalCWSF___Diff.rds"))


# 서로 같은 그룹 취급







