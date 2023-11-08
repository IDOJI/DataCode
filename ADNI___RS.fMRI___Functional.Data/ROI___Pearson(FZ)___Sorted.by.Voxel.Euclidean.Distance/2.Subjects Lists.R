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

path_Save = paste0(path_Data_SB_FDA_Euclidean, "/", "Subjects_List")







#===============================================================================
# Load FC Data
#===============================================================================
path_SortedFC = list.files(path_Data_SB_FDA, full.names=T, pattern = "Euclidean") %>% list.files(full.names=T, pattern = "Sorted")
path_Data = list.files(path_SortedFC, full.names=T, pattern = "\\.rds$")
Data_1 = readRDS(path_Data[1])
Data_2 = readRDS(path_Data[2])

Data.list = list(Data_1, Data_2)

RID_FC = Data_1[[1]] %>% colnames









#===============================================================================
# Subjects
#===============================================================================
Subjects = read.csv(paste0(path_Data_Subject, "/Subjects_Lists_Exported/Final/[Final_Selected]_Subjects_list.csv"))

# Intersection with FC
Subjects_New = Subjects %>% 
  dplyr::filter(NFQ___BAND.TYPE == "SB") %>% 
  dplyr::select(RID,
                DEMO___DIAGNOSIS_NEW,
                DEMO___SEX, 
                DEMO___AGE,
                DEMO___ADNIMERGE___APOE4, 
                DEMO___MMSE___MMSCORE,
                # NFQ___BAND.TYPE,
                # VISCODE2
                ) %>% 
  dplyr::mutate(DEMO___DIAGNOSIS_NEW = case_when(
    DEMO___DIAGNOSIS_NEW %in% c("AD(Possible)", "AD(Probable)", "Dementia") ~ "AD",
    DEMO___DIAGNOSIS_NEW %in% c("LMCI", "EMCI") ~ "MCI",
    TRUE ~ DEMO___DIAGNOSIS_NEW
  )) %>% 
  dplyr::mutate(DEMO___SEX = case_when(
    DEMO___SEX == "Male" ~ 1,
    TRUE ~ 0
  )) %>% 
  dplyr::arrange(RID) %>% 
  dplyr::mutate(RID = paste0("RID_", sprintf("%04d", RID))) %>% 
  dplyr::filter(RID %in% RID_FC) %>% 
  dplyr::mutate(DEMO___DIAGNOSIS_NEW = factor(DEMO___DIAGNOSIS_NEW, levels = c("CN", "MCI", "AD"))) %>% 
  as_tibble




#===============================================================================
# Extract
#===============================================================================
Subjects_AD.MCI = Subjects_New %>% dplyr::filter(DEMO___DIAGNOSIS_NEW %in% c("AD", "MCI"))
Subjects_AD.CN = Subjects_New %>% dplyr::filter(DEMO___DIAGNOSIS_NEW %in% c("AD", "CN"))
Subjects_MCI.CN = Subjects_New %>% dplyr::filter(DEMO___DIAGNOSIS_NEW %in% c("MCI", "CN"))




#===============================================================================
# Export
#===============================================================================
saveRDS(Subjects_New, paste0(path_Save, "/Subjects_List_SB___ALL.rds"))
saveRDS(Subjects_AD.MCI, paste0(path_Save, "/Subjects_List_SB___ADMCI.rds"))
saveRDS(Subjects_AD.CN, paste0(path_Save, "/Subjects_List_SB___ADCN.rds"))
saveRDS(Subjects_MCI.CN, paste0(path_Save, "/Subjects_List_SB___MCICN.rds"))







