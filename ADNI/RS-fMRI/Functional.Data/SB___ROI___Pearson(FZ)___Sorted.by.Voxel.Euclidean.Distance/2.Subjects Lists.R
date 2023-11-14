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
#=============================================================================================
path_Dropbox = paste0(path_OS, "Dropbox")
path_GitHub = list.files(path_Dropbox, pattern = "Github", full.names = T)
path_Rpkgs = list.files(path_GitHub, pattern = "Rpkgs", full.names = T)
Rpkgs = c("ADNIprep", "StatsR", "refineR")
Load = sapply(Rpkgs, function(y){
  list.files(paste0(path_Rpkgs, "/", y, "/R"), full.names = T) %>% walk(source) 
})
#=============================================================================================
path_Data = paste0(path_Dropbox, "/Data")
path_ADNI = list.files(path_Data, full.names = T, pattern = "ADNI")
path_Subjects = list.files(path_ADNI, full.names = TRUE, pattern = "Subjects.Lists") %>% 
  list.files(., full.names = TRUE) %>%
  grep("Subjects_Lists_Exported$", ., value = TRUE) %>% 
  list.files(., full.names = TRUE) %>% 
  grep("Final$", ., value = TRUE) %>% 
  list.files(., full.names = TRUE) %>% 
  grep("list.csv$", ., value  =TRUE)
path_FD = list.files(path_ADNI, full.names = T, pattern = "Functional.Data")
#=============================================================================================








#===============================================================================
# Path
#===============================================================================
path_Euclidean = list.files(path_FD, pattern = "Euclidean", full.names = T)
path_Euclidean_SortedFC = list.files(path_Euclidean, pattern = "Sorted", full.names = T) %>% 
  list.files(., full.names = TRUE, pattern = "\\.rds$") %>% 
  grep("___FunImgAR", ., value = TRUE)

path_Save = paste0(path_Euclidean, "/Subjects_List_Splitted")






#===============================================================================
# Extract RID from FC data
#===============================================================================
RID_FC = readRDS(path_Euclidean_SortedFC[1])[[1]] %>% colnames







#===============================================================================
# Subjects
#===============================================================================
Subjects = read.csv(path_Subjects)

# Intersection with FC
Subjects_Full = Subjects %>% 
  dplyr::filter(NFQ___BAND.TYPE == "SB") %>% 
  dplyr::select(RID,
                DEMO___DIAGNOSIS_NEW,
                DEMO___SEX, 
                DEMO___AGE,
                DEMO___ADNIMERGE___APOE4, 
                DEMO___ADNIMERGE___PTEDUCAT,
                DEMO___MMSE___MMSCORE,
                PTDEMO___PTHAND
                # DEMO___ADNIMERGE___PTMARRY
                # NFQ___BAND.TYPE,
                # VISCODE2
                ) %>% 
  dplyr::mutate(DEMO___DIAGNOSIS_NEW = case_when(
    DEMO___DIAGNOSIS_NEW %in% c("AD(Possible)", "AD(Probable)", "Dementia") ~ "AD",
    DEMO___DIAGNOSIS_NEW %in% c("LMCI", "EMCI") ~ "MCI",
    TRUE ~ DEMO___DIAGNOSIS_NEW
  )) %>% 
  # dplyr::mutate(DEMO___ADNIMERGE___PTMARRY = factor(DEMO___ADNIMERGE___PTMARRY)) %>% 
  dplyr::mutate(DEMO___SEX = case_when(
    DEMO___SEX == "Male" ~ 1,
    TRUE ~ 0
  )) %>% 
  dplyr::mutate(PTDEMO___PTHAND = case_when(
    PTDEMO___PTHAND == "Right" ~ 1,
    TRUE ~ 0
  )) %>% 
  dplyr::arrange(RID) %>% 
  dplyr::mutate(RID = paste0("RID_", sprintf("%04d", RID))) %>% 
  dplyr::filter(RID %in% RID_FC) %>% 
  dplyr::mutate(DEMO___DIAGNOSIS_NEW = factor(DEMO___DIAGNOSIS_NEW, levels = c("CN", "MCI", "AD")))










#===============================================================================
# Subset by Disease Group
#===============================================================================
Subjects_List.list = list()
# AD MCI CN
Subjects_List.list[[1]] = Subjects_Full = Subjects_Full
Subjects_List.list[[2]] = Subjects_NonNA = Subjects_Full %>% na.omit
# AD MCI
Subjects_List.list[[3]] = Subjects_AD.MCI_Full = Subjects_Full %>% dplyr::filter(DEMO___DIAGNOSIS_NEW %in% c("AD", "MCI"))
Subjects_List.list[[4]] = Subjects_AD.MCI_NonNA = Subjects_AD.MCI_Full %>% na.omit
# AD CN
Subjects_List.list[[5]] = Subjects_AD.CN_Full = Subjects_Full %>% dplyr::filter(DEMO___DIAGNOSIS_NEW %in% c("AD", "CN"))
Subjects_List.list[[6]] = Subjects_AD.CN_NonNA = Subjects_AD.CN_Full %>% na.omit
# MCI CN
Subjects_List.list[[7]] = Subjects_MCI.CN_Full = Subjects_Full %>% dplyr::filter(DEMO___DIAGNOSIS_NEW %in% c("MCI", "CN"))
Subjects_List.list[[8]] = Subjects_MCI.CN_NonNA = Subjects_MCI.CN_Full %>% na.omit

names(Subjects_List.list) = c("Subjects_Full", "Subjects_Full_NA",
                              "Subjects_ADMCI", "Subjects_ADMCI_NA",
                              "Subjects_ADCN", "Subjects_ADCN_NA",
                              "Subjects_MCICN", "Subjects_MCICN_NA")







#===============================================================================
# Splitting by "Diagnosis" maintaining proportion
#===============================================================================
fs::dir_create(path_Save, recurse = T)

for(i in 1:length(Subjects_List.list)){
  
  ith_Splitted_Data = SUB___Fold(Data = Subjects_List.list[[i]], Var_1 = "DEMO___DIAGNOSIS_NEW", y_Var = "DEMO___DIAGNOSIS_NEW")
  
  saveRDS(ith_Splitted_Data, paste0(path_Save, "/", names(Subjects_List.list)[i], ".rds"))
  
}












