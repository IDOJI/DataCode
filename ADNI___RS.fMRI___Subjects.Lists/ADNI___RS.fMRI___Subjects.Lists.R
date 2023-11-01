################################################################################
# 0. Loading functions
################################################################################
# rm(list=ls())
#========================================================
# Mac
#========================================================
# path_OS = "/Users/Ido/"
# path_External.Drive = "/Volumes/Seagate/"
#========================================================
# Windows
#========================================================
path_OS = "C:/Users/lleii/"


#========================================================
# SB
#========================================================
path_External.Drive = "E:/"
path_External.Drive_RS.fMRI = paste0(path_External.Drive, "ADNI/ADNI_RS.fMRI___SB")






#========================================================
# MB
#========================================================
# path_External.Drive = "F:/" # MB
# path_External.Drive_RS.fMRI = paste0(path_External.Drive, "ADNI/ADNI_RS.fMRI___MB")









#========================================================
# File names
#========================================================
# QC
Subjects_QC_ADNI2GO  = "MAYOADIRL_MRI_IMAGEQC_12_08_15_24Jun2023"
Subjects_QC_ADNI3    = "MAYOADIRL_MRI_QUALITY_ADNI3_24Jun2023"
# NFQ
Subjects_NFQ         = "MAYOADIRL_MRI_FMRI_NFQ_04_06_22"
# Search
Subjects_Search_FMRI      = "idaSearch_6_23_2023_fMRI"
Subjects_Search_MRI      = "idaSearch_6_23_2023_MRI"
Subjects_Search = "idaSearch_6_24_2023"
Subjects_Study.Visits.Summary = "STUDYSUM_25Jun2023"
# Registry
Subjects_Registry = "REGISTRY_23Jun2023"
# Others
Subjects_BLCHANGE    = "BLCHANGE_11Jun2023"
Subjects_DX_Summary  = "DXSUM_PDXCONV_ADNIALL_11Jun2023"
Subjects_PTDEMO = "PTDEMOG_24Jun2023"
Subjects_APOE = "APOERES_21Jun2023"
# CV
Subjects_CV_ADNI2GO = "Clinician Verification/CLIELG_24Jun2023"
Subjects_CV_ADNI3 = "Clinician Verification/CLIELG_ADNI3_24Jun2023"
# ADAS
Subjects_ADAS = "ADAS_ADNIGO23_25Jun2023"
Subjects_MMSE = "MMSE_25Jun2023"
Subjects_APOE = "APOERES_21Jun2023"
#========================================================
# Other path
#========================================================
# 외장하드
path_ADNI.Unzipped.Folders = paste0(path_External.Drive_RS.fMRI, "/ADNI")

# 전처리 완료 폴더
path_Preprocessing.Completed = paste0(path_External.Drive_RS.fMRI, "/Completed")
path_Preprocessing.Error = paste0(path_External.Drive_RS.fMRI, "/Error")

path_Normalization.Pictures = paste0(path_External.Drive_RS.fMRI, "/Completed_Normalization_Pictures")
path_Normalization.Pictures_Excluding = paste0(path_External.Drive_RS.fMRI, "/Completed_Normalization_Pictures_Excluding")

path_Completed_Voxelwise.Signals_Raw = paste0(path_External.Drive_RS.fMRI, "/Completed_Voxelwise.BOLD.Signals_Raw")
path_Completed_Voxelwise.Signals_Standardization_Zscore = paste0(path_External.Drive_RS.fMRI, "/Completed_Voxelwise.BOLD.Signals_Standardization_Zscore")


# Subject 리스트
path_Subjects.Lists_Downloaded = paste0(path_OS, "Dropbox/Github/Papers___Data/Papers___Data___ADNI___RS.fMRI___Subjects.Lists/___Subjects_Lists_Downloaded")
path_Export_Subjects.Lists = paste0(path_OS, "Dropbox/Github/Papers___Data/Papers___Data___ADNI___RS.fMRI___Subjects.Lists/TEST")
path_Export_Subjects.Lists = paste0(path_Export_Subjects.Lists, "/Final")

path_All.Subjects.EPB.List.File = paste0(path_Export_Subjects.Lists, "/0.All_Subjects/[Final_Selected]_Subjects_list_EPB_(0.All_Subjects).csv")
path_All.Subjects.MT1.List.File = paste0(path_Export_Subjects.Lists, "/0.All_Subjects/[Final_Selected]_Subjects_list_MT1_(0.All_Subjects).csv")
# path_Old.All.Subjects.List.Folder = paste0(path_OS, "Dropbox/Github/Rpkgs/ADNIprep/Subjects_Lists_Exported_1/0.All_Subjects")
# path_New.All.Subjects.List.Folder = paste0(path_OS, "Dropbox/Github/Rpkgs/ADNIprep/Subjects_Lists_Exported_3/0.All_Subjects")
# Export
# path_Export_Rda       = paste0(path_OS, "Dropbox/Github/Rpkgs/Papers___Data/Data___ADNI___RS.fMRI___Subjects.Lists")
#===============================================================================
require(dplyr)
require(tidyverse)
require(openxlsx)
require(oro.nifti)

list.files(paste0(path_OS, "Dropbox/Github/Rpkgs/ADNIprep/R"), full.names = T) %>% walk(source)
list.files(paste0(path_OS, "Dropbox/Github/Rpkgs/StatsR/R"), full.names = T) %>% walk(source)
list.files(paste0(path_OS, "Dropbox/Github/Rpkgs/refineR/R"), full.names = T) %>% walk(source)
#=============================================================================================





























