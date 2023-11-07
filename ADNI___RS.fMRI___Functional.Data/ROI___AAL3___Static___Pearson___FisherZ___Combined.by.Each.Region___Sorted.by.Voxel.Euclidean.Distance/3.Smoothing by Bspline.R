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
path_Smoothing_Pipeline = list.files(path_Data_SB_FDA_Euclidean, full.names = T, pattern = "Smoothing")

path_Smoothing_1 = list.files(path_Smoothing_Pipeline[1], full.name=T, pattern = "\\.rds")
path_Smoothing_2 = list.files(path_Smoothing_Pipeline[2], full.name=T, pattern = "\\.rds")

Smoothing_1 = lapply(path_Smoothing_1, readRDS) %>% setNames(sub("\\.rds$", "", basename(path_Smoothing_1)))
Smoothing_2 = lapply(path_Smoothing_2, readRDS) %>% setNames(sub("\\.rds$", "", basename(path_Smoothing_2)))

# Save
saveRDS(Smoothing_1, file = paste0(path_Data_SB_FDA_Euclidean, "/Smoothing___FunImgARCWSF.rds"))
saveRDS(Smoothing_2, file = paste0(path_Data_SB_FDA_Euclidean, "/Smoothing___FunImgARglobalCWSF.rds"))
















