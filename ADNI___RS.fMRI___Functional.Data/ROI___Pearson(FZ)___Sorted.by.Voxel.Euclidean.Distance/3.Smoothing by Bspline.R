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
path_Save_All = paste0(path_Data_SB_FDA_Euclidean, "/", "All")
path_Save_ADMCI = paste0(path_Data_SB_FDA_Euclidean, "/", "ADMCI")
path_Save_ADCN = paste0(path_Data_SB_FDA_Euclidean, "/", "ADCN")
path_Save_MCICN = paste0(path_Data_SB_FDA_Euclidean, "/", "MCICN")









#===============================================================================
# Load Subjects
#===============================================================================
Subjects_List = readRDS(path_SubjectsList)










#===============================================================================
# Load FC Data
#===============================================================================
path_Data = list.files(path_Data_SB_FDA, full.names=T, pattern = "Euclidean") %>% 
  list.files(pattern = "Sorted", full.names = T) %>% 
  list.files(pattern = "\\.rds$", full.names = T)

Data_1 = readRDS(path_Data[1])
Data_2 = readRDS(path_Data[2])
RID_FC = Data_1[[1]] %>% colnames








#===============================================================================
# Smoothing function
#===============================================================================
# Define a function
Smoothing_Function = function(RID, FC, File_Name, lambdas, path_Export){
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
                    path_Export = paste0(path_Export, "/Smoothing/", File_Name), 
                    file.name = Brain_Regions[i])
    
    
  }) %>% setNames(Brain_Regions)
  
  
  return(Results.list)
}








#===============================================================================
# Smoothing : FunImgARCWSF
#===============================================================================
File_Names = paste0("FunImgARCWSF___", names(Subjects_List))

Smoothing_1.list = lapply(seq_along(Subjects_List), function(k){
  Smoothing_Function(RID = Subjects_List[[k]]$RID,
                     FC = Data_1, 
                     path_Export = path_Data_SB_FDA_Euclidean, 
                     File_Name = File_Names[k],
                     lambdas = exp(seq(-5, -4, 0.1))) %>% suppressWarnings()
})




#===============================================================================
# Smoothing : FunImgARglobalCWSF
#===============================================================================
File_Names = paste0("FunImgARglobalCWSF___", names(Subjects_List))

Smoothing_2.list = lapply(seq_along(Subjects_List), function(k){
  Smoothing_Function(RID = Subjects_List[[k]]$RID,
                     FC = Data_2, 
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
  
  nth_Each_Brain_Regions = list.files(path_Smoothing_Folders[n], full.name=F, pattern = "\\.rds") %>% tools::file_path_sans_ext()
  
  nth_Smoothed_Data = lapply(path_nth_Each_Brain_Regions, readRDS) %>% setNames(nth_Each_Brain_Regions)
  
  saveRDS(nth_Smoothed_Data, paste0(path_Smoothing, "/" , Folders[n], ".rds"))
}






















