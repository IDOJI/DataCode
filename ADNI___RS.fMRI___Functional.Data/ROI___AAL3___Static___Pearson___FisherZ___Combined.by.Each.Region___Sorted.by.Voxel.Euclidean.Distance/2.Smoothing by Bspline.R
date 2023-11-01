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
path_Data_SB_FDA_Euclidean = list.files(path_Data_SB_FDA, full.names = T, pattern = "")








#===============================================================================
# Load Data
#===============================================================================
# Subjects
Subjects = read.csv(paste0(path_Data_Subject, "/Subjects_Lists_Exported/Final/[Final_Selected]_Subjects_list.csv"))
Subjects_SB = Subjects[Subjects$NFQ___BAND.TYPE=="SB",]

# FC combined
path_Data_1 = paste0(path_Data_SB_FDA, "/ROI___AAL3___Static___Pearson___FisherZ___Combined.by.Each.Region___Sorted.by.Voxel.Euclidean.Distance") %>% list.files(full.names=T, pattern="FunImgARCWSF.rds")
path_Data_2 = paste0(path_Data_SB_FDA, "/ROI___AAL3___Static___Pearson___FisherZ___Combined.by.Each.Region___Sorted.by.Voxel.Euclidean.Distance") %>% list.files(full.names=T, pattern="FunImgARglobalCWSF.rds")
Data_1 = readRDS(path_Data_1)
Data_2 = readRDS(path_Data_2)








#===============================================================================
# Smoothing
#===============================================================================
# Define a function
Smoothing_Function = function(Data, Preprocessing_Pipeline, path_Export){
  Brain_Regions = names(Data)
  
  Results.list = lapply(seq_along(Brain_Regions), function(i){
    # Raw data
    y_raw = Data[[i]][,-1]
    x_raw = Data[[i]][,1]
    
    
    # Exclude rows with all non-NA values
    y = y_raw[y_raw %>% complete.cases,]
    x = x_raw[x_raw %>% complete.cases]
    
    
    # Bspline Options
    Bspline = list(y = y,
                   x = x,
                   range_vals = c(min(x), max(x)),
                   nbasis = NULL,
                   norder = 4,
                   breaks = x,
                   labmdas = exp(-100:100),
                   m_int2Lfd = 2)
    
    
    # Smoothing
    FDA___Smoothing(Bspline = Bspline, 
                    path_Export = paste0(path_Export, "/Smoothing___", Preprocessing_Pipeline), 
                    file.name = Brain_Regions[i])
    
    
  }) %>% setNames(Brain_Regions)
  
  
  return(Results.list)
}








#----------------------------
# FunImgARCWSF
#----------------------------
# Smoothing_1 = Smoothing_Function(Data = Data_1, Preprocessing_Pipeline = "FunImgARCWSF", path_Export = path_Data_SB_FDA_Euclidean) %>% suppressWarnings()




#----------------------------
# FunImgARglobalCWSF
#----------------------------
# Smoothing_2 = Smoothing_Function(Data = Data_1, Preprocessing_Pipeline = "FunImgARglobalCWSF", path_Export = path_Data_SB_FDA_Euclidean) %>% suppressWarnings()



#----------------------------
# Loading Data
#----------------------------
path_Smoothing_Pipeline = list.files(path_Data_SB_FDA_Euclidean, full.names = T, pattern = "Smoothing")

path_Smoothing_1 = list.files(path_Smoothing_Pipeline[1], full.name=T, pattern = "\\.rds")
path_Smoothing_2 = list.files(path_Smoothing_Pipeline[2], full.name=T, pattern = "\\.rds")

Smoothing_1 = lapply(path_Smoothing_1, readRDS) %>% setNames(sub("\\.rds$", "", basename(path_Smoothing_1)))
Smoothing_2 = lapply(path_Smoothing_2, readRDS) %>% setNames(sub("\\.rds$", "", basename(path_Smoothing_2)))

# Save
saveRDS(Smoothing_1, file = paste0(path_Data_SB_FDA_Euclidean, "/Smoothing___FunImgARCWSF.rds"))
saveRDS(Smoothing_2, file = paste0(path_Data_SB_FDA_Euclidean, "/Smoothing___FunImgARglobalCWSF.rds"))

