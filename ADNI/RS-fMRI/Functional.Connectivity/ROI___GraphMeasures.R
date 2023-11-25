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
# path_OS = "C:/Users/lleii/"
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
setwd(paste0(path_OS, "Dropbox/Github/Papers/Papers___Wrting/Papers___Writing___PCC FDA/Data"))
getwd()










#===============================================================================
# Subjects list
#===============================================================================
Subjects = read.csv(paste0(path_OS, "Dropbox/Data/#ADNI___RS.fMRI___Subjects.Lists/Subjects_Lists_Exported/Final/[Final_Selected]_Subjects_list.csv"))
Subjects_SB = Subjects[Subjects$NFQ___BAND.TYPE=="SB",]







#===============================================================================
# path & files list
#===============================================================================
# Data names
Name_Data_1 = "Voxelwise___AAL3___FunImgARCWSF"
Name_Data_2 = "Voxelwise___AAL3___FunImgARglobalCWSF"

# Data path
path_Data_1 = paste0(path_OS, "Dropbox/Data/ADNI___RS.fMRI___BOLD.Signals/", Name_Data_1, "___Raw")
path_Data_2 = paste0(path_OS, "DropboxData/ADNI___RS.fMRI___BOLD.Signals/", Name_Data_2, "___Raw")
path_Data =c(path_Data_1, path_Data_2)


# Exporting path & name
Name_Export_Data_1 = paste0(Name_Data_1, "___Static___Pearson___FisherZ___Median.FC")
Name_Export_Data_2 = paste0(Name_Data_2, "___Static___Pearson___FisherZ___Median.FC")
path_Export = paste0(path_OS, "Dropbox/Data/ADNI___RS.fMRI___Functional.Connectivity/")


# Data list
path_List_Data_1 = list.files(path_Data_1, full.names = T)
path_List_Data_2 = list.files(path_Data_2, full.names = T) # global





#===============================================================================
# Computing static FC for each region
#===============================================================================
#===========
# non-
#===========
pattern = "(RID_[0-9]+___BOLD.Signals___Voxelwise___AAL3___FunImgARCWSF)"
Median_FC_1.list = lapply(path_List_Data_1, function(path_ith_VoxelwiseBoldSignals){
  # Extract the desired string using regular expressions
  extracted_string = regmatches(path_ith_VoxelwiseBoldSignals, regexpr(pattern, path_ith_VoxelwiseBoldSignals))

  ith_FC_Results = RS.fMRI_5_Euclidean.Distance___Voxelwise.BOLD.Signals(ith_VoxelwiseBoldSignals = readRDS(path_ith_VoxelwiseBoldSignals),
                                                                         file.name = paste0(extracted_string, "___Median.FC.by.Euclidean.Dist"),
                                                                         path_Export = paste0(path_Export, Name_Export_Data_1))
  return(ith_FC_Results)
})




#===========
# global
#===========
pattern = "(RID_[0-9]+___BOLD.Signals___Voxelwise___AAL3___FunImgARglobalCWSF)"
Median_FC_2.list = lapply(path_List_Data_2, function(path_ith_VoxelwiseBoldSignals){
  # Extract the desired string using regular expressions
  extracted_string = regmatches(path_ith_VoxelwiseBoldSignals, regexpr(pattern, path_ith_VoxelwiseBoldSignals))
  ith_FC_Results = RS.fMRI_5_Euclidean.Distance___Voxelwise.BOLD.Signals(readRDS(path_ith_VoxelwiseBoldSignals),
                                                                         file.name = paste0(extracted_string, "___Median.FC.by.Euclidean.Dist"),
                                                                         path_Export = paste0(path_Export, Name_Export_Data_2))
  return(ith_FC_Results)
})









#===============================================================================
# Combining Data
#===============================================================================
path_Exported_Data_1 = paste0(path_Export, Name_Export_Data_1)
path_Exported_Data_2 = paste0(path_Export, Name_Export_Data_2)

path_Exported_Data_1_Files = list.files(path_Exported_Data_1, full.names=T, pattern="Median.FC")
path_Exported_Data_2_Files = list.files(path_Exported_Data_2, full.names=T, pattern="Median.FC")


#============
# non global
#============
Median_FC_1.list = list()
Median_FC_1_RowCoordinates.list = list()
Median_FC_1_ColCoordinates.list = list()

Median_FC_1_.list = lapply(seq_along(path_Exported_Data_1_Files), function(i){
  print(i)
  y = readRDS(path_Exported_Data_1_Files[i])
  Median_FC_1.list <<- c(Median_FC_1.list, list(y[[1]]))
  Median_FC_1_RowCoordinates.list <<- c(Median_FC_1_RowCoordinates.list, list(y[[2]]))
  Median_FC_1_ColCoordinates.list <<- c(Median_FC_1_ColCoordinates.list, list(y[[3]]))
})


y[[1]] %>% dim
y[[1]][1,2]
y$row_Coordiantes[1,1]
head(y$row_Coordiantes)
head(y$col_Coordiantes)













