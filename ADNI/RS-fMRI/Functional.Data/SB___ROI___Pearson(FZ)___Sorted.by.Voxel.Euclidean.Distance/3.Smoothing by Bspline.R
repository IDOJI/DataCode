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
require(tidyr)
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
path_Euclidean = list.files(path_FD, pattern = "Euclidean", full.names=TRUE)
#=============================================================================================








#===============================================================================
# Path
#===============================================================================
path_Subjects = list.files(path_Euclidean, pattern = "Subjects", full.names = TRUE) %>% list.files(., full.names=TRUE)
Names_Subjects = path_Subjects %>% basename_sans_ext


path_SortedFC = path_Euclidean %>% 
  list.files(., pattern = "Sorted", full.names = T) %>% 
  list.files(., pattern = "FunImgAR", full.names = T) %>% 
  grep("\\.rds$", ., value = T)
Names_SortedFC = path_SortedFC %>% basename_sans_ext()







#===============================================================================
# Load Subjects List
#===============================================================================
Subjects_List = lapply(path_Subjects, readRDS) %>% setNames(Names_Subjects)







#===============================================================================
# Load FC Data
#===============================================================================
SortedFC.list = lapply(path_SortedFC, readRDS) %>% setNames(Names_SortedFC)







#===============================================================================
# Select FC for each Subject List
#===============================================================================
Pipelines = c("FunImgARCWSF", "FunImgARglobalCWSF")
for(i in seq_along(SortedFC.list)){
  
  ith_SortedFC.list = SortedFC.list[[i]]
  
  ith_Name_SortedFC = Names_SortedFC[i]
  
  ith_Pipeline = Pipelines[i]
  
  
  for(j in seq_along(Subjects_List)){
    
    jth_Subject_List_Name = Names_Subjects[j]
    
    jth_Subject_List = Subjects_List[[j]]
    
    jth_RID_Train = jth_Subject_List$Train_X$RID
    
    jth_RID_Test = jth_Subject_List$Test_X$RID
    
    
    for(k in seq_along(ith_SortedFC.list)){
      
      kth_Brain_Name = names(ith_SortedFC.list)[k]
      
      kth_Brain = ith_SortedFC.list[[k]]
      kth_Brain = kth_Brain[complete.cases(kth_Brain),]
      
      
      kth_x = kth_Brain[,1]
      
      kth_y = kth_Brain[,-1]
      
      kth_y_Train = kth_y[, colnames(kth_y) %in% jth_RID_Train]
      kth_y_Test = kth_y[, colnames(kth_y) %in% jth_RID_Test]
      kth_y_Combined = list(Train = kth_y_Train, Test = kth_y_Test)
      
      # Smoothing : Train
      Results = lapply(seq_along(kth_y_Combined), function(n){
        FDA___Smoothing(Bspline = list(y = kth_y_Combined[[n]],
                                       x = kth_x,
                                       range_vals = c(min(kth_x), max(kth_x)),
                                       nbasis = NULL,
                                       norder = 4,
                                       breaks = kth_x,
                                       labmdas =  exp(seq(-5, -4, 0.1)),
                                       m_int2Lfd = 2,
                                       argvals = kth_x), 
                        path_Export = paste0(path_Euclidean, "/Smoothing/", ith_Pipeline, "___", jth_Subject_List_Name), 
                        file.name = paste0(names(kth_y_Combined)[n], "___", kth_Brain_Name))  
      })
    }
  }
}








  




  




# #===============================================================================
# # Smoothing : FunImgARCWSF
# #===============================================================================
# File_Names = paste0(, names(Subjects_List))
# 
# Smoothing_1.list = lapply(seq_along(Subjects_List), function(k){
#   Smoothing_Function(RID = Subjects_List[[k]]$RID,
#                      FC = Data_1, 
#                      path_Export = path_Data_SB_FDA_Euclidean, 
#                      File_Name = File_Names[k],
#                      lambdas =) %>% suppressWarnings()
# })
# 
# 
# 
# 
# #===============================================================================
# # Smoothing : FunImgARglobalCWSF
# #===============================================================================
# File_Names = paste0("FunImgARglobalCWSF___", names(Subjects_List))
# 
# Smoothing_2.list = lapply(seq_along(Subjects_List), function(k){
#   Smoothing_Function(RID = Subjects_List[[k]]$RID,
#                      FC = Data_2, 
#                      path_Export = path_Data_SB_FDA_Euclidean, 
#                      File_Name = File_Names[k],
#                      lambdas = exp(seq(-5, -4, 0.1))) %>% suppressWarnings()
# })



















#===============================================================================
# Combining Results
#===============================================================================
path_Smoothing = list.files(path_Data_SB_FDA_Euclidean, full.names = T, pattern = "Smoothing")
path_Smoothing_Folders = path_Smoothing %>% list.files(full.names=T)
Folders = path_Smoothing %>% list.files(full.names=F) %>% tools::file_path_sans_ext()

for(n in 1:length(Folders)){
  
  path_nth_Each_Brain_Regions = list.files(path_Smoothing_Folders[n], full.name=T, pattern = "\\.rds")
  
  nth_Each_Brain_Regions = list.files(path_Smoothing_Folders[n], full.name=F, pattern = "\\.rds") %>% tools::file_path_sans_ext()
  
  nth_Smoothed_Data = lapply(path_nth_Each_Brain_Regions, readRDS) %>% setNames(nth_Each_Brain_Regions)
  
  saveRDS(nth_Smoothed_Data, paste0(path_Smoothing, "/" , Folders[n], ".rds"))
}






















