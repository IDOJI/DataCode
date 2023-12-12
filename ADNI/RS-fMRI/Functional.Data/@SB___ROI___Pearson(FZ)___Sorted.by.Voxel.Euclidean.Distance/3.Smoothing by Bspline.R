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


path_Smoothing = list.files(path_Euclidean, pattern = "Smoothing", full.names = T)







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
                        path_Export = paste0(path_Smoothing, "/", ith_Pipeline, "___", jth_Subject_List_Name), 
                        file.name = paste0(names(kth_y_Combined)[n], "___", kth_Brain_Name))  
      })
    }
  }
}









#===============================================================================
# Combining Results
#===============================================================================
path_Smoothed_Folders = list.files(path_Smoothing, full.names = T)
Names_Smoothed_Folders = path_Smoothed_Folders %>% basename_sans_ext()

for(i in seq_along(path_Smoothed_Folders)){
  tictoc::tic()
  pattern.vec = c("Train", "Test")
  
  Results = sapply(pattern.vec, function(kth_pattern){
    
    
    ith_Files = list.files(path_Smoothed_Folders[i], full.names = T, pattern = kth_pattern) %>% 
      grep("\\.rds$", ., value = T)
    
    ith_Files_Names = ith_Files %>% 
      basename_sans_ext() %>% 
      gsub(pattern = "Train___", replacement = "", x = .)
    
    ith_Combined = lapply(ith_Files, readRDS) %>% setNames(ith_Files_Names)
    
    saveRDS(ith_Combined, file = paste0(path_Smoothing, "/", Names_Smoothed_Folders[i], "___", kth_pattern, ".rds"))
    
  })
  tictoc::toc()
}
















