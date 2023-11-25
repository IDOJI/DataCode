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
install_packages = function(packages, load=TRUE) {
  # load : load the packages after installation?
  for(pkg in packages) {
    if (!require(pkg, character.only = TRUE)) {
      install.packages(pkg)
    }
    
    if(load){
      library(pkg, character.only = TRUE)
    }
  }
}
install_packages(c("tidyverse", "dplyr", "clipr", "fda", "tidyr", "stringr"))
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
# Subjects List
path_Subjects = list.files(path_ADNI, full.names = TRUE, pattern = "Subjects.Lists") %>% 
  list.files(., full.names = TRUE) %>%
  grep("Subjects_Lists_Exported$", ., value = TRUE) %>% 
  list.files(., full.names = TRUE) %>% 
  grep("Final$", ., value = TRUE) %>% 
  list.files(., full.names = TRUE) %>% 
  grep("list.csv$", ., value  =TRUE)
# FC
path_FC = list.files(path_ADNI, "Connectivity", full.names = T)
path_FC_Matrix = list.files(path_FC, "___Matrix", full.names = T) %>% grep("Matrix___", ., value = T, invert = T)
path_FC_Graph = list.files(path_FC, "GraphMeasures", full.names = T)
# FDA
path_FD = list.files(path_ADNI, full.names = T, pattern = "Functional.Data")
path_Euclidean = list.files(path_FD, pattern = "Euclidean", full.names=TRUE)
path_FPCA = list.files(path_Euclidean, pattern = "FPCA", full.names=TRUE)
#=============================================================================================








#===============================================================================
# Path files
#===============================================================================
path_FC_Matrix_Pipelines = list.files(path_FC_Matrix, full.names = T)
Pipelines = basename_sans_ext(path_FC_Matrix_Pipelines)
path_Files_For_Each_Pipeline = lapply(path_FC_Matrix_Pipelines, list.files, full.names=T) %>% setNames(Pipelines)










#===============================================================================
# Extract RID
#===============================================================================
RID_1 = path_Files_For_Each_Pipeline[[1]] %>% 
  basename_sans_ext() %>% 
  gsub("^(RID_\\d+).*", "\\1", .)


RID_2 = path_Files_For_Each_Pipeline[[2]] %>% 
  basename_sans_ext() %>% 
  gsub("^(RID_\\d+).*", "\\1", .)





#===============================================================================
# Loading files 
#===============================================================================
FunImgARCWSF = lapply(path_Files_For_Each_Pipeline[[1]], readRDS) %>% setNames(RID_1)
FunImgARglobalCWSF = lapply(path_Files_For_Each_Pipeline[[2]], readRDS) %>% setNames(RID_2)




#===============================================================================
# Save
#===============================================================================
saveRDS(FunImgARCWSF, paste0(path_FC_Matrix, "/FunImgARCWSF.rds"))
saveRDS(FunImgARglobalCWSF, paste0(path_FC_Matrix, "/FunImgARglobalCWSF.rds"))





#===============================================================================
# Check the results
#===============================================================================
test = lapply(list.files(path_FC_Matrix, full.names=T), readRDS)






















