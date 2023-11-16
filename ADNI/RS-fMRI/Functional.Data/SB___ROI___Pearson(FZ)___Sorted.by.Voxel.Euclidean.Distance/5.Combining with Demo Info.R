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
path_Subjects = list.files(path_ADNI, full.names = TRUE, pattern = "Subjects.Lists") %>% 
  list.files(., full.names = TRUE) %>%
  grep("Subjects_Lists_Exported$", ., value = TRUE) %>% 
  list.files(., full.names = TRUE) %>% 
  grep("Final$", ., value = TRUE) %>% 
  list.files(., full.names = TRUE) %>% 
  grep("list.csv$", ., value  =TRUE)
path_FD = list.files(path_ADNI, full.names = T, pattern = "Functional.Data")
path_Euclidean = list.files(path_FD, pattern = "Euclidean", full.names=TRUE)
path_FPCA = list.files(path_Euclidean, pattern = "FPCA", full.names=TRUE)
#=============================================================================================











#===============================================================================
# Path
#===============================================================================
# PC Scores
path_Scores = list.files(path_FPCA, full.names=T, pattern = "Scores")
path_Scores_Train = path_Scores %>% grep("Train", ., value = T)
path_Scores_Test = path_Scores %>% grep("Test", ., value = T)

Names_Scores_Pipeline = path_Scores_Test %>% 
  basename_sans_ext() %>% 
  str_extract(., "(?<=Scores___).*(?=___Subjects)")
Names_Scores_Subjects = path_Scores_Test %>% 
  basename_sans_ext() %>% 
  str_extract(., "(?<=CWSF___).*(?=___Test)")
Names_Scores_Files = paste0(Names_Scores_Pipeline, "___", Names_Scores_Subjects)



# Subject
path_Subjects = list.files(path_Euclidean, pattern = "Subjects", full.names=T) %>% list.files(full.names = T)
path_Subjects = c(path_Subjects, path_Subjects)
# Names_Subjects = path_Subjects %>% basename_sans_ext()










#===============================================================================
# Combine Train, Test and Subjects
#===============================================================================
# Load subject
Subjects.list = lapply(path_Subjects, readRDS) %>% setNames(Names_Scores_Files)

# Load Train
Scores_Train = lapply(path_Scores_Train, readRDS) %>% setNames(Names_Scores_Files)

# Load Test
Scores_Test = lapply(path_Scores_Test, readRDS) %>% setNames(Names_Scores_Files)


# Export
Combined = lapply(seq_along(Scores_Train), function(k){
  
  kth_Subjects.list = Subjects.list[[k]]
  kth_Scores_Train = Scores_Train[[k]]
  kth_Scores_Test = Scores_Test[[k]]
  
  # Extract scores
  kth_Subjects.list$Train_X = cbind(kth_Subjects.list$Train_X, kth_Scores_Train$fPCA_Scores)
  kth_Subjects.list$Test_X = cbind(kth_Subjects.list$Test_X, kth_Scores_Test$Scores)
  
  # Extract Features Groups
  kth_Subjects.list$Train_X_FeaturesGroupsNums = kth_Scores_Train$Features_Group_Nums
  kth_Subjects.list$Test_X_FeaturesGroupsNums = kth_Scores_Test$Features_Group_Nums
  
  # Exporting
  saveRDS(kth_Subjects.list, file = paste0())
})






