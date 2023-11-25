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
requier(tidyr)
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
path_Smoothed = list.files(path_Euclidean, full.names = T, pattern = "Smoothing") %>% 
  list.files(., full.names = T)

# Subjects List Names
Names_Subjects = path_Smoothed %>% grep("\\.rds$", ., value = T, invert = T) %>% basename_sans_ext()

# Smoothed files
path_Smoothed_Files = path_Smoothed %>% grep("\\.rds$", ., value = T)
path_Smoothed_Files_Train = path_Smoothed_Files %>% grep("Train", ., value = T)
path_Smoothed_Files_Test = path_Smoothed_Files %>% grep("Test", ., value = T)
Names_Smoothed_Files = path_Smoothed_Files %>% 
  basename_sans_ext() %>% 
  grep("Train", ., value = T) %>% 
  gsub("___Train", "", .)


# Save
path_FPCA = list.files(path_Euclidean, pattern = "FPCA", full.names=T)







#===============================================================================
# Loading smoothing Data
#===============================================================================
Smoothing_Train.list = lapply(path_Smoothed_Files_Train, readRDS) %>% setNames(basename_sans_ext(path_Smoothed_Files_Train))
Smoothing_Test.list = lapply(path_Smoothed_Files_Test, readRDS) %>% setNames(basename_sans_ext(path_Smoothed_Files_Test))





#===============================================================================
# Functional fPCA for Train data
#===============================================================================
Names_Smoothing_Train = names(Smoothing_Train.list)

# (@Completed) compute FPCA 
# FPCA_Train = lapply(seq_along(Smoothing_Train.list), function(k){
#   
#   kth_Smoothing = Smoothing_Train.list[[k]]
#   
#   kth_Regions = names(kth_Smoothing)
#   
#   tictoc::tic()
# 
#   kth_FPCA = lapply(seq_along(kth_Smoothing), function(i){
#     
#     FDA___fPCA(fdobj = kth_Smoothing[[i]]$smoothing$fd, 
#                threshold = 0.9,
#                path_Export = paste0(path_FPCA, "/", Names_Smoothing_Train[k]), 
#                file.name = kth_Regions[i])
#     
#   }) %>% setNames(kth_Regions)
#   
#   saveRDS(kth_FPCA, paste0(path_FPCA, "/FPCA___", Names_Smoothing_Train[k], ".rds"))
#   
#   
#   tictoc::toc()
#   
# 
#   cat("\n", paste0(crayon::bgRed(basename(Names_Smoothing_Train[k])), crayon::green(" is done!")) ,"\n")
# })


# Load data
path_FPCA_Train = list.files(path_FPCA, full.name = T, pattern = "\\.rds$") %>% 
  grep("Scores", ., value = T, invert = T)
Names_FPCA_Train = path_FPCA_Train %>% basename_sans_ext()
FPCA_Train.list = lapply(path_FPCA_Train, readRDS) %>% setNames(Names_FPCA_Train)








#===============================================================================
# @ Exporting fPCA scores for Train
#===============================================================================
# Extract_fPCA_Scores_with_GroupNums = function(FPCA, path_Export, File.Name){
#   # Export directory
#   fs::dir_create(path_Export, recurse = T)
#   
#   # Group lasso를 위한 numbering
#   FPCA_Scores_GroupNum = c()
#   
#   # Extract FPCA scores
#   FPCA_Scores = lapply(seq_along(FPCA), function(i){
#     
#     ith_Region = FPCA[[i]]
#     
#     ith_PC_Scores = ith_Region$scores %>% as.data.frame
#     
#     names(ith_PC_Scores) = paste0(names(FPCA)[i], "___", 1:ncol(ith_PC_Scores))
#     
#     FPCA_Scores_GroupNum <<- c(FPCA_Scores_GroupNum, rep(i, times = ncol(ith_PC_Scores)))
#     
#     return(ith_PC_Scores)
#   })
#   
#   FPCA_Scores = do.call(cbind, FPCA_Scores)
#   
#   FPCA_Combined = list(fPCA_Scores = FPCA_Scores, Features_Group_Nums = FPCA_Scores_GroupNum)
#   
#   saveRDS(FPCA_Combined, file = paste0(path_Export, "/Scores___", File.Name, "___Train.rds"))
# 
# }
# 
# 
# 
# # Extracting scores from Train
# FPCA_Scores = mapply(Extract_fPCA_Scores_with_GroupNums, 
#                      FPCA = FPCA_Train.list, 
#                      File.Name = Names_Smoothed_Files, 
#                      path_Export = path_FPCA, 
#                      SIMPLIFY = FALSE)










#===============================================================================
# Exporting fPCA scores for Test
#===============================================================================
for(i in seq_along(FPCA_Train.list)){
  tictoc::tic()
  ith_FPCA_Train = FPCA_Train.list[[i]]
  ith_Smoothing_Test = Smoothing_Test.list[[i]]
  
  ith_BrainRegion = names(ith_FPCA_Train)
  
  ith_Scores_Group_Num = c()
  
  # Computer inner product for each Brain Region
  ith_Scores_Test = lapply(seq_along(ith_FPCA_Train), function(j){
    # FPCA results from Train  
    ijth_Region_FCPA_Train = ith_FPCA_Train[[j]]
  
    # centering before eigenvalue decomposition
    ijth_Region_Smoothing_FD_Centered_Test = center.fd(ith_Smoothing_Test[[j]]$smoothing$fd) 
    
    ijth_Scores_Test = fda::inprod(fdobj1 = ijth_Region_Smoothing_FD_Centered_Test, 
                                   fdobj2 = ijth_Region_FCPA_Train$harmonics)
    
    colnames(ijth_Scores_Test) = paste0(ith_BrainRegion[j], "___",  1:ncol(ijth_Scores_Test))
    rownames(ijth_Scores_Test) = colnames(ijth_Region_Smoothing_Test$smoothing$y)
    ith_Scores_Group_Num <<- c(ith_Scores_Group_Num, rep(j, times = ncol(ijth_Scores_Test)))
    
    return(ijth_Scores_Test)
  }) %>% setNames(names(ith_FPCA_Train)) %>% do.call(cbind, .) %>% as.data.frame()
  
  
  saveRDS(list(Scores = ith_Scores_Test, Features_Group_Nums = ith_Scores_Group_Num), 
               file = paste0(path_FPCA, "/Scores___", Names_Smoothed_Files[i], "___Test.rds"))
  tictoc::toc()
}








# path_Smooothing = list.files(path_Euclidean, pattern = "Smoothing", full.names=T) %>%
#   list.files(pattern = "FunImgARCWSF___Subjects_ADCN___Train.rds", full.names=T)
# 
# Smoothing_Train = path_Smooothing %>% readRDS
# ACC_Smoothing = Smoothing_Train$ACC_pre_L$smoothing
# 
# 
# 
# path_FPCA_New = list.files(path_FPCA, pattern = "FPCA___FunImgARCWSF___Subjects_ADCN___Train.rds", full.names=T)
# FPCA_new = path_FPCA_New %>% readRDS
# ACC_FPCA = FPCA_new$ACC_pre_L
# 
# 
# center.fd(functionalData)
# Scores = inprod(center.fd(ACC_Smoothing$fd), ACC_FPCA$harmonics) %>% as.data.frame
# cbind(Scores[1,] %>% unlist, ACC_FPCA$scores[1,])








