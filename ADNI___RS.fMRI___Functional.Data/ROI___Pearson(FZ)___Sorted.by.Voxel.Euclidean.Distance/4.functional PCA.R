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
path_Data_SB_FDA = paste0(path_OS, "Dropbox/Data/ADNI___RS.fMRI___SB___Functional.Data")
path_Data_SB_FDA_Euclidean = list.files(path_Data_SB_FDA, full.names = T, pattern = "Euclidean")



path_Data_SB_FDA_Euclidean_Smoothing = list.files(path_Data_SB_FDA_Euclidean, full.names=T, pattern = "Smoothing") %>% list.files(full.names=T, pattern = "\\.rds$")
path_Data_SB_FDA_Euclidean__FPCA = paste0(path_Data_SB_FDA_Euclidean, "/FPCA")
path_Data_SB_FDA_Euclidean__FPCA_Scores = paste0(path_Data_SB_FDA_Euclidean, "/FPCA_Scores_With_Group_Nums")


# 긴 경로용
# path_Data_SB_FDA_Euclidean__FPCA = paste0(path_Data_SB_FDA, "/FPCA")
# path_Data_SB_FDA_Euclidean__FPCA_Scores = paste0(path_Data_SB_FDA, "/FPCA_Scores_With_Group_Nums")







#===============================================================================
# Loading smoothing Data
#===============================================================================
Smoothing.list = lapply(path_Data_SB_FDA_Euclidean_Smoothing, readRDS) %>% setNames( basename(path_Data_SB_FDA_Euclidean_Smoothing))









#===============================================================================
# functional PCA
#===============================================================================
FPCA.list = lapply(seq_along(Smoothing.list), function(k){
  
  kth_Smoothing = Smoothing.list[[k]]
  
  kth_Regions = names(kth_Smoothing)
  
  tictoc::tic()
  
  kth_FPCA = lapply(seq_along(kth_Smoothing), function(i){
    FDA___fPCA(fdobj = kth_Smoothing[[i]]$smoothing$fd, 
               threshold = 0.9, 
               # path_Export = paste0(path_Data_SB_FDA_Euclidean__FPCA, "/", basename(path_Data_SB_FDA_Euclidean_Smoothing)[k] %>% tools::file_path_sans_ext()), 
               path_Export = paste0(path_Data_SB_FDA_Euclidean__FPCA, "/", basename(path_Data_SB_FDA_Euclidean_Smoothing)[k] %>% tools::file_path_sans_ext()), 
               file.name = kth_Regions[i])
    
  }) %>% setNames(kth_Regions)
  
  saveRDS(kth_FPCA, paste0(path_Data_SB_FDA_Euclidean, "/FPCA/FPCA___", basename(path_Data_SB_FDA_Euclidean_Smoothing)[k]))
  
  tictoc::toc()
  
  cat("\n", paste0(crayon::bgRed(basename(path_Data_SB_FDA_Euclidean_Smoothing)[k]), crayon::green(" is done!")) ,"\n")
  return(kth_FPCA)
})











#===============================================================================
# Exporting fPCA scores
#===============================================================================
# Define a function
Extrac_fPCA_Scores_with_GroupNums = function(FPCA, path_Export, File.Name){
  # Export directory
  fs::dir_create(path_Export, recurse = T)
  
  # Group lasso를 위한 numbering
  FPCA_Scores_GroupNum = c()
  
  # Extract FPCA scores
  FPCA_Scores = lapply(seq_along(FPCA), function(i){
    
    ith_Region = FPCA[[i]]
    
    ith_PC_Scores = ith_Region$scores %>% as.data.frame %>% as_tibble
    
    names(ith_PC_Scores) = paste0(names(FPCA)[i], "___", 1:ncol(ith_PC_Scores))
    
    FPCA_Scores_GroupNum <<- c(FPCA_Scores_GroupNum, rep(i, times = ncol(ith_PC_Scores)))
    
    return(ith_PC_Scores)
  })
  
  FPCA_Scores = do.call(cbind, FPCA_Scores) %>% as_tibble
  
  FPCA_Combined = list(fPCA_Scores = FPCA_Scores, Features_Group_Nums = FPCA_Scores_GroupNum)
  
  saveRDS(FPCA_Combined, file = paste0(path_Export, "/", File.Name))
  
  return(FPCA_Combined)
}



# Extracting
FPCA_Scores = mapply(Extrac_fPCA_Scores_with_GroupNums, 
                     FPCA = FPCA.list, 
                     File.Name = basename(path_Data_SB_FDA_Euclidean_Smoothing), 
                     path_Export = path_Data_SB_FDA_Euclidean__FPCA_Scores, 
                     SIMPLIFY = FALSE)







  


