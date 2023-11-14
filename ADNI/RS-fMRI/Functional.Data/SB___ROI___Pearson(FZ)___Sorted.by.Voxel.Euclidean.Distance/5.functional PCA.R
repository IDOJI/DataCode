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
path_Subjects = list.files(path_ADNI, full.names = T, pattern = "Subjects.Lists")
path_FD = list.files(path_ADNI, full.names = T, pattern = "Functional.Data")
#=============================================================================================














#===============================================================================
# Path
#===============================================================================
path_FD_Euclidean = list.files(path_FD, full.names = T, pattern = "Euclidean")
path_FD_Smoothing = list.files(path_FD_Euclidean, full.names = T, pattern = "Smoothing")
path_FD_Subjects = list.files(path_FD_Euclidean, full.names = T, pattern = "Splitted")



# Files
path_Smoothing = list.files(path_FD_Smoothing, full.names=T, pattern = "\\.rds$")
path_Subjects = list.files(path_FD_Subjects, full.names=T, pattern = "\\.rds$")

# Names
Names_Smoothing = basename(path_Smoothing) %>% 
  tools::file_path_sans_ext()
Names_Subjects = basename(path_Subjects) %>% 
  tools::file_path_sans_ext() %>% 
  gsub(pattern = "___Splitted", replacement = "")



# Save
path_FPCA = paste0(path_FD_Euclidean, "/FPCA")
path_FPCA_Scores = paste0(path_FD_Euclidean, "/FPCA_Scores_With_Group_Nums")







#===============================================================================
# Loading smoothing Data
#===============================================================================
Smoothing.list = lapply(path_Smoothing, readRDS) %>% setNames(Names_Smoothing)
Subjects.list = lapply(path_Subjects, readRDS) %>% setNames(Names_Subjects)









#===============================================================================
# Split B-spline data by Train & Test
#===============================================================================
Splitted_Bspline.list = list()
for(k in 1:length(Smoothing.list)){
  
  which_Sub_Ind = which(Names_Subjects == sub(".*___", "", Names_Smoothing[k]))
  kth_Subjects = Subjects.list[[which_Sub_Ind]]
  
  # Extract RID
  kth_Train_RID = kth_Subjects$Train_X$RID
  kth_Test_RID = kth_Subjects$Test_X$RID
  
  # Split B-spline for each region
  lapply(Smoothing.list[[k]], function(y){
    y$smoothing$gcv
  })
  
  
}














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

path_FPCA_RDS = list.files(path_Data_SB_FDA_Euclidean__FPCA, pattern = "\\.rds$", full.names=T)

FPCA.list = lapply(path_FPCA_RDS, readRDS)

names(FPCA.list) = basename(path_FPCA_RDS) %>% tools::file_path_sans_ext()









#===============================================================================
# Exporting fPCA scores
#===============================================================================
# Define a function
Extract_fPCA_Scores_with_GroupNums = function(FPCA, path_Export, File.Name){
  # Export directory
  fs::dir_create(path_Export, recurse = T)
  
  # Group lasso를 위한 numbering
  FPCA_Scores_GroupNum = c()
  
  # Extract FPCA scores
  FPCA_Scores = lapply(seq_along(FPCA), function(i){
    
    ith_Region = FPCA[[i]]
    
    ith_PC_Scores = ith_Region$scores %>% as.data.frame
    
    names(ith_PC_Scores) = paste0(names(FPCA)[i], "___", 1:ncol(ith_PC_Scores))
    
    FPCA_Scores_GroupNum <<- c(FPCA_Scores_GroupNum, rep(i, times = ncol(ith_PC_Scores)))
    
    return(ith_PC_Scores)
  })
  
  FPCA_Scores = do.call(cbind, FPCA_Scores)
  
  FPCA_Combined = list(fPCA_Scores = FPCA_Scores, Features_Group_Nums = FPCA_Scores_GroupNum)
  
  saveRDS(FPCA_Combined, file = paste0(path_Export, "/", File.Name, ".rds"))
  
  return(FPCA_Combined)
}



# Extracting
FPCA_Scores = mapply(Extract_fPCA_Scores_with_GroupNums, 
                     FPCA = FPCA.list, 
                     File.Name = basename(path_Data_SB_FDA_Euclidean_Smoothing) %>% tools::file_path_sans_ext(), 
                     path_Export = path_Data_SB_FDA_Euclidean__FPCA_Scores, 
                     SIMPLIFY = FALSE)


