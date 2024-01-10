# @ Basic Setting ============================================================================================================================================================================
## @ Loading packages ======================================================================================
# rm(list=ls())
path_OS = "C:/Users/lleii/" # Windows
path_OS = "/Users/Ido/" # Mac
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
install_packages(c("tidyverse", "dplyr", "clipr", "fda", "tidyr", "stringr", "gridExtra", "zoo"))
## @ Loading functions ============================================================================================================================================================================
path_Dropbox = paste0(path_OS, "Dropbox")
path_GitHub = list.files(path_Dropbox, pattern = "Github", full.names = T)
path_Rpkgs = list.files(path_GitHub, pattern = "Rpkgs", full.names = T)

Rpkgs = c("ADNIprep", "StatsR", "refineR", "dimR")

Load = sapply(Rpkgs, function(y){
  list.files(paste0(path_Rpkgs, "/", y, "/R"), full.names = T) %>% walk(source) 
})
## @ Setting path ============================================================================================================================================================================
# path : Data
path_Data = paste0(path_Dropbox, "/Papers Data")
# path : ADNI
path_ADNI = list.files(path_Data, full.names = T, pattern = "ADNI")
# path : Subjects List
path_Subjects = list.files(path_ADNI, "Subjects.Lists", full.names = TRUE) %>% 
  list.files(., full.names = TRUE) %>%
  grep("Subjects_Lists_Exported$", ., value = TRUE) %>% 
  list.files(., full.names = TRUE) %>% 
  grep("Final$", ., value = TRUE) %>% 
  list.files(., full.names = TRUE) %>% 
  grep("list.csv$", ., value  =TRUE)
# path : BOLD
path_BOLD = list.files(path_ADNI, "BOLD", full.names = T)
# path : FC
path_FC = list.files(path_ADNI, "Connectivity", full.names = T)
path_FC_Matrix = list.files(path_FC, "___Matrix", full.names = T) %>% 
  grep("Matrix___", ., value = T, invert = T)
path_FC_Matrix_Raw = path_FC_Matrix %>% grep("Pearson___", ., value=T)
path_FC_Matrix_FZ = path_FC_Matrix %>% grep("Pearson___", ., value=T, invert=T)
path_FC_Graph = list.files(path_FC, "GraphMeasures", full.names = T)
path_FC_CCA = list.files(path_FC, "_CCA", full.names = T)
# path : FDA
path_FD = list.files(path_ADNI, full.names = T, pattern = "Functional.Data")
path_Euclidean = list.files(path_FD, pattern = "Euclidean", full.names=TRUE)
path_FPCA = list.files(path_Euclidean, pattern = "FPCA", full.names=TRUE)
# path : papers
path_Papers = list.files(path_Data, "Papers", full.names = T)
# path : FDA
path_Papers_FDA = list.files(path_Papers, "Euclidean", full.names = T)
path_Papers_FDA_Data = list.files(path_Papers_FDA, "Data", full.names = T)








#===============================================================================
# WD
#===============================================================================
# path_clip()
# setwd("/Users/Ido/Library/CloudStorage/Dropbox/Github/Papers/Papers___Wrting/___Papers___진행중/Data")
setwd(paste0(path_OS, "Dropbox/Data"))
getwd()









#===============================================================================
# Path
#===============================================================================
path_Data_Subject = paste0(path_OS, "Dropbox/Data/#ADNI___RS.fMRI___Subjects.Lists")
path_Data_BOLD = paste0(path_OS, "Dropbox/Data/ADNI___RS.fMRI___BOLD.Signals")
path_Data_FC = paste0(path_OS, "Dropbox/Data/ADNI___RS.fMRI___Functional.Connectivity")
path_Data_FDA = paste0(path_OS, "Dropbox/Data/ADNI___RS.fMRI___SB___Functional.Data")
# path Export
path_Data_FDA_Export = paste0(path_Data_FDA, "/ROI___AAL3___Static___Pearson___FisherZ___Combined.by.Each.Region___Sorted.by.Voxel.Euclidean.Distance")









#===============================================================================
# Load Data
#===============================================================================
# Subjects
Subjects = read.csv(paste0(path_Data_Subject, "/Subjects_Lists_Exported/Final/[Final_Selected]_Subjects_list.csv"))
Subjects_SB = Subjects[Subjects$NFQ___BAND.TYPE=="SB",]

# FC combined
path_FC_1 = paste0(path_Data_FC, "/ROI___AAL3___FunImgARCWSF___Static___Pearson___FisherZ___Combined.by.Each.Region") %>% list.files(full.names=T)
path_FC_2 = paste0(path_Data_FC, "/ROI___AAL3___FunImgARglobalCWSF___Static___Pearson___FisherZ___Combined.by.Each.Region") %>% list.files(full.names=T)
FC_1 = readRDS(path_FC_1)
FC_2 = readRDS(path_FC_2)









#===============================================================================
# Center dist
#===============================================================================
# Data names
Name_Data_1 = "Voxelwise___AAL3___FunImgARCWSF"

# Data path
path_Data_1 = paste0(path_Data_BOLD, "/", Name_Data_1, "___Raw")

# Data path list
path_List_Data_1 = list.files(path_Data_1, full.names = T)

# Load Data
BOLD = readRDS(path_List_Data_1[1])

# Compute median & Export
path_Export = path_Data_FDA_Export
# Center_Dist.mat = RS.fMRI_5_Euclidean.Distance___Voxelwise.BOLD.Signals___Center.Coordinates(BOLD, path_Export)


# Center distance
path_Center_Dist.mat = list.files(path_Data_FDA_Export, full.names=T, pattern = "Center")
Center_Dist.mat = readRDS(path_Center_Dist.mat)

















#===============================================================================
# Sorting FC by dist
#===============================================================================
RS.fMRI_5_Euclidean.Distance___Voxelwise.BOLD.Signals___Sorted.FC.by.Dist.for.Each.Region = function(Brain_Region, FC.list, Center_Dist.mat, path_Export, preprocessing_pipeline, figure=F){
  #===============================================================================
  # path Export
  #===============================================================================
  path_Export_2 = paste0(path_Export, "/", preprocessing_pipeline)
  dir.create(path_Export_2, F)
  
  
  
  
  #===============================================================================
  # Sort FC by Distance for each brain region
  #===============================================================================
  FC_Sorted.list = lapply(Brain_Region, function(kth_Region){
    k = which(Brain_Region == kth_Region)
    
    kth_Region_FC = FC.list[[k]]
    kth_Dist = Center_Dist.mat[,k]
    kth_Order = order(kth_Dist)
    
    kth_Dist_Sorted = kth_Dist[kth_Order]
    kth_Region_FC_Sorted = kth_Region_FC[kth_Order,]
    
    
    if(figure){
      png(filename = paste0(path_Export_2, "/", kth_Region, ".png"), bg="white", width = 5000, height = 1000)
      # Set the character expansion for the main title
      par(cex.main = 2)  # Adjust the value (2) to change the font size
      # png(filename = paste0("C:/Users/lleii/Desktop","/test", ".png"), bg="white", width = 5000, height = 1000)
      matplot(x= kth_Dist_Sorted, y = kth_Region_FC_Sorted, type = "l", main = paste0("Region : ", kth_Region))
      # Draw vertical lines
      abline(v = kth_Dist_Sorted, col = "red", lty = 2)  # Adjust the color and line type as needed
      dev.off()
    }
    
    
    cat("\n", crayon::red(kth_Region), crayon::green("is done!"),"\n")
    
    kth_Region_FC_Sorted_with_Mean.Dist = cbind(Mean_Dist = kth_Dist_Sorted, kth_Region_FC_Sorted)
    kth_Region_FC_Sorted_with_Mean.Dist
  })
  names(FC_Sorted.list) = Brain_Region
  
  
  
  
  
  #===============================================================================
  # Export
  #===============================================================================
  dir.create(path_Export, F)
  saveRDS(FC_Sorted.list, paste0(path_Export_2, ".rds"))
  
  
  
  return(FC_Sorted.list)
}

#==============
# non global
#==============
Brain_Region = names(FC_1)
FC.list = FC_1
path_Export = path_Data_FDA_Export
preprocessing_pipeline = "FunImgARCWSF"
FC_1_Sorted.list = RS.fMRI_5_Euclidean.Distance___Voxelwise.BOLD.Signals___Sorted.FC.by.Dist.for.Each.Region(Brain_Region, FC.list, Center_Dist.mat, path_Export, preprocessing_pipeline, T)


#==============
# global
#==============
Brain_Region = names(FC_2)
FC.list = FC_2
path_Export = path_Data_FDA_Export
preprocessing_pipeline = "FunImgARglobalCWSF"
FC_2_Sorted.list = RS.fMRI_5_Euclidean.Distance___Voxelwise.BOLD.Signals___Sorted.FC.by.Dist.for.Each.Region(Brain_Region, FC.list, Center_Dist.mat, path_Export, preprocessing_pipeline, T)






