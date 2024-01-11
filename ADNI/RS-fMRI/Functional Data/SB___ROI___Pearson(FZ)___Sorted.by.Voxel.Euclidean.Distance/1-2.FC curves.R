# @ Selected Brain Regions =================================================================================================
Selected_Regions = c("ACC_sub_L", 
                     "Caudate_R", 
                     "Cingulate_Mid_L", 
                     "Cingulate_Mid_R", 
                     "Cingulate_Post_L", 
                     "Cingulate_Post_R",
                     "Frontal_Inf_Tri_L",
                     "OFClat_L", 
                     "OFCmed_R", 
                     "Parietal_Inf_R",
                     "Supp_Motor_Area_R", 
                     "Temporal_Mid_L", 
                     "Temporal_Mid_R", 
                     "Thal_IL_L")

AAL3 = readRDS("/Users/Ido/Library/CloudStorage/Dropbox/Github/Rpkgs/ADNIprep/Preprocessing Tools/RS-fMRI/AAL3/AAL3 atlas.rds")

Selected_Brain_Regions = AAL3 %>% dplyr::filter(Abbreviation %in% Selected_Regions)

Selected_Brain_Regions$Description[10] = "the Right Inferior Parietal Gyrus, Excluding Supramarginal and Angular Gyri"




# @ FC data ========================================================================================================
# Load
## global
Sorted_FC_by_Dist = readRDS("/Users/Ido/Library/CloudStorage/Dropbox/Papers Data/ADNI/RS-fMRI/Functional Data/SB___ROI___Pearson(FZ)___Sorted.by.Voxel.Euclidean.Distance/1.Sorted_FC/Sorted.FC.by.Dist___FunImgARglobalCWSF.rds")


# Subset by selected regions
Selected_Sorted_FC_by_Dist = Sorted_FC_by_Dist[names(Sorted_FC_by_Dist) %in% Selected_Regions]







# @ Subjects ========================================================================================================
# Diagnosis info
Subjects_ADCN = readRDS("/Users/Ido/Library/CloudStorage/Dropbox/Papers Data/ADNI/RS-fMRI/Functional Data/SB___ROI___Pearson(FZ)___Sorted.by.Voxel.Euclidean.Distance/2.Subjects_List_Splitted/Subjects_ADCN_NA.rds")

# Data combining (train + test)
Subjects_ADCN_X = rbind(Subjects_ADCN$Train_X, Subjects_ADCN$Test_X)
Subjects_ADCN_y = rbind(Subjects_ADCN$Train_y, Subjects_ADCN$Test_y)

## New data
Subjects_ADCN_2 = cbind(Subjects_ADCN_X, Subjects_ADCN_y)

## Extract RID
RID_CN = Subjects_ADCN_2 %>% dplyr::filter(DEMO___DIAGNOSIS_NEW=="CN") %>% dplyr::select(RID) %>% unlist %>% unname
RID_AD = Subjects_ADCN_2 %>% dplyr::filter(DEMO___DIAGNOSIS_NEW=="AD") %>% dplyr::select(RID) %>% unlist %>% unname






# @ 1.Matplot ==============================================================================================================================================================
for(k in 1:length(Selected_Sorted_FC_by_Dist)){
  ## @ 2.Subset kth Data =========================================================================================================================
  # Subset kth data (rm NA)
  kth_Data = Selected_Sorted_FC_by_Dist[[k]] %>% as.data.frame %>% na.omit
  
  
  # x : dist
  kth_Dist = kth_Data$Mean_Dist
  
  
  # AD & its mean
  kth_Subjects_AD = kth_Data[,colnames(kth_Data) %in% RID_AD]
  kth_Subjects_AD_Mean = kth_Subjects_AD %>% rowMeans()
  
  
  # CN & its mean
  kth_Subjects_CN = kth_Data[,colnames(kth_Data) %in% RID_CN]
  kth_Subjects_CN_Mean = kth_Subjects_CN %>% rowMeans()
  
  
  
  
  
  
  
  
  ## @ 2.데이터프레임을 길게 만들어 열을 세로로 정렬 ==================================================================================================================================
  # All subjects
  kth_Subjects_CN_Long = cbind(Dist = kth_Dist, kth_Subjects_CN) %>% 
    gather(key = "RID", value = "FC", -Dist) %>% 
    mutate(Diagnosis = "CN")
  kth_Subjects_AD_Long = cbind(Dist = kth_Dist, kth_Subjects_AD) %>% 
    gather(key = "RID", value = "FC", -Dist) %>% 
    mutate(Diagnosis = "AD")
  
  
  # Mean vector
  kth_Subjects_CN_Mean_Long = data.frame(Dist = kth_Dist, RID = NA, FC = kth_Subjects_CN_Mean, Diagnosis = "Mean CN")
  kth_Subjects_AD_Mean_Long = data.frame(Dist = kth_Dist, RID = NA, FC = kth_Subjects_AD_Mean, Diagnosis = "Mean AD")
  
  
  # Combine 
  kth_Subjects_Combined_Long = rbind(kth_Subjects_CN_Long, kth_Subjects_AD_Long, kth_Subjects_CN_Mean_Long, kth_Subjects_AD_Mean_Long)
  
  
  
  # Brain region
  kth_BrainRegion = Selected_Brain_Regions$Description[k]
  
  
  
  
  
  
  
  
  
  ## @ 2.plotting  ==============================================================================================================================================================
  # Colour
  # kth_Color_AD = rep("red", ncol(kth_Subjects_AD))
  # kth_Color_CN = rep("blue", ncol(kth_Subjects_CN))
  # kth_Color_Combined = c(kth_Color_AD, kth_Color_CN)
  
  
  # 그래프 생성
  # p = ggplot(kth_Subjects_Combined_Long, aes(x = Dist, y = FC, group = RID, colour = Diagnosis)) +
  #   geom_line() +
  #   scale_color_manual(
  #     values = c("CN" = "#81DAF5", "AD" = "#F78181", "Mean CN" = "blue", "Mean AD" = "red"),
  #     breaks = c("CN", "AD", "Mean CN", "Mean AD")
  #   ) +
  #   labs(color = "Legend Title")  # 레전드 타이틀 추가
  p = ggplot() +
    geom_line(data = kth_Subjects_CN_Long, aes(x = Dist, y = FC, group = RID, color = "CN")) +
    geom_line(data = kth_Subjects_AD_Long, aes(x = Dist, y = FC, group = RID, color = "AD")) +
    # Mean
    geom_line(data = kth_Subjects_CN_Mean_Long, aes(x = Dist, y = FC, color = "Mean CN"), size = 1.5) +
    geom_line(data = kth_Subjects_AD_Mean_Long, aes(x = Dist, y = FC, color = "Mean AD"), size = 1.5) +
    scale_color_manual(
      values = c("CN" = "#81DAF5", "AD" = rgb(255, 204, 0, maxColorValue = 255), "Mean CN" = "blue", "Mean AD" = "red"),
      breaks = c("CN", "AD", "Mean CN", "Mean AD")
    ) +
    labs(color = "Colors") +  # 레전드 타이틀 추가
    theme(legend.title = element_text(size = 20, face = "bold"), 
          legend.text = element_text(size = 15, face = "bold")) +  # 텍스트 크기와 굵기 설정
    xlab("Distance") +
    ylab("Functional Connectivity") +
    ggtitle(kth_BrainRegion) +
    theme(
      axis.title.x = element_text(size = 20, face = "bold"),  # x축 레이블 크기와 색상 설정
      axis.title.y = element_text(size = 20, face = "bold"),  # y축 레이블 크기와 색상 설정
      plot.title = element_text(size = 35, color = "black", hjust = 0.5, face = "bold")  # 그래프 제목 크기, 색상 및 위치 설정
    )
  
  
  
  
  
  
  
  
  ## @ 2.Exporting ==============================================================================================================================================================
  path_FunctionalData_FCcurves = paste0(path_FunctionalData, "/Sorted Functional Connectivity by Voxel Euclidean Distance___SB/1.FC curves")
  ggsave(paste0(path_FunctionalData_FCcurves, "/", fit_length(k, 3), "th___", kth_BrainRegion, ".png"), 
         plot = p, width = 15, height = 8, dpi = 300, limitsize = F)
  cat("\n", crayon::red(kth_BrainRegion), crayon::green(" is exported !"), "\n")
}














