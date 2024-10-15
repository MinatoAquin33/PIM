library(iml)
library(ggplot2)
predictor_Simplified_Manual <- Predictor$new(learner_Simplified_Manual, data = dataset_train[, c(vars_Manual_Simplified)], y = dataset_train$Abnormal_glucose_metabolism)
shapley <- Shapley$new(predictor_Simplified_Manual, x.interest = dataset_test[295, c(vars_Manual_Simplified)])

shapvalue <- as.data.frame(shapley$results[,"feature"])
shapvalue$value <- sub(".*=", "", shapley$results$feature.value)
colnames(shapvalue) <- c("feature","value")


# 定义一个函数来还原 Z 标准化值
restore_value <- function(z_score, feature) {
  if (feature %in% names(train_means) && feature %in% names(train_sds)) {
    mean_value <- train_means[[feature]]
    sd_value <- train_sds[[feature]]
    original_value <- z_score * sd_value + mean_value
    
    # 根据特征名称设置保留的小数位数
    if (feature == "Age") {
      return(round(original_value, 0))  # Age 保留 0 位小数
    } else {
      return(round(original_value, 1))  # 其他特征保留 1 位小数
    }
  } else {
    return(NA)  # 特征不存在
  }
}

# 应用函数到 shapvalue 的 value 列
shapvalue$original_value <- mapply(restore_value, 
                                   z_score = as.numeric(as.character(shapvalue$value)), 
                                   feature = shapvalue$feature)

shapvalue$feature <- c(
  "Age",
  "Marital status",
  "Education level",
  "Poverty Income Ratio",
  "Race",
  "Gender",
  "Smoking Status",
  "Alcohol Consumption",
  "BMI",
  "WC", 
  "Physical activity",
  "Hypertension",
  "Hyperlipidemia",
  "Age",
  "Marital status",
  "Education level",
  "Poverty Income Ratio",
  "Race",
  "Gender",
  "Smoking Status",
  "Alcohol Consumption",
  "BMI",
  "WC", 
  "Physical activity",
  "Hypertension",
  "Hyperlipidemia"

)


# 初始化 fvalue 列
shapvalue$fvalue <- NA


for (i in 1:nrow(shapvalue)) {
  if (!is.na(as.numeric((shapvalue$value[i])))) {
    # 如果 value 是数值，使用 original_value
    shapvalue$fvalue[i] <- paste0(shapvalue$feature[i], "=", shapvalue$original_value[i])
  } else {
    # 否则，使用原始的 value
    shapvalue$fvalue[i] <- paste0(shapvalue$feature[i], "=", shapvalue$value[i])
  }
}




shapley$results$feature.value <- shapvalue$fvalue
shapley$results <- shapley$results[shapley$results$class=="Yes",]
shapley$results$class <- factor(rep("Prediabetes", length(shapley$results$class)))

shap_plot <- 
  plot(shapley)

shap_plot <- 
  shap_plot +
  ggtitle(paste0("Prediabete probability = ",round(shapley$y.hat.interest$Yes,3)))

ggsave(
  filename = "Paper/plots_raw/SHAP.tiff",   # 文件名
  plot = shap_plot,              # ggplot对象
  device = "tiff",               # 保存格式
  dpi = 600,                     # 分辨率，通常300 dpi适合出版
  width = 19,                     # 图像宽度，单位为英寸，可根据期刊要求调整
  height = 10,                    # 图像高度，单位为英寸，可根据期刊要求调整
  units = "cm",                  # 单位，可选："in", "cm", "mm"
  compression = "lzw"            # 压缩格式，LZW通常是推荐的无损压缩
)
shap_plot
