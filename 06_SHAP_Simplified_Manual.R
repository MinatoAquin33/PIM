library(iml)
library(ggplot2)
predictor_Simplified_Manual <- Predictor$new(learner_Simplified_Manual, data = dataset_train[, c(vars_Manual_Simplified)], y = dataset_train$Abnormal_glucose_metabolism)
shapley <- Shapley$new(predictor_Simplified_Manual, x.interest = dataset_test[295, c(vars_Manual_Simplified)])

shapvalue <- as.data.frame(shapley$results[,"feature"])
shapvalue$value <- sub(".*=", "", shapley$results$feature.value)
colnames(shapvalue) <- c("feature","value")



restore_value <- function(z_score, feature) {
  if (feature %in% names(train_means) && feature %in% names(train_sds)) {
    mean_value <- train_means[[feature]]
    sd_value <- train_sds[[feature]]
    original_value <- z_score * sd_value + mean_value
    

    if (feature == "Age") {
      return(round(original_value, 0))  
    } else {
      return(round(original_value, 1))  
    }
  } else {
    return(NA)  
  }
}


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



shapvalue$fvalue <- NA


for (i in 1:nrow(shapvalue)) {
  if (!is.na(as.numeric((shapvalue$value[i])))) {
    
    shapvalue$fvalue[i] <- paste0(shapvalue$feature[i], "=", shapvalue$original_value[i])
  } else {
   
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
  filename = "Paper/plots_raw/SHAP.tiff",   
  plot = shap_plot,              
  device = "tiff",              
  dpi = 600,                    
  width = 19,                     
  height = 10,                   
  units = "cm",                 
  compression = "lzw"            
)
shap_plot
