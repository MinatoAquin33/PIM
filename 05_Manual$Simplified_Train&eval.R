library(mlr3verse)
library(mlr3)
library(mlr3learners)
library(mlr3tuning)
library(future)

set.seed(42)




## S-baseline ------


task_train <- as_task_classif(dataset_train[, c(vars_Baseline_Simplified, "Abnormal_glucose_metabolism")], target = "Abnormal_glucose_metabolism", positive = "Yes")
task_test <- as_task_classif(dataset_test[, c(vars_Baseline_Simplified, "Abnormal_glucose_metabolism")], target = "Abnormal_glucose_metabolism", positive = "Yes")


learner_Simplified_Baseline <- lrn("classif.lightgbm")
learner_Simplified_Baseline$predict_type <- "prob"


param_set <- ps(
  num_leaves = p_int(2, 255),
  max_depth = p_int(-1, 20), 
  learning_rate = p_dbl(0.0001, 0.3), 
  num_iterations = p_int(1, 5000), 
  min_data_in_leaf = p_int(1, 5000), 
  lambda_l1 = p_int(1, 100),
  lambda_l2 = p_int(1, 100),
  feature_fraction = p_dbl(0.01, 1)
)


tuner <- tnr("mbo") 
instance_Simplified_Baseline <- TuningInstanceBatchSingleCrit$new(
  task = task_train,
  learner = learner_Simplified_Baseline,
  resampling = rsmp("cv", folds = 10), 
  measure = msr("classif.acc"), 
  search_space = param_set,
  terminator = trm("stagnation", iters = 10, threshold = 0)
)


future::plan("multisession", workers = 15)
tuner$optimize(instance_Simplified_Baseline)
future::plan(sequential)


learner_Simplified_Baseline$param_set$values <- instance_Simplified_Baseline$result_learner_param_vals



learner_Simplified_Baseline$train(task_train)


prediction_Simplified_Baseline <- learner_Simplified_Baseline$predict(task_test)






## Manual ------


task_train <- as_task_classif(dataset_train[, c(vars_Manual, "Abnormal_glucose_metabolism")], target = "Abnormal_glucose_metabolism", positive = "Yes")
task_test <- as_task_classif(dataset_test[, c(vars_Manual, "Abnormal_glucose_metabolism")], target = "Abnormal_glucose_metabolism", positive = "Yes")



learner_Manual <- lrn("classif.lightgbm")
learner_Manual$predict_type <- "prob"


param_set <- ps(
  num_leaves = p_int(2, 255),
  max_depth = p_int(-1, 20), 
  learning_rate = p_dbl(0.0001, 0.3), 
  num_iterations = p_int(1, 5000), 
  min_data_in_leaf = p_int(1, 5000), 
  lambda_l1 = p_int(1, 100),
  lambda_l2 = p_int(1, 100),
  feature_fraction = p_dbl(0.01, 1)
)


tuner <- tnr("mbo") 
instance_Manual <- TuningInstanceBatchSingleCrit$new(
  task = task_train,
  learner = learner_Manual,
  resampling = rsmp("cv", folds = 10), 
  measure = msr("classif.acc"), 
  search_space = param_set,
  terminator = trm("stagnation", iters = 10, threshold = 0)
)


future::plan("multisession", workers = 15)
tuner$optimize(instance_Manual)
future::plan(sequential)



learner_Manual$param_set$values <- instance_Manual$result_learner_param_vals


learner_Manual$train(task_train)


prediction_Manual <- learner_Manual$predict(task_test)






## S-Manual ------


task_train <- as_task_classif(dataset_train[, c(vars_Manual_Simplified, "Abnormal_glucose_metabolism")], target = "Abnormal_glucose_metabolism", positive = "Yes")
task_test <- as_task_classif(dataset_test[, c(vars_Manual_Simplified, "Abnormal_glucose_metabolism")], target = "Abnormal_glucose_metabolism", positive = "Yes")



learner_Simplified_Manual <- lrn("classif.lightgbm")
learner_Simplified_Manual$predict_type <- "prob"


param_set <- ps(
  num_leaves = p_int(2, 255),
  max_depth = p_int(-1, 20), 
  learning_rate = p_dbl(0.0001, 0.3),
  num_iterations = p_int(1, 5000), 
  min_data_in_leaf = p_int(1, 5000), 
  lambda_l1 = p_int(1, 100),
  lambda_l2 = p_int(1, 100),
  feature_fraction = p_dbl(0.01, 1)
)


tuner <- tnr("mbo") 
instance_Simplified_Manual <- TuningInstanceBatchSingleCrit$new(
  task = task_train,
  learner = learner_Simplified_Baseline,
  resampling = rsmp("cv", folds = 10), 
  measure = msr("classif.acc"), 
  search_space = param_set,
  terminator = trm("stagnation", iters = 10, threshold = 0)
)


future::plan("multisession", workers = 15)
tuner$optimize(instance_Simplified_Manual)
future::plan(sequential)



learner_Simplified_Manual$param_set$values <- instance_Simplified_Manual$result_learner_param_vals


learner_Simplified_Manual$train(task_train)


prediction_Simplified_Manual <- learner_Simplified_Manual$predict(task_test)







## 性能测试 ------

metrics2<- 
  data.frame(
    Model = "Baseline Model",
    AUC = prediction_rf_baseline$score(msr("classif.auc")),
    Sensitivity = prediction_rf_baseline$score(msr("classif.tpr")),
    Specificity = prediction_rf_baseline$score(msr("classif.tnr")),
    Accuracy = prediction_rf_baseline$score(msr("classif.acc"))
  )
plotlist2 <- list()
plotlist2$baseline <- autoplot(prediction_rf_baseline, type = "roc")



metrics2<- 
  rbind(metrics2,
        data.frame(
          Model = "Simplified Baseline Model",
          AUC = prediction_Simplified_Baseline$score(msr("classif.auc")),
          Sensitivity = prediction_Simplified_Baseline$score(msr("classif.tpr")),
          Specificity = prediction_Simplified_Baseline$score(msr("classif.tnr")),
          Accuracy = prediction_Simplified_Baseline$score(msr("classif.acc"))
        ))
plotlist2$Simplified_Baseline <- autoplot(prediction_Simplified_Baseline, type = "roc")



metrics2<- 
  rbind(metrics2,
        data.frame(
          Model = "Manual Model",
          AUC = prediction_Manual$score(msr("classif.auc")),
          Sensitivity = prediction_Manual$score(msr("classif.tpr")),
          Specificity = prediction_Manual$score(msr("classif.tnr")),
          Accuracy = prediction_Manual$score(msr("classif.acc"))
        ))
plotlist2$Manual <- autoplot(prediction_Manual, type = "roc")



metrics2<- 
  rbind(metrics2,
        data.frame(
          Model = "Simplified Manual Model",
          AUC = prediction_Simplified_Manual$score(msr("classif.auc")),
          Sensitivity = prediction_Simplified_Manual$score(msr("classif.tpr")),
          Specificity = prediction_Simplified_Manual$score(msr("classif.tnr")),
          Accuracy = prediction_Simplified_Manual$score(msr("classif.acc"))
        ))
plotlist2$Simplified_Manual <- autoplot(prediction_Simplified_Manual, type = "roc")


# 绘图 ------------------


# 绘制条形图
library(ggplot2)

metrics_long2 <- reshape2::melt(metrics2, id.vars = "Model")
barplot2 <- 
  ggplot(metrics_long2, aes(x = variable, y = value, fill = Model)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Performance of LightGBM Models", x = "variable", y = "Value") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

barplot2


ggsave(
  filename = "Paper/plots_raw/barplot2.tiff",   
  plot = barplot2,             
  device = "tiff",              
  dpi = 600,                     
  width = 19,                     
  height = 13,                   
  units = "cm",                  
  compression = "lzw"          
)


AUC_2 <- 
  ggplot() +
  geom_line(data = plotlist2$baseline$data, aes(x = x, y = y, color = "Baseline Model", 
                                                                     
  )) +
  geom_line(data = plotlist2$Manual$data, aes(x = x, y = y, color = "Manual Model", 
                                                                      
  )) +
  geom_line(data = plotlist2$Simplified_Baseline$data, aes(x = x, y = y, color = "Simplified Baseline Model", 
                                                  
  )) +
  geom_line(data = plotlist2$Simplified_Manual$data, aes(x = x, y = y, color = "Simplified Manual Model", 
                                                           
  )) +
  
  scale_color_manual(
    values = c(
      "Baseline Model" = "#F8766D",
      "Manual Model"= "#7CAE00",
      "Simplified Baseline Model" = "#00BFC4",
      "Simplified Manual Model" = "#C77CFF"
    )
  ) +
  labs(color = "Model",
       title = "ROC of LightGBM Models" ,
       x = "1-Specificity",
       y = "Sensitivity") +
  
  

  annotate("text", x = 1, y = 0, 
           label = paste("AUC:", round(prediction_rf_baseline$score(msr("classif.auc")), 3)), color = "#F8766D", vjust = -4, hjust=1) +
  annotate("text", x = 1, y = 0, 
           label = paste("AUC:", round(prediction_Manual$score(msr("classif.auc")), 3)), color = "#7CAE00", vjust = -2.5, hjust=1) +
  annotate("text", x = 1, y = 0, 
           label = paste("AUC:", round(prediction_Simplified_Baseline$score(msr("classif.auc")), 3)), color = "#00BFC4", vjust = -1, hjust=1) +
  annotate("text", x = 1, y = 0, 
           label = paste("AUC:", round(prediction_Simplified_Manual$score(msr("classif.auc")), 3)), color = "#C77CFF", vjust = 0.5, hjust=1) +
  
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

AUC_2


ggsave(
  filename = "Paper/plots_raw/AUC2.tiff",   
  plot = AUC_2,              
  device = "tiff",              
  dpi = 600,                     
  width = 19,                     
  height = 13,                   
  units = "cm",                 
  compression = "lzw"            
)

library(patchwork)
(barplot2/AUC_2)

ggsave(
  filename = "Paper/plots_raw/Lgbm_models_performance.tiff",   
  plot = (barplot2/AUC_2),            
  device = "tiff",              
  dpi = 600,                    
  width = 19,                   
  height = 24,                
  units = "cm",              
  compression = "lzw"       
)
