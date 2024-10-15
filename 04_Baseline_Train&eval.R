library(mlr3verse)
library(mlr3)
library(mlr3learners)
library(mlr3tuning)
library(future)

set.seed(42)
# 创建任务-------
task_train <- as_task_classif(dataset_train[, c(vars_Baseline,"Abnormal_glucose_metabolism")], target = "Abnormal_glucose_metabolism", positive = "Yes")
task_test <- as_task_classif(dataset_test[, c(vars_Baseline,"Abnormal_glucose_metabolism")], target = "Abnormal_glucose_metabolism", positive = "Yes")


# LR ------

learner_log_reg_baseline <- lrn("classif.log_reg")
learner_log_reg_baseline$predict_type <- "prob"


learner_log_reg_baseline$train(task_train)


prediction_log_reg_baseline <- learner_log_reg_baseline$predict(task_test)



# RF ---------



learner_rf_baseline <- lrn("classif.ranger")
learner_rf_baseline$predict_type <- "prob"


param_set <- ps(
  mtry = p_int(1, ncol(dataset_train[, vars_Baseline]) - 1), 
  min.node.size = p_int(1, 10),
  num.trees = p_int(1, 1000),
  max.depth = p_int(1, 20) 
)


tuner <- tnr("mbo")
instance_rf_baseline <- TuningInstanceBatchSingleCrit$new(
  task = task_train,
  learner = learner_rf_baseline,
  resampling = rsmp("cv", folds = 10),
  measure = msr("classif.acc"), 
  search_space = param_set,
  terminator = trm("stagnation", iters = 10, threshold = 0)
)


future::plan("multisession", workers = 15)
tuner$optimize(instance_rf_baseline)
future::plan(sequential)



learner_rf_baseline$param_set$values <- instance_rf_baseline$result_learner_param_vals


learner_rf_baseline$train(task_train)





prediction_rf_baseline <- learner_rf_baseline$predict(task_test)









# LGBM -------------



learner_lgbm_baseline <- lrn("classif.lightgbm")
learner_lgbm_baseline$predict_type <- "prob"


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
instance_lgbm_baseline <- TuningInstanceBatchSingleCrit$new(
  task = task_train,
  learner = learner_lgbm_baseline,
  resampling = rsmp("cv", folds = 10), 
  measure = msr("classif.acc"),
  search_space = param_set,
  terminator = trm("stagnation", iters = 10, threshold = 0)
)


future::plan("multisession", workers = 15)
tuner$optimize(instance_lgbm_baseline)
future::plan(sequential)



learner_lgbm_baseline$param_set$values <- instance_lgbm_baseline$result_learner_param_vals


learner_lgbm_baseline$train(task_train)




prediction_lgbm_baseline <- learner_lgbm_baseline$predict(task_test)





# SVM ------------


pipeline <- po("encode", method = "one-hot") %>>% lrn("classif.svm")
learner_svm_baseline <- as_learner(pipeline)
learner_svm_baseline$predict_type <- "prob"


param_set <- ps(
  classif.svm.cost = p_dbl(0.1, 10), # cost的范围
  classif.svm.gamma = p_dbl(0, 5)

)



tuner <- tnr("mbo") 
instance_svm_baseline <- TuningInstanceBatchSingleCrit$new(
  task = task_train,
  learner = learner_svm_baseline,
  resampling = rsmp("cv", folds = 10), 
  measure = msr("classif.acc"), 
  search_space = param_set,
  terminator = trm("stagnation", iters = 10, threshold = 0)
)


future::plan("multisession", workers = 15)
tuner$optimize(instance_svm_baseline)
future::plan(sequential)


best_params_svm_baseline <- instance_svm_baseline$result_learner_param_vals
best_params_svm_baseline$classif.svm.kernel <- "radial"
best_params_svm_baseline$classif.svm.type <- "C-classification"

learner_svm_baseline$param_set$values <- best_params_svm_baseline


learner_svm_baseline$train(task_train)


prediction_svm_baseline <- learner_svm_baseline$predict(task_test)




# XGBOOST --------


pipeline <- po("encode", method = "one-hot") %>>% lrn("classif.xgboost")
learner_xgboost_baseline <- as_learner(pipeline)
learner_xgboost_baseline$predict_type <- "prob"


param_set <- ps(
  classif.xgboost.eta = p_dbl(0.001, 0.3), 
  classif.xgboost.max_depth = p_int(1, 100), 
  classif.xgboost.subsample = p_dbl(0, 1), 
  classif.xgboost.colsample_bytree = p_dbl(0, 1), 
  classif.xgboost.gamma = p_dbl(0, 1),
  classif.xgboost.lambda = p_dbl(0, 10), 
  classif.xgboost.alpha = p_dbl(0, 10) 
)


tuner <- tnr("mbo") 
instance_xgboost_baseline <- TuningInstanceBatchSingleCrit$new(
  task = task_train,
  learner = learner_xgboost_baseline,
  resampling = rsmp("cv", folds = 10),
  measure = msr("classif.acc"), 
  search_space = param_set,
  terminator = trm("stagnation", iters = 10, threshold = 0)
)


future::plan("multisession", workers = 15)
tuner$optimize(instance_xgboost_baseline)
future::plan(sequential)




learner_xgboost_baseline$param_set$values <- instance_xgboost_baseline$result_learner_param_vals


learner_xgboost_baseline$train(task_train)


prediction_xgboost_baseline <- learner_xgboost_baseline$predict(task_test)





# 性能评估 ------------



metrics_df<- 
  data.frame(
    Model = "Logistic Regression",
    AUC = prediction_log_reg_baseline$score(msr("classif.auc")),
    Sensitivity = prediction_log_reg_baseline$score(msr("classif.tpr")),
    Specificity = prediction_log_reg_baseline$score(msr("classif.tnr")),
    Accuracy = prediction_log_reg_baseline$score(msr("classif.acc"))
  )
plotlist <- list()
plotlist$LR <- autoplot(prediction_log_reg_baseline, type = "roc")


metrics_df<- 
  rbind(metrics_df,
        data.frame(
          Model = "Random Forest",
          AUC = prediction_rf_baseline$score(msr("classif.auc")),
          Sensitivity = prediction_rf_baseline$score(msr("classif.tpr")),
          Specificity = prediction_rf_baseline$score(msr("classif.tnr")),
          Accuracy = prediction_rf_baseline$score(msr("classif.acc"))
        ))
plotlist$RF <- autoplot(prediction_rf_baseline, type = "roc")


metrics_df<- 
  rbind(metrics_df,
        data.frame(
          Model = "LightGBM",
          AUC = prediction_lgbm_baseline$score(msr("classif.auc")),
          Sensitivity = prediction_lgbm_baseline$score(msr("classif.tpr")),
          Specificity = prediction_lgbm_baseline$score(msr("classif.tnr")),
          Accuracy = prediction_lgbm_baseline$score(msr("classif.acc"))
        ))
plotlist$LGBM <- autoplot(prediction_lgbm_baseline, type = "roc")


metrics_df<- 
  rbind(metrics_df,
        data.frame(
          Model = "Support Vector Machine",
          AUC = prediction_svm_baseline$score(msr("classif.auc")),
          Sensitivity = prediction_svm_baseline$score(msr("classif.tpr")),
          Specificity = prediction_svm_baseline$score(msr("classif.tnr")),
          Accuracy = prediction_svm_baseline$score(msr("classif.acc"))
        ))
plotlist$SVM <- autoplot(prediction_svm_baseline, type = "roc")


metrics_df<- 
  rbind(metrics_df,
        data.frame(
          Model = "XGBoost",
          AUC = prediction_xgboost_baseline$score(msr("classif.auc")),
          Sensitivity = prediction_xgboost_baseline$score(msr("classif.tpr")),
          Specificity = prediction_xgboost_baseline$score(msr("classif.tnr")),
          Accuracy = prediction_xgboost_baseline$score(msr("classif.acc"))
        ))
plotlist$XGBoost <- autoplot(prediction_svm_baseline, type = "roc")
# 绘图 ------------------



library(ggplot2)

metrics_long <- reshape2::melt(metrics_df, id.vars = "Model")
metrics_long$Model <- factor(metrics_long$Model,levels = c("Logistic Regression", 
                                                           "Random Forest", 
                                                           "LightGBM", 
                                                           "Support Vector Machine", 
                                                           "XGBoost"))
barplot_baseline <- 
ggplot(metrics_long, aes(x = variable, y = value, fill = Model)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Performance of Baseline Models on Test Set", x = "variable", y = "Value") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

barplot_baseline


ggsave(
  filename = "Paper/plots_raw/barplot_baseline.tiff",  
  plot = barplot_baseline,            
  device = "tiff",             
  dpi = 600,                     
  width = 19,                    
  height = 13,                   
  units = "cm",                  
  compression = "lzw"            
)


AUC_baseline <- 
ggplot() +
  geom_line(data = plotlist$LR$data, aes(x = x, y = y, color = factor("Logistic Regression", 
                                                                      levels = c("Logistic Regression", 
                                                                                 "Random Forest", 
                                                                                 "LightGBM", 
                                                                                 "Support Vector Machine", 
                                                                                 "XGBoost"))
                                         )) +
  geom_line(data = plotlist$RF$data, aes(x = x, y = y, color = factor("Random Forest", 
                                                                      levels = c("Logistic Regression", 
                                                                                 "Random Forest", 
                                                                                 "LightGBM", 
                                                                                 "Support Vector Machine", 
                                                                                 "XGBoost"))
                                         )) +
  geom_line(data = plotlist$LGBM$data, aes(x = x, y = y, color = factor("LightGBM", 
                                                                        levels = c("Logistic Regression", 
                                                                                   "Random Forest", 
                                                                                   "LightGBM", 
                                                                                   "Support Vector Machine", 
                                                                                   "XGBoost"))
                                           )) +
  geom_line(data = plotlist$SVM$data, aes(x = x, y = y, color = factor("Support Vector Machine", 
                                                                       levels = c("Logistic Regression", 
                                                                                  "Random Forest", 
                                                                                  "LightGBM", 
                                                                                  "Support Vector Machine", 
                                                                                  "XGBoost"))
                                          )) +
  geom_line(data = plotlist$XGBoost$data, aes(x = x, y = y, color = factor("XGBoost", 
                                                                           levels = c("Logistic Regression", 
                                                                                      "Random Forest", 
                                                                                      "LightGBM", 
                                                                                      "Support Vector Machine", 
                                                                                      "XGBoost"))
                                              )) +
  
  scale_color_manual(
    values = c(
      "Logistic Regression" = "#F8766D",
      "Random Forest" = "#A3A500",
      "LightGBM" = "#00BF7D",
      "Support Vector Machine" = "#00B0F6",
      "XGBoost" = "#e76bf3"
    )
  ) +
  labs(color = "Model",
       title = "ROC of Baseline Models on Test Set",
       x = "1-Specificity",
       y = "Sensitivity") +

  
  
  annotate("text", x = 1, y = 0,
           label = paste("AUC:", round(prediction_log_reg_baseline$score(msr("classif.auc")), 3)), color = "#F8766D", vjust = -5.5, hjust= 1) +
  annotate("text", x = 1, y = 0, 
           label = paste("AUC:", round(prediction_rf_baseline$score(msr("classif.auc")), 3)), color = "#A3A500", vjust = -4, hjust=1) +
  annotate("text", x = 1, y = 0, 
           label = paste("AUC:", round(prediction_lgbm_baseline$score(msr("classif.auc")), 3)), color = "#00BF7D", vjust = -2.5, hjust=1) +
  annotate("text", x = 1, y = 0, 
           label = paste("AUC:", round(prediction_svm_baseline$score(msr("classif.auc")), 3)), color = "#00B0F6", vjust = -1, hjust=1) +
  annotate("text", x = 1, y = 0, 
           label = paste("AUC:", round(prediction_xgboost_baseline$score(msr("classif.auc")), 3)), color = "#e76bf3", vjust = 0.5, hjust=1) +
  
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

AUC_baseline


ggsave(
  filename = "Paper/plots_raw/AUC_baseline.tiff",  
  plot = AUC_baseline,             
  device = "tiff",              
  dpi = 600,                   
  width = 19,                     
  height = 13,                  
  units = "cm",                  
  compression = "lzw"            
)


library(patchwork)
(barplot_baseline/AUC_baseline)

ggsave(
  filename = "Paper/plots_raw/baseline_performance.tiff",   
  plot = (barplot_baseline/AUC_baseline),           
  device = "tiff",            
  dpi = 600,                    
  width = 19,                    
  height = 24,                   
  units = "cm",                
  compression = "lzw"           
)
rm(metrics_long)
