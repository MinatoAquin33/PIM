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
# 1. 定义学习器
learner_log_reg_baseline <- lrn("classif.log_reg")
learner_log_reg_baseline$predict_type <- "prob"

# 2. 训练模型
learner_log_reg_baseline$train(task_train)

# 3. 进行预测
prediction_log_reg_baseline <- learner_log_reg_baseline$predict(task_test)



# RF ---------


# 2. 定义学习器
learner_rf_baseline <- lrn("classif.ranger")
learner_rf_baseline$predict_type <- "prob"

# 3. 设置超参数搜索空间
param_set <- ps(
  mtry = p_int(1, ncol(dataset_train[, vars_Baseline]) - 1), # mtry的范围
  min.node.size = p_int(1, 10), # 最小节点大小的范围
  num.trees = p_int(1, 1000), # 树的数量范围
  max.depth = p_int(1, 20) # 深度范围
)

# 4. 定义调优实例
tuner <- tnr("mbo") # 贝叶斯优化调优器
instance_rf_baseline <- TuningInstanceBatchSingleCrit$new(
  task = task_train,
  learner = learner_rf_baseline,
  resampling = rsmp("cv", folds = 10), # 交叉验证
  measure = msr("classif.acc"), # 优化指标
  search_space = param_set,
  terminator = trm("stagnation", iters = 10, threshold = 0)
)

# 5. 执行超参数调优
future::plan("multisession", workers = 15)
tuner$optimize(instance_rf_baseline)
future::plan(sequential)

# 6. 获取最佳参数

learner_rf_baseline$param_set$values <- instance_rf_baseline$result_learner_param_vals

# 7. 训练模型
learner_rf_baseline$train(task_train)





# 9. 进行预测
prediction_rf_baseline <- learner_rf_baseline$predict(task_test)


# 10. 计算性能指标，acc tpr tnr auc






# LGBM -------------


# 2. 定义学习器
learner_lgbm_baseline <- lrn("classif.lightgbm")
learner_lgbm_baseline$predict_type <- "prob"

# 3. 设置超参数搜索空间
param_set <- ps(
  num_leaves = p_int(2, 255), # 叶子数的范围
  max_depth = p_int(-1, 20), # 最大深度的范围，-1表示不限制
  learning_rate = p_dbl(0.0001, 0.3), # 学习率的范围
  num_iterations = p_int(1, 5000), # 树的数量范围
  min_data_in_leaf = p_int(1, 5000), 
  lambda_l1 = p_int(1, 100),
  lambda_l2 = p_int(1, 100),
  feature_fraction = p_dbl(0.01, 1)
)

# 4. 定义调优实例
tuner <- tnr("mbo") # 贝叶斯优化调优器
instance_lgbm_baseline <- TuningInstanceBatchSingleCrit$new(
  task = task_train,
  learner = learner_lgbm_baseline,
  resampling = rsmp("cv", folds = 10), # 交叉验证
  measure = msr("classif.acc"), # 优化指标
  search_space = param_set,
  terminator = trm("stagnation", iters = 10, threshold = 0)
)

# 5. 执行超参数调优
future::plan("multisession", workers = 15)
tuner$optimize(instance_lgbm_baseline)
future::plan(sequential)

# 6. 获取最佳参数

learner_lgbm_baseline$param_set$values <- instance_lgbm_baseline$result_learner_param_vals

# 7. 训练模型
learner_lgbm_baseline$train(task_train)



# 9. 进行预测
prediction_lgbm_baseline <- learner_lgbm_baseline$predict(task_test)

# 10. 计算性能指标，acc tpr tnr auc



# SVM ------------


# 2. 定义学习器
pipeline <- po("encode", method = "one-hot") %>>% lrn("classif.svm")
learner_svm_baseline <- as_learner(pipeline)
learner_svm_baseline$predict_type <- "prob"

# 3. 设置超参数搜索空间
param_set <- ps(
  classif.svm.cost = p_dbl(0.1, 10), # cost的范围
  classif.svm.gamma = p_dbl(0, 5)

)


# 4. 定义调优实例
tuner <- tnr("mbo") # 贝叶斯优化调优器
instance_svm_baseline <- TuningInstanceBatchSingleCrit$new(
  task = task_train,
  learner = learner_svm_baseline,
  resampling = rsmp("cv", folds = 10), # 交叉验证
  measure = msr("classif.acc"), # 优化指标
  search_space = param_set,
  terminator = trm("stagnation", iters = 10, threshold = 0)
)

# # 5. 执行超参数调优
# future::plan("multisession", workers = 15)
# tuner$optimize(instance_svm_baseline)
# future::plan(sequential)
# 
# # 6. 获取最佳参数
# best_params_svm_baseline <- instance_svm_baseline$result_learner_param_vals
# best_params_svm_baseline$classif.svm.kernel <- "radial"
# best_params_svm_baseline$classif.svm.type <- "C-classification"
# 
# learner_svm_baseline$param_set$values <- best_params_svm_baseline

# 7. 训练模型
learner_svm_baseline$train(task_train)

# 9. 进行预测
prediction_svm_baseline <- learner_svm_baseline$predict(task_test)

# 10. 计算性能指标，acc tpr tnr auc



# XGBOOST --------

# 2. 定义学习器
pipeline <- po("encode", method = "one-hot") %>>% lrn("classif.xgboost")
learner_xgboost_baseline <- as_learner(pipeline)
learner_xgboost_baseline$predict_type <- "prob"

# 设置超参数搜索空间
param_set <- ps(
  classif.xgboost.eta = p_dbl(0.001, 0.3), # 学习率
  classif.xgboost.max_depth = p_int(1, 100), # 树的最大深度
  classif.xgboost.subsample = p_dbl(0, 1), # 样本采样比例
  classif.xgboost.colsample_bytree = p_dbl(0, 1), # 每棵树的列采样比例
  classif.xgboost.gamma = p_dbl(0, 1), # 最小损失下降值
  classif.xgboost.lambda = p_dbl(0, 10), # L2 正则化
  classif.xgboost.alpha = p_dbl(0, 10) # L1 正则化
)

# 4. 定义调优实例
tuner <- tnr("mbo") # 贝叶斯优化调优器
instance_xgboost_baseline <- TuningInstanceBatchSingleCrit$new(
  task = task_train,
  learner = learner_xgboost_baseline,
  resampling = rsmp("cv", folds = 10), # 交叉验证
  measure = msr("classif.acc"), # 优化指标
  search_space = param_set,
  terminator = trm("stagnation", iters = 10, threshold = 0)
)

# 5. 执行超参数调优
future::plan("multisession", workers = 15)
tuner$optimize(instance_xgboost_baseline)
future::plan(sequential)

# 6. 获取最佳参数


learner_xgboost_baseline$param_set$values <- instance_xgboost_baseline$result_learner_param_vals

# 7. 训练模型
learner_xgboost_baseline$train(task_train)

# 10. 计算性能指标，acc tpr tnr auc

# 9. 进行预测
prediction_xgboost_baseline <- learner_xgboost_baseline$predict(task_test)





# 性能评估 ------------
# 10. 计算性能指标，acc tpr tnr auc


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


# 绘制条形图
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
  filename = "Paper/plots_raw/barplot_baseline.tiff",   # 文件名
  plot = barplot_baseline,              # ggplot对象
  device = "tiff",               # 保存格式
  dpi = 600,                     # 分辨率，通常300 dpi适合出版
  width = 19,                     # 图像宽度，单位为英寸，可根据期刊要求调整
  height = 13,                    # 图像高度，单位为英寸，可根据期刊要求调整
  units = "cm",                  # 单位，可选："in", "cm", "mm"
  compression = "lzw"            # 压缩格式，LZW通常是推荐的无损压缩
)

# 绘制AUC图
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
  filename = "Paper/plots_raw/AUC_baseline.tiff",   # 文件名
  plot = AUC_baseline,              # ggplot对象
  device = "tiff",               # 保存格式
  dpi = 600,                     # 分辨率，通常300 dpi适合出版
  width = 19,                     # 图像宽度，单位为英寸，可根据期刊要求调整
  height = 13,                    # 图像高度，单位为英寸，可根据期刊要求调整
  units = "cm",                  # 单位，可选："in", "cm", "mm"
  compression = "lzw"            # 压缩格式，LZW通常是推荐的无损压缩
)


library(patchwork)
(barplot_baseline/AUC_baseline)

ggsave(
  filename = "Paper/plots_raw/baseline_performance.tiff",   # 文件名
  plot = (barplot_baseline/AUC_baseline),              # ggplot对象
  device = "tiff",               # 保存格式
  dpi = 600,                     # 分辨率，通常300 dpi适合出版
  width = 19,                     # 图像宽度，单位为英寸，可根据期刊要求调整
  height = 24,                    # 图像高度，单位为英寸，可根据期刊要求调整
  units = "cm",                  # 单位，可选："in", "cm", "mm"
  compression = "lzw"            # 压缩格式，LZW通常是推荐的无损压缩
)
rm(metrics_long)
