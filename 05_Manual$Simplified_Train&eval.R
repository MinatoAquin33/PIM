library(mlr3verse)
library(mlr3)
library(mlr3learners)
library(mlr3tuning)
library(future)

set.seed(42)




## S-baseline ------

# 1. 创建任务
task_train <- as_task_classif(dataset_train[, c(vars_Baseline_Simplified, "Abnormal_glucose_metabolism")], target = "Abnormal_glucose_metabolism", positive = "Yes")
task_test <- as_task_classif(dataset_test[, c(vars_Baseline_Simplified, "Abnormal_glucose_metabolism")], target = "Abnormal_glucose_metabolism", positive = "Yes")


# 2. 定义学习器
learner_Simplified_Baseline <- lrn("classif.lightgbm")
learner_Simplified_Baseline$predict_type <- "prob"

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
instance_Simplified_Baseline <- TuningInstanceBatchSingleCrit$new(
  task = task_train,
  learner = learner_Simplified_Baseline,
  resampling = rsmp("cv", folds = 10), # 交叉验证
  measure = msr("classif.acc"), # 优化指标
  search_space = param_set,
  terminator = trm("stagnation", iters = 10, threshold = 0)
)

# 5. 执行超参数调优
future::plan("multisession", workers = 15)
tuner$optimize(instance_Simplified_Baseline)
future::plan(sequential)

# 6. 获取最佳参数
learner_Simplified_Baseline$param_set$values <- instance_Simplified_Baseline$result_learner_param_vals


# 7. 训练模型
learner_Simplified_Baseline$train(task_train)

# 9. 进行预测
prediction_Simplified_Baseline <- learner_Simplified_Baseline$predict(task_test)






## Manual ------

# 1. 创建任务
task_train <- as_task_classif(dataset_train[, c(vars_Manual, "Abnormal_glucose_metabolism")], target = "Abnormal_glucose_metabolism", positive = "Yes")
task_test <- as_task_classif(dataset_test[, c(vars_Manual, "Abnormal_glucose_metabolism")], target = "Abnormal_glucose_metabolism", positive = "Yes")


# 2. 定义学习器
learner_Manual <- lrn("classif.lightgbm")
learner_Manual$predict_type <- "prob"

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
instance_Manual <- TuningInstanceBatchSingleCrit$new(
  task = task_train,
  learner = learner_Manual,
  resampling = rsmp("cv", folds = 10), # 交叉验证
  measure = msr("classif.acc"), # 优化指标
  search_space = param_set,
  terminator = trm("stagnation", iters = 10, threshold = 0)
)

# 5. 执行超参数调优
future::plan("multisession", workers = 15)
tuner$optimize(instance_Manual)
future::plan(sequential)

# 6. 获取最佳参数

learner_Manual$param_set$values <- instance_Manual$result_learner_param_vals

# 7. 训练模型
learner_Manual$train(task_train)

# 9. 进行预测
prediction_Manual <- learner_Manual$predict(task_test)






## S-Manual ------

# 1. 创建任务
task_train <- as_task_classif(dataset_train[, c(vars_Manual_Simplified, "Abnormal_glucose_metabolism")], target = "Abnormal_glucose_metabolism", positive = "Yes")
task_test <- as_task_classif(dataset_test[, c(vars_Manual_Simplified, "Abnormal_glucose_metabolism")], target = "Abnormal_glucose_metabolism", positive = "Yes")


# 2. 定义学习器
learner_Simplified_Manual <- lrn("classif.lightgbm")
learner_Simplified_Manual$predict_type <- "prob"

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
instance_Simplified_Manual <- TuningInstanceBatchSingleCrit$new(
  task = task_train,
  learner = learner_Simplified_Baseline,
  resampling = rsmp("cv", folds = 10), # 交叉验证
  measure = msr("classif.acc"), # 优化指标
  search_space = param_set,
  terminator = trm("stagnation", iters = 10, threshold = 0)
)

# 5. 执行超参数调优
future::plan("multisession", workers = 15)
tuner$optimize(instance_Simplified_Manual)
future::plan(sequential)

# 6. 获取最佳参数

learner_Simplified_Manual$param_set$values <- instance_Simplified_Manual$result_learner_param_vals

# 7. 训练模型
learner_Simplified_Manual$train(task_train)

# 9. 进行预测
prediction_Simplified_Manual <- learner_Simplified_Manual$predict(task_test)

# 10. 计算性能指标，acc tpr tnr auc





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
  filename = "Paper/plots_raw/barplot2.tiff",   # 文件名
  plot = barplot2,              # ggplot对象
  device = "tiff",               # 保存格式
  dpi = 600,                     # 分辨率，通常300 dpi适合出版
  width = 19,                     # 图像宽度，单位为英寸，可根据期刊要求调整
  height = 13,                    # 图像高度，单位为英寸，可根据期刊要求调整
  units = "cm",                  # 单位，可选："in", "cm", "mm"
  compression = "lzw"            # 压缩格式，LZW通常是推荐的无损压缩
)

# 绘制AUC图
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
  filename = "Paper/plots_raw/AUC2.tiff",   # 文件名
  plot = AUC_2,              # ggplot对象
  device = "tiff",               # 保存格式
  dpi = 600,                     # 分辨率，通常300 dpi适合出版
  width = 19,                     # 图像宽度，单位为英寸，可根据期刊要求调整
  height = 13,                    # 图像高度，单位为英寸，可根据期刊要求调整
  units = "cm",                  # 单位，可选："in", "cm", "mm"
  compression = "lzw"            # 压缩格式，LZW通常是推荐的无损压缩
)

library(patchwork)
(barplot2/AUC_2)

ggsave(
  filename = "Paper/plots_raw/Lgbm_models_performance.tiff",   # 文件名
  plot = (barplot2/AUC_2),              # ggplot对象
  device = "tiff",               # 保存格式
  dpi = 600,                     # 分辨率，通常300 dpi适合出版
  width = 19,                     # 图像宽度，单位为英寸，可根据期刊要求调整
  height = 24,                    # 图像高度，单位为英寸，可根据期刊要求调整
  units = "cm",                  # 单位，可选："in", "cm", "mm"
  compression = "lzw"            # 压缩格式，LZW通常是推荐的无损压缩
)
