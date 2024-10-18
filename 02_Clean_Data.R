# 提取所需变量 -----------
dataset <- data.frame(matrix(ncol = 0, nrow = nrow(dataset_NHANES)))

dataset$SEQN <- dataset_NHANES$SEQN #编号

dataset$Age <- dataset_NHANES$RIDAGEYR #年龄

dataset$Marital_status <- dataset_NHANES$DMDMARTL #婚姻
levels(dataset$Marital_status)[7:9] <- NA
levels(dataset$Marital_status) <-
  c(
    "Married/Living with Partner",
    "Widowed/Divorced/Separated",
    "Widowed/Divorced/Separated",
    "Widowed/Divorced/Separated",
    "Never married",
    "Married/Living with Partner"
  )
dataset$Marital_status <- as.character(dataset$Marital_status)

dataset$Marital_status2 <- dataset_NHANES$DMDMARTZ
levels(dataset$Marital_status2)[c(4,5,7)] <- NA
dataset$Marital_status2 <- as.character(dataset$Marital_status2)

dataset$Marital_status <- ifelse(is.na(dataset$Marital_status), dataset$Marital_status2, dataset$Marital_status)
dataset$Marital_status <- gsub("Married/Living with Partner", "Married/Living with partner", dataset$Marital_status)
dataset$Marital_status <- factor(dataset$Marital_status,levels = c("Never married","Married/Living with partner","Widowed/Divorced/Separated"))
dataset$Marital_status2 <- NULL

dataset$Education_level <- dataset_NHANES$DMDEDUC2 #教育
levels(dataset$Education_level)[6:8] <- NA
levels(dataset$Education_level) <- c(
  "Below high school",
  "Below high school",
  "High school/equivalent",
  "Collage and above",
  "Collage and above",
  "Below high school",
  "Below high school",
  "High school/equivalent",
  "Collage and above",
  "Collage and above"
)

dataset$Poverty_income_ratio <- dataset_NHANES$INDFMPIR #贫困收入比

dataset$Pregnancy_status <- dataset_NHANES$RIDEXPRG #妊娠

dataset$Race <- dataset_NHANES$RIDRETH3 #种族

dataset$Gender <- dataset_NHANES$RIAGENDR #性别



dataset$Systolic_pressure <- rowMeans(dataset_NHANES[, c("BPXSY1", "BPXSY2", "BPXSY3", "BPXSY4")], na.rm = TRUE)#收缩压
dataset$Systolic_pressure2 <- rowMeans(dataset_NHANES[, c("BPXOSY1", "BPXOSY2", "BPXOSY3")], na.rm = TRUE)#收缩压

add_pressures <- function(x, y) {
  ifelse(is.na(x) & is.na(y), NA, ifelse(is.na(x), 0, x) + ifelse(is.na(y), 0, y))
}

dataset$Systolic_pressure <- add_pressures(dataset$Systolic_pressure, dataset$Systolic_pressure2)

dataset$Systolic_pressure2 <- NULL

dataset$Diastolic_pressure <- rowMeans(dataset_NHANES[, c("BPXDI1", "BPXDI2", "BPXDI3", "BPXDI4")], na.rm = TRUE)#舒张压
dataset$Diastolic_pressure2 <- rowMeans(dataset_NHANES[, c("BPXOSY1", "BPXOSY2", "BPXOSY3")], na.rm = TRUE)#收缩压
dataset$Diastolic_pressure <- add_pressures(dataset$Diastolic_pressure, dataset$Diastolic_pressure2)

dataset$Diastolic_pressure2 <- NULL

dataset$Hypertension <- dataset_NHANES$BPQ020
levels(dataset$Hypertension)[3:4] <- NA
dataset$Hypertension <- as.character(dataset$Hypertension)

dataset$Hypertension <- ifelse(dataset$Hypertension == "Yes"| dataset$Systolic_pressure >= 130 | dataset$Diastolic_pressure >= 80, "Yes", dataset$Hypertension)
dataset$Hypertension <- factor(dataset$Hypertension,levels =  c("No","Yes"))


dataset$Smoke <- dataset_NHANES$LBXCOT #吸烟/二手烟
dataset$Smoke <- ifelse(dataset$Smoke < median(dataset$Smoke,na.rm = TRUE), 0, 1)
dataset$Smoke <- factor(dataset$Smoke,
                                   levels = c(0, 1),
                                   labels = c('No', 'Yes'),
)


dataset$Alcohol <- ifelse(
  dataset_NHANES$ALQ101 == "No",
  0,
  ifelse(
    dataset_NHANES$ALQ101 == "Yes",
    1,
      NA
    )
)
dataset$Alcohol <- factor(dataset$Alcohol,
                        levels = c(0, 1),
                        labels = c('No', 'Yes'),
)


dataset$Total_cholesterol <- dataset_NHANES$TC #血脂
dataset$Total_triglycerides <- dataset_NHANES$TG
dataset$HDL_cholesterol <- dataset_NHANES$HDL_C
dataset$LDL_cholesterol <- dataset_NHANES$LDL_C

dataset$Hyperlipidemia <- ifelse(dataset_NHANES$BPQ080 == "Yes", 1, 0)

dataset$Hyperlipidemia <- ifelse(dataset$Total_triglycerides >= 150 | dataset$Total_cholesterol >= 200 | dataset$LDL_cholesterol >= 150 |
                                   (dataset$HDL_cholesterol <= 40 & dataset$Gender == "Male") |
                                   dataset$Hyperlipidemia == 1 |
                                   (dataset$HDL_cholesterol <= 50 & dataset$Gender == "Female"), 1, 0)
dataset$Hyperlipidemia <- factor(dataset$Hyperlipidemia,
                                 levels = c(0, 1),
                                 labels = c('No', 'Yes'),
)


dataset$Two_hour_glucose_OGTT <- dataset_NHANES$LBXGLT # OGTT两小时血糖
dataset$Fasting_glucose <- dataset_NHANES$LBXGLU # 空腹血糖
dataset$Glycohemoglobin <- dataset_NHANES$LBXGH # 糖化血红蛋白
dataset$Taking_insulin <- dataset_NHANES$DIQ050  # 当前正在使用胰岛素
levels(dataset$Taking_insulin)[3:4] <- NA

dataset$Taking_diabetic_pills <- dataset_NHANES$DIQ070  # 使用降糖药
levels(dataset$Taking_diabetic_pills)[3:4] <- NA  


dataset$WBC <- dataset_NHANES$LBXWBCSI              # 白血球计数 WBC: White_blood_cell_count
dataset$LY <- dataset_NHANES$LBXLYPCT               # 淋巴细胞百分比 LY: Lymphocyte_percent
dataset$ALC <- dataset_NHANES$LBDLYMNO              # 淋巴细胞绝对值 ALC: Absolute_lymphocyte_count
dataset$MO <- dataset_NHANES$LBXMOPCT               # 单核细胞百分比 MO: Monocyte_percent
dataset$AMC <- dataset_NHANES$LBDMONO               # 单核细胞绝对值 AMC: Absolute_monocyte_count
dataset$NE <- dataset_NHANES$LBXNEPCT               # 中性粒细胞百分比 NE: Neutrophils_percent
dataset$ANC <- dataset_NHANES$LBDNENO               # 中性粒细胞绝对值 ANC:Absolute_neutrophils_count
dataset$EO <- dataset_NHANES$LBXEOPCT               # 嗜酸性细胞百分比 EO: Eosinophils_percent
dataset$AEC <- dataset_NHANES$LBDEONO               # 嗜酸性细胞绝对值 AEC: Absolute_eosinophils_count
dataset$BA <- dataset_NHANES$LBXBAPCT               # 嗜碱性细胞百分比 BA: Basophils_percent
dataset$ABC <- dataset_NHANES$LBDBANO               # 嗜碱性细胞绝对 ABC:Absolute_basophils_percent_count
dataset$RBC <- dataset_NHANES$LBXRBCSI              # 红细胞计数 RBC:Red_blood_cell_count
dataset$Hb <- dataset_NHANES$LBXHGB                 # 血红蛋白 Hb:Hemoglobin
dataset$Hct <- dataset_NHANES$LBXHCT                # 红细胞压积 Hct:Hematocrit
dataset$MCV <- dataset_NHANES$LBXMCVSI              # 平均红细胞体积 MCV: Mean_corpuscula_volume
dataset$MCH <- dataset_NHANES$LBXMCHSI              # 平均红细胞血红蛋白 MCH: Mean_corpuscula_hemoglobin
dataset$MCHC <- dataset_NHANES$LBXMC                # 平均红细胞血红蛋白浓度 MCHC: Mean_corpuscula_hemoglobin_concentration
dataset$RDW <- dataset_NHANES$LBXRDW                # 红细胞分布宽度 RDW: Red_cell_distribution_width
dataset$PLT <- dataset_NHANES$LBXPLTSI              # 血小板计数 PLT: Platelet_count
dataset$MPV <- dataset_NHANES$LBXMPSI               # 平均血小板体积 MPV: Mean_platelet_volume

dataset$Body_mass_index <- dataset_NHANES$BMXBMI  # 体重指数
dataset$Waist <- dataset_NHANES$BMXWAIST #腰围

PAQdt <- as.data.frame(dataset_NHANES[,"SEQN"])

PAQdt$Vigorous_work_activity <- dataset_NHANES$PAQ605
PAQdt$PAQ610 <- dataset_NHANES$PAQ610
PAQdt$PAQ610[PAQdt$PAQ610 %in% c(99, 77)] <- NA
PAQdt$PAD615 <- dataset_NHANES$PAD615
PAQdt$PAD615[PAQdt$PAD615 %in% c(9999, 7777)] <- NA
PAQdt$Vigorous_work_activity <- ifelse(PAQdt$Vigorous_work_activity == "Yes", PAQdt$PAQ610*PAQdt$PAD615 ,
                                        ifelse(PAQdt$Vigorous_work_activity == "No", 0, NA))

PAQdt$Vigorous_work_activity <- PAQdt$Vigorous_work_activity*2



PAQdt$Moderate_work_activity <- dataset_NHANES$PAQ620
PAQdt$PAQ625 <- dataset_NHANES$PAQ625
PAQdt$PAQ625[PAQdt$PAQ625 %in% c(99, 77)] <- NA
PAQdt$PAD630 <- dataset_NHANES$PAD630
PAQdt$PAD630[PAQdt$PAD630 %in% c(9999, 7777)] <- NA
PAQdt$Moderate_work_activity <- ifelse(PAQdt$Moderate_work_activity == "Yes", PAQdt$PAQ625*PAQdt$PAD630 ,
                                       ifelse(PAQdt$Moderate_work_activity == "No", 0, NA))



PAQdt$Vigorous_recreational_activity <- dataset_NHANES$PAQ650
PAQdt$PAQ655 <- dataset_NHANES$PAQ655
PAQdt$PAQ655[PAQdt$PAQ655 %in% c(99, 77)] <- NA
PAQdt$PAD660 <- dataset_NHANES$PAD660
PAQdt$PAD660[PAQdt$PAD660 %in% c(9999, 7777)] <- NA
PAQdt$Vigorous_recreational_activity <- ifelse(PAQdt$Vigorous_recreational_activity == "Yes", PAQdt$PAQ655*PAQdt$PAD660 ,
                                       ifelse(PAQdt$Vigorous_recreational_activity == "No", 0, NA))

PAQdt$Vigorous_recreational_activity <- PAQdt$Vigorous_recreational_activity*2


PAQdt$Moderate_recreational_activity <- dataset_NHANES$PAQ665
PAQdt$PAQ670 <- dataset_NHANES$PAQ670
PAQdt$PAQ670[PAQdt$PAQ670 %in% c(99, 77)] <- NA
PAQdt$PAD675 <- dataset_NHANES$PAD675
PAQdt$PAD675[PAQdt$PAD675 %in% c(9999, 7777)] <- NA
PAQdt$Moderate_recreational_activity <- ifelse(PAQdt$Moderate_recreational_activity == "Yes", PAQdt$PAQ670*PAQdt$PAD675 ,
                                       ifelse(PAQdt$Moderate_recreational_activity == "No", 0, NA))


PAQdt$total_activity <- rowSums(PAQdt[, c("Moderate_recreational_activity", 
                                          "Vigorous_recreational_activity", 
                                          "Moderate_work_activity", 
                                          "Vigorous_work_activity")], 
                                na.rm = TRUE)

PAQdt$total_activity <- ifelse(is.na(PAQdt$Vigorous_work_activity)|is.na(PAQdt$Vigorous_recreational_activity)|is.na(PAQdt$Moderate_work_activity)|is.na(PAQdt$Moderate_work_activity),NA,PAQdt$total_activity)


dataset$Physical_activity <- PAQdt$total_activity



dataset$Diabetes <- dataset_NHANES$DIQ010 # 诊断糖尿病
dataset$Prediabetes <- dataset_NHANES$DIQ160 # 诊断糖尿病前期



CVD <- data.frame(matrix(ncol = 0, nrow = nrow(dataset_NHANES))) # 心血管疾病
CVD$SEQN <- dataset_NHANES$SEQN
CVD$Heart_attack <- dataset_NHANES$MCQ160E
CVD$Stroke <- dataset_NHANES$MCQ160F
CVD$Coronary_heart_disease <- dataset_NHANES$MCQ160C
CVD$Angina <- dataset_NHANES$MCQ160D
CVD$Congestive_heart_failure <- dataset_NHANES$MCQ160B

CVD <- CVD[complete.cases(CVD[, c("Heart_attack", "Stroke", "Coronary_heart_disease", "Angina", "Congestive_heart_failure")]), ]

CVD$CVD <- ifelse(CVD$Heart_attack == "Yes" | CVD$Stroke == "Yes" | CVD$Coronary_heart_disease == "Yes" | CVD$Angina == "Yes" | CVD$Congestive_heart_failure == "Yes",1,0)
CVD$CVD <- factor(CVD$CVD,levels = c(0,1),labels = c("No","Yes"))

dataset <- plyr::join_all(
  list(dataset,CVD[,c("CVD","SEQN")]),
  by = 'SEQN',
  type = 'full'
)




# 数据清洗 ----------
dataset <- dataset[dataset$Age>=20,] # 排除年龄小于20岁的对象
dataset <- dataset[dataset$Pregnancy_status != "Yes, positive lab pregnancy test or self-reported pregnant at exam" | is.na(dataset$Pregnancy_status), ] # 排除妊娠对象
dataset$Pregnancy_status <- NULL


dataset <- dataset[complete.cases(dataset[,c("Fasting_glucose","Glycohemoglobin","Two_hour_glucose_OGTT")]),]


dataset$Abnormal_glucose_metabolism <- ifelse(dataset$Fasting_glucose < 100, 0, 1)
dataset$Abnormal_glucose_metabolism <- ifelse(dataset$Fasting_glucose > 125, 2, dataset$Abnormal_glucose_metabolism)

dataset$Abnormal_glucose_metabolism <- ifelse(dataset$Glycohemoglobin >= 5.7, 1, dataset$Abnormal_glucose_metabolism)
dataset$Abnormal_glucose_metabolism <- ifelse(dataset$Glycohemoglobin >= 6.5, 2, dataset$Abnormal_glucose_metabolism)

dataset$Abnormal_glucose_metabolism <- ifelse(dataset$Two_hour_glucose_OGTT >= 140 & dataset$Two_hour_glucose_OGTT < 200, 1, dataset$Abnormal_glucose_metabolism)
dataset$Abnormal_glucose_metabolism <- ifelse(dataset$Two_hour_glucose_OGTT >= 200, 2, dataset$Abnormal_glucose_metabolism)



dataset[dataset$Diabetes=="Borderline","Abnormal_glucose_metabolism"] <- 1
dataset[dataset$Diabetes=="Yes","Abnormal_glucose_metabolism"] <- 2
dataset[dataset$Taking_insulin=="Yes","Abnormal_glucose_metabolism"] <- 1
dataset[dataset$Taking_diabetic_pills=="Yes","Abnormal_glucose_metabolism"] <- 1


# 删除所有Diabete对象
dataset$Abnormal_glucose_metabolism <- ifelse(dataset$Abnormal_glucose_metabolism == 2, NA , dataset$Abnormal_glucose_metabolism)
dataset$Abnormal_glucose_metabolism <- factor(dataset$Abnormal_glucose_metabolism,levels=c(0,1),labels = c("No","Yes"))# 0:No 1:Prediabetes
dataset <- dataset[complete.cases(dataset$Abnormal_glucose_metabolism),]

# 删除金标准相关变量
dataset <- subset(dataset, select = -c(Taking_insulin, Taking_diabetic_pills, Diabetes, Prediabetes, Two_hour_glucose_OGTT,Glycohemoglobin,Fasting_glucose))


# 拆分数据集 -------
set.seed(42)

dataset_train <- dataset[dataset$SEQN<83732,]
dataset_test <- dataset[dataset$SEQN>=83732 & dataset$SEQN<=93702,]


# 插补数据 ------
set.seed(42)

Abnormal_glucose_metabolism <- dataset_train$Abnormal_glucose_metabolism
imputed_data_train <- mice::mice(subset(dataset_train, select = -c(Abnormal_glucose_metabolism)),seed = 42)
dataset_train <- mice::complete(imputed_data_train, 1)
dataset_train <- cbind(dataset_train,Abnormal_glucose_metabolism)


Abnormal_glucose_metabolism <- dataset_test$Abnormal_glucose_metabolism
imputed_data_test <- mice::mice(subset(dataset_test, select = -c(Abnormal_glucose_metabolism)),seed = 42)
dataset_test <- mice::complete(imputed_data_test, 1)
dataset_test <- cbind(dataset_test,Abnormal_glucose_metabolism)


rm(Abnormal_glucose_metabolism)

# 欠采样 -----
set.seed(42)
dataset_train <-
  rbind(dataset_train[dataset_train$Abnormal_glucose_metabolism == "No", ], dataset_train[sample(which(dataset_train$Abnormal_glucose_metabolism == "Yes"), sum(dataset_train$Abnormal_glucose_metabolism == "No")), ])





dataset_ds1 <- dataset_train # 用于描述
dataset_ds2 <- dataset_test # 用于描述





# 标准化数据集 -----
# 提取dataset_train中数值变量的列名，排除"SEQN"
numeric_cols <- names(dataset_train)[sapply(dataset_train, is.numeric) & names(dataset_train) != "SEQN"]

# 提取dataset_train中的均值和标准差
train_means <- sapply(dataset_train[numeric_cols], mean)
train_sds <- sapply(dataset_train[numeric_cols], sd)

# 使用dataset_train的均值和标准差来缩放dataset_test
dataset_test[numeric_cols] <- mapply(function(x, mean, sd) {
  (x - mean) / sd
}, dataset_test[numeric_cols], train_means, train_sds, SIMPLIFY = FALSE)

# 缩放dataset_train
dataset_train[numeric_cols] <- mapply(function(x, mean, sd) {
  (x - mean) / sd
}, dataset_train[numeric_cols], train_means, train_sds, SIMPLIFY = FALSE)

rm(numeric_cols,train_indices,add_pressures,PAQdt,imputed_data_test,imputed_data_train)
