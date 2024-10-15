# Baseline ------
vars_Baseline <- names(dataset)
vars_Baseline<- setdiff(vars_Baseline,
           c("Abnormal_glucose_metabolism","SEQN","Hypertension","Hyperlipidemia"))

# Manual ------
vars_Manual<- c(vars_Baseline,"Hypertension","Hyperlipidemia")

vars_Manual<- setdiff(vars_Manual,
           c("Glycohemoglobin","Systolic_pressure","Diastolic_pressure","Total_cholesterol","Total_triglycerides","HDL_cholesterol","LDL_cholesterol","WBC","LY","ALC","MO","AMC","NE","ANC","EO","AEC","BA","ABC","RBC","Hb","Hct","MCV","MCH","MCHC","RDW","PLT","MPV"))


# LASSO for Baseline_Simplified ----------
set.seed(42)
varnames <- c(vars_Baseline,"Abnormal_glucose_metabolism")

x <- model.matrix(as.formula("Abnormal_glucose_metabolism ~ ."), data = dataset_train[, varnames]) # 展
x <- x[, -1]

y <- dataset_train$Abnormal_glucose_metabolism


cv_lasso <- glmnet::cv.glmnet(x, y, family = "binomial", alpha = 1)
cv_lasso$lambda.min


model_glm_lasso <- glmnet::glmnet(x,
                                  y,
                                  family = "binomial",
                                  alpha = 1,
                                  lambda = cv_lasso$lambda.min)

coef(model_glm_lasso)  


lasso_coefs <- coef(model_glm_lasso)
selected_vars <- rownames(lasso_coefs)[lasso_coefs[, 1] != 0]

selected_vars <- selected_vars[selected_vars != "(Intercept)"]

selected_vars <- sub("\\d+$", "", selected_vars)

selected_vars <- gsub("Marital_statusWidowed/Divorced/Separated|Marital_statusNever married|Marital_statusMarried/Living with partner", "Marital_status", selected_vars)
selected_vars <- gsub("Education_levelCollage and above", "Education_level", selected_vars)
selected_vars <- gsub("Education_levelHigh school/equivalent", "Education_level", selected_vars)
selected_vars <- gsub("Poverty_income_ratio.L|Poverty_income_ratio.Q", "Poverty_income_ratio", selected_vars)
selected_vars <- gsub("RaceOther Hispanic|RaceNon-Hispanic White|RaceNon-Hispanic Black|RaceNon-Hispanic Asian|RaceOther Race - Including Multi-Racial", "Race", selected_vars)
selected_vars <- gsub("Yes|Female", "", selected_vars)

selected_vars <- unique(selected_vars)
vars_Baseline_Simplified <- selected_vars




# LASSO for Manual_Simplified ----------
varnames <- c(vars_Manual,"Abnormal_glucose_metabolism")
x <- model.matrix(as.formula("Abnormal_glucose_metabolism ~ ."), data = dataset_train[, varnames])
x <- x[, -1] 
y <- dataset_train$Abnormal_glucose_metabolism
cv_lasso <- glmnet::cv.glmnet(x, y, family = "binomial", alpha = 1)
cv_lasso$lambda.min
model_glm_lasso <- glmnet::glmnet(x,
                                  y,
                                  family = "binomial",
                                  alpha = 1,
                                  lambda = cv_lasso$lambda.min)
coef(model_glm_lasso)  
lasso_coefs <- coef(model_glm_lasso)
selected_vars <- rownames(lasso_coefs)[lasso_coefs[, 1] != 0]
selected_vars <- selected_vars[selected_vars != "(Intercept)"]
selected_vars <- sub("\\d+$", "", selected_vars)
selected_vars <- gsub("Marital_statusWidowed/Divorced/Separated|Marital_statusNever married|Marital_statusMarried/Living with partner", "Marital_status", selected_vars)
selected_vars <- gsub("Education_levelCollage and above", "Education_level", selected_vars)
selected_vars <- gsub("Education_levelHigh school/equivalent", "Education_level", selected_vars)
selected_vars <- gsub("Poverty_income_ratio.L|Poverty_income_ratio.Q", "Poverty_income_ratio", selected_vars)
selected_vars <- gsub("RaceOther Hispanic|RaceNon-Hispanic White|RaceNon-Hispanic Black|RaceNon-Hispanic Asian|RaceOther Race - Including Multi-Racial", "Race", selected_vars)
selected_vars <- gsub("Yes|Female", "", selected_vars)


selected_vars <- unique(selected_vars)
vars_Manual_Simplified <- selected_vars

rm(x,y,varnames,model_glm_lasso,lasso_coefs,cv_lasso,selected_vars)
