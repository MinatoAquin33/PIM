for (pkg in c("shiny", "iml", "ggplot2", "mlr3verse","remotes","lightgbm")) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg)
  }
}


if (!requireNamespace("mlr3extralearners", quietly = TRUE)) {
  remotes::install_github("mlr-org/mlr3extralearners@*release")
}


library(shiny)
library(iml)
library(ggplot2)
library(mlr3verse)
library(mlr3extralearners)

load("Models.RData")

ui <- fluidPage(
  tags$head(tags$script(HTML('
    $(document).on("click", "#submit", function() {
      window.scrollTo(0, 0);
    });
  '))),
  
  titlePanel("Prediabetes Insight Model: Prediction and Analysis of Prediabetes Risk"),
  
  sidebarLayout(
    sidebarPanel(
      numericInput("age", "Age:", value = NA, min = 0, max = 120),
      helpText("Enter the age of the individual in years."),
      
      selectInput("gender", "Gender:", choices = c("Male", "Female")),
      helpText("Select the gender of the individual."),
      
      selectInput("marital_status", "Marital Status:", 
                  choices = c("Never married", "Married or Living with partner", "Widowed or Divorced or Separated")),
      helpText("Select the marital status of the individual."),
      
      selectInput("education_level", "Education Level:", 
                  choices = c("Below high school", "High school or equivalent", "Collage and above")),
      helpText("Select the highest level of education attained."),
      
      selectInput("race", "Race:", 
                  choices = c("Mexican American", "Non-Hispanic White", 
                              "Non-Hispanic Black", "Non-Hispanic Asian", "Other Hispanic", "Other Race")),
      helpText("Select the race of the individual."),
      
      numericInput("poverty_income_ratio", "Poverty Income Ratio:", value = NA, min = 0,max=99),
      helpText("Enter the ratio of family income to poverty threshold."),
      
      selectInput("hypertension", "Hypertension:", choices = c("No", "Yes")),
      helpText("Does this individual have hypertension?"),
      
      selectInput("hyperlipidemia", "Hyperlipidemia:", choices = c("No", "Yes")),
      helpText("Does this individual have hyperlipidemia."),
      
      selectInput("smoke", "Smoke:", choices = c("No", "Yes")),
      helpText("Does the individual smoke or is exposed to secondhand smoke?"),
      
      selectInput("alcohol", "Alcohol:", choices = c("No", "Yes")),
      helpText("Does this individual drink alcohol?"),
      
      numericInput("body_mass_index", "Body Mass Index:", value = NA, min = 0,max=999),
      helpText(("Enter the body mass index (BMI) of the individual.")),
      helpText(withMathJax(("\\( BMI = \\frac{weight (kg)}{height (m)^2} \\)"))),
      
      numericInput("waist", "waist circumference", value = NA, min = 0,max=999),
      helpText("Enter the waist circumference of the individual in cm."),
      
      numericInput("physical_activity", "Physical Activity:", value = NA, min = 0, max= 9999),
      helpText("Enter the number of minutes of physical activity for this individual each week."),
      helpText("Here, physical activity refers to moderate-intensity physical activities in work or daily life, while high-intensity physical activities should be converted to equivalent moderate-intensity physical activity time by multiplying by 2."),
      helpText("Moderate-intensity activities will increase heart rate and breathing but are not overly strenuous. Examples include brisk walking and cycling."),
      helpText("High-intensity physical activities will cause a significant increase in heart rate and breathing. Examples include jogging, playing basketball, and soccer."),
      
      actionButton("submit", "Submit")
    ),
    
    mainPanel(
      uiOutput("predictionText"),
      plotOutput("shapPlot"),
      textOutput("abbreviation")
    )
  )
)

server <- function(input, output) {
  
  observeEvent(input$submit, {
    new_data <- data.frame(
      Age = (input$age - train_means["Age"]) / train_sds["Age"],
      Marital_status = factor(
        input$marital_status,
        levels = c(
          "Never married",
          "Married or Living with partner",
          "Widowed or Divorced or Separated"
        ),
        labels = c(
          "Never married",
          "Married/Living with partner",
          "Widowed/Divorced/Separated"
        )
      ), 
      Education_level = factor(
        input$education_level,
        levels = c(
          "Below high school",
          "High school or equivalent",
          "Collage and above"
        ),
        labels = c(
          "Below high school",
          "High school/equivalent",
          "Collage and above"
        )
      ), 
      Poverty_income_ratio = (input$poverty_income_ratio - train_means["Poverty_income_ratio"]) / train_sds["Poverty_income_ratio"],
      Race = factor(
        input$race,
        levels = c(
          "Mexican American",
          "Other Hispanic",
          "Non-Hispanic White",
          "Non-Hispanic Black",
          "Non-Hispanic Asian",
          "Other Race"
        ),
        labels = c(
          "Mexican American",
          "Other Hispanic",
          "Non-Hispanic White",
          "Non-Hispanic Black",
          "Non-Hispanic Asian",
          "Other Race - Including Multi-Racial"
        )
      ), 
      Gender = factor(input$gender, levels = c("Male", "Female")),
      Hypertension = factor(input$hypertension, levels = c("No", "Yes")),
      Smoke = factor(input$smoke, levels = c("No", "Yes")),
      Alcohol = factor(input$alcohol, levels = c("No", "Yes")),
      Hyperlipidemia = factor(input$hyperlipidemia, levels = c("No", "Yes")),
      Body_mass_index = (input$body_mass_index - train_means["Body_mass_index"]) / train_sds["Body_mass_index"],
      Waist = (input$waist - train_means["Waist"]) / train_sds["Waist"],
      Physical_activity = (input$physical_activity - train_means["Physical_activity"]) / train_sds["Physical_activity"]
    )
    prediction <- learner_Simplified_Manual$predict_newdata(new_data)
    prob <- prediction$prob[1]
    
    result <- ifelse(prob<0.5,"Normal","Prediabetes")
    
    output$predictionText <- renderUI({
      HTML(paste("<span style='font-size:20px;'>The individual is predicted to be:", result, "</span>"))
    })

    
    
    # SHAP

    predictor_Simplified_Manual <- Predictor$new(learner_Simplified_Manual, data = dataset_train[, c(vars_Manual_Simplified)], y = dataset_train$Abnormal_glucose_metabolism)
    shapley <- Shapley$new(predictor_Simplified_Manual, x.interest = new_data[1,c(vars_Manual_Simplified)])

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

    output$shapPlot <- renderPlot({
      shap_plot
    })
    output$abbreviation <- renderText({
      paste("WC: waist circumference, CVD: Cardiovascular Disease, BMI: Body Mass Index")
    })

  })
}

shinyApp(ui = ui, server = server)
