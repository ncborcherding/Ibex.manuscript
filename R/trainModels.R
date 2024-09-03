library(caret)
library(dplyr)
library(pROC)
library(xgboost)
library(nnet)

# Function to tune and train models using caret
trainModels <- function(data_matrix, 
                        target_vector, 
                        class, 
                        split_ratio = 0.7, 
                        use_smote = TRUE) {
  
  # Binarize the target vector (assumes binary classification: 0 and 1)
  binary_target <- as.factor(ifelse(target_vector %in% class, "Yes", "No"))
  # Convert data matrix to a data frame and add the binary target
  df <- as.data.frame(data_matrix)
  df$Target <- binary_target
  
  # Split data into training and validation sets
  set.seed(123)  # For reproducibility
  train_index <- createDataPartition(df$Target, p = split_ratio, list = FALSE)
  train_data <- df[train_index, ]
  validation_data <- df[-train_index, ]
  
  if (use_smote) {
    train_data <- DMwR::SMOTE(Target ~ ., data = train_data, perc.over = 100, perc.under = 200)
  }
  
  # Define train control with 5-fold cross-validation
  train_control <- trainControl(
    method = "cv", 
    number = 5,
    classProbs = TRUE,
    summaryFunction = twoClassSummary,
    savePredictions = "final"
  )
  
  # Define a list of models to train
  models <- list(
    "Logistic Regression" = "glm",
    "Random Forest" = "rf",
    "Support Vector Machine" = "svmRadial",
    "K-Nearest Neighbors" = "knn",
    "Naive Bayes" = "nb",
    "Gradient Boosting Machine" = "gbm",
    "Extreme Gradient Boosting" = "xgbTree",
    "Neural Network (MLP)" = "nnet",
    "Decision Tree (rpart)" = "rpart",
    "Linear Discriminant Analysis" = "lda"
  )
  
  # Initialize a list to store model results
  model_results <- list()
  
  # Loop through each model type
  for (model_name in names(models)) {
    set.seed(123)  # For reproducibility
    # Define class weights for the model if applicable
    if (model_name %in% c("Logistic Regression", "Support Vector Machine")) {
      weights <- ifelse(train_data$Target == 1, 10, 1)
      model <- train(
        Target ~ ., 
        data = train_data,
        method = models[[model_name]],
        trControl = train_control,
        metric = "ROC",  # Optimizing for ROC AUC
        preProcess = c("center", "scale"),  # Preprocessing steps
        weights = weights,  # Apply class weights
        tuneLength = 10  # Adjusts the level of hyperparameter tuning
      )
    } else {
      model <- train(
        Target ~ ., 
        data = train_data,
        method = models[[model_name]],
        trControl = train_control,
        metric = "ROC",  # Optimizing for ROC AUC
        preProcess = c("center", "scale"),  # Preprocessing steps
        tuneLength = 10  # Adjusts the level of hyperparameter tuning
      )
    }
  
    
    # Predict on the validation set
    validation_predictions <- predict(model, validation_data)
    
    # Store results including validation predictions
    model_results[[model_name]] <- list(
      model = model,
      validation_predictions = validation_predictions,
      validation_true = validation_data$Target,
      confusion_matrix = confusionMatrix(validation_predictions, validation_data$Target, positive = "Yes")
    )
  }
  return(model_results)
}
