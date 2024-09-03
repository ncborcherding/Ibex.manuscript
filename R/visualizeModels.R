library(ggplot2)
library(gridExtra)
library(pROC)

# Visualization Function
visualizeModel <- function(model_results) {
  
  # Initialize lists for storing plot data
  roc_data <- list()
  accuracy_data <- data.frame(Model = character(), Accuracy = numeric(), AUC = numeric(), stringsAsFactors = FALSE)
  var_imp_data <- list()
  
  # Extract ROC curves, accuracy, and variable importance
  for (model_name in names(model_results)) {
    model_info <- model_results[[model_name]]
    model <- model_info$model
    validation_pred <- model_info$validation_predictions
    validation_true <- model_info$validation_true
    
    # Calculate ROC Curve Data
    roc_curve <- roc(validation_true, validation_pred[, "1"], levels = rev(levels(validation_true)))
    roc_data[[model_name]] <- data.frame(
      Sensitivity = roc_curve$sensitivities,
      Specificity = 1 - roc_curve$specificities,
      Model = model_name
    )
    
    # Calculate Accuracy and AUC on the validation set
    predicted_classes <- ifelse(validation_pred[, "1"] > 0.5, "1", "0")
    accuracy <- mean(predicted_classes == validation_true)
    auc <- roc_curve$auc
    accuracy_data <- rbind(accuracy_data, data.frame(Model = model_name, Accuracy = accuracy, AUC = auc))
    
    # Variable Importance (if available)
    if ("varImp" %in% names(model)) {
      imp <- varImp(model)
      var_imp_data[[model_name]] <- imp
    }
  }
  
  # ROC Plot
  roc_plot <- ggplot() +
    geom_line(data = do.call(rbind, roc_data), aes(x = Specificity, y = Sensitivity, color = Model)) +
    labs(title = "ROC Curves", x = "1 - Specificity", y = "Sensitivity") +
    theme_minimal()
  
  # Accuracy Comparison Plot
  accuracy_plot <- ggplot(accuracy_data, aes(x = reorder(Model, Accuracy), y = Accuracy, fill = Model)) +
    geom_bar(stat = "identity") +
    coord_flip() +
    labs(title = "Accuracy Comparison", x = "Model", y = "Accuracy") +
    theme_minimal() +
    theme(legend.position = "none")
  
  # AUC Comparison Plot
  auc_plot <- ggplot(accuracy_data, aes(x = reorder(Model, AUC), y = AUC, fill = Model)) +
    geom_bar(stat = "identity") +
    coord_flip() +
    labs(title = "AUC Comparison", x = "Model", y = "AUC") +
    theme_minimal() +
    theme(legend.position = "none")
  
  # Variable Importance Plot (for models that provide variable importance)
  var_imp_plot <- list()
  for (model_name in names(var_imp_data)) {
    var_imp <- var_imp_data[[model_name]]
    var_imp_df <- as.data.frame(var_imp$importance)
    var_imp_df$Variable <- rownames(var_imp_df)
    var_imp_plot[[model_name]] <- ggplot(var_imp_df, aes(x = reorder(Variable, Overall), y = Overall)) +
      geom_bar(stat = "identity", fill = "steelblue") +
      coord_flip() +
      labs(title = paste("Variable Importance -", model_name), x = "Variable", y = "Importance") +
      theme_minimal()
  }
  
  # Combine Variable Importance Plots
  if (length(var_imp_plot) > 0) {
    var_imp_grid <- marrangeGrob(var_imp_plot, ncol = 2, nrow = ceiling(length(var_imp_plot) / 2))
  } else {
    var_imp_grid <- NULL
  }
  
  # Display the plots
  grid.arrange(roc_plot, accuracy_plot, auc_plot, var_imp_grid, ncol = 1, top = "Model Performance Visualization")
}