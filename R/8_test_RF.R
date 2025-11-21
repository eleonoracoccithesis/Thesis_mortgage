#0. LOAD PACKAGES ______________________________________________________________
library(dplyr)
library(tidyr)
library(caret)


#1. PREPROCESS FUNCTION ________________________________________________________
prep_bin <- function(df) {
  df_bin <- df %>%
    filter(premium_debt_paid_2023 %in% c("1", "2")) %>%
    select(-participant, -year, -split)
  
  df_bin$premium_debt_paid_2023 <- ifelse(
    df_bin$premium_debt_paid_2023 == "2", "No", "Yes"
  )
  df_bin$premium_debt_paid_2023 <- factor(
    df_bin$premium_debt_paid_2023,
    levels = c("No", "Yes")
  )
  
  df_bin <- df_bin %>% drop_na()
  df_bin
}


#2. HELPER: PRECISION / RECALL / F1 ____________________________________________
compute_prf <- function(predictions, truth, positive) {
  truth <- factor(truth)
  predictions <- factor(predictions, levels = levels(truth))
  
  cm <- table(truth, predictions)
  
  if (!(positive %in% rownames(cm)) || !(positive %in% colnames(cm))) {
    precision <- 0
    recall    <- 0
    f1        <- 0
  } else {
    TP <- cm[positive, positive]
    FP <- sum(cm[, positive]) - TP
    FN <- sum(cm[positive, ]) - TP
    
    precision <- ifelse(TP + FP == 0, 0, TP / (TP + FP))
    recall    <- ifelse(TP + FN == 0, 0, TP / (TP + FN))
    f1        <- ifelse(precision + recall == 0, 0,
                        2 * precision * recall / (precision + recall))
  }
  
  c(Precision = precision, Recall = recall, F1 = f1)
}


#3. TEST-EVALUATION FUNCTION ___________________________________________________
evaluate_test_set <- function(data, model, name) { 
  # Prepare test data
  test_bin <- prep_bin(data)
  
  X_test <- test_bin %>% select(-premium_debt_paid_2023)
  y_test <- test_bin$premium_debt_paid_2023
  
  # Predict
  pred <- predict(model, newdata = X_test)
  pred <- factor(pred, levels = levels(y_test))
  
  # Confusion matrix
  cat("\n=== Confusion Matrix for", name, "===\n")
  cm <- confusionMatrix(pred, y_test, positive = "No")
  print(cm$table)
  
  # Class-specific metrics
  prf_no  <- compute_prf(pred, y_test, "No")
  prf_yes <- compute_prf(pred, y_test, "Yes")
  
  precision_no  <- prf_no["Precision"]
  recall_no     <- prf_no["Recall"]
  f1_no         <- prf_no["F1"]
  
  precision_yes <- prf_yes["Precision"]
  recall_yes    <- prf_yes["Recall"]
  f1_yes        <- prf_yes["F1"]
  
  macro_f1 <- mean(c(f1_no, f1_yes))
  accuracy <- as.numeric(cm$overall["Accuracy"])
  
  # Print results
  cat("\n=== Metrics for", name, "===\n")
  cat("Class: No  | Precision:", round(precision_no, 4),
      " Recall:", round(recall_no, 4),
      " F1:", round(f1_no, 4), "\n")
  cat("Class: Yes | Precision:", round(precision_yes, 4),
      " Recall:", round(recall_yes, 4),
      " F1:", round(f1_yes, 4), "\n")
  cat("Macro F1 Score:", round(macro_f1, 4), "\n")
  cat("Overall Accuracy:", round(accuracy, 4), "\n")
  
  # Return list
  list(
    ConfusionMatrix = cm$table,
    Accuracy = accuracy,
    Class_Metrics = list(
      No  = c(Precision = precision_no, Recall = recall_no, F1 = f1_no),
      Yes = c(Precision = precision_yes, Recall = recall_yes, F1 = f1_yes)
    ),
    MacroF1 = macro_f1
  )
}


#4. RUN TESTS __________________________________________________________________
# 2021–2022 dev test
rf_test_results_2122 <- evaluate_test_set(
  test_final,
  model_rf_f1,
  "Random Forest (2021–2022 dev test)"
)

# 2023 final test
rf_test_results_2023 <- evaluate_test_set(
  final_test_2023,
  model_rf_f1,
  "Random Forest (2023 final test)"
)
