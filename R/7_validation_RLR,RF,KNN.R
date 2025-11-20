#0. LOAD PACKAGES ______________________________________________________________
library(dplyr)
library(tidyr)
library(caret)


#1. PREPROCESS VAL / TEST LIKE TRAIN ___________________________________________
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

val_df_bin  <- prep_bin(val_final)
test_df_bin <- prep_bin(test_final)   # for later, when you evaluate on test

table(val_df_bin$premium_debt_paid_2023)
table(test_df_bin$premium_debt_paid_2023)


#2. HELPER: PRECISION / RECALL / F1 FOR A GIVEN POSITIVE _______________________
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


#3. EVALUATION FUNCTION ________________________________________________________
evaluate_model <- function(predictions, truth) {
  truth <- factor(truth, levels = c("No", "Yes"))
  predictions <- factor(predictions, levels = levels(truth))
  
  cm <- confusionMatrix(predictions, truth, positive = "No")
  
  prf_no  <- compute_prf(predictions, truth, "No")
  prf_yes <- compute_prf(predictions, truth, "Yes")
  
  macro_f1 <- mean(c(prf_no["F1"], prf_yes["F1"]))
  
  list(
    ConfusionMatrix = cm$table,
    Precision_No = prf_no["Precision"],
    Recall_No    = prf_no["Recall"],
    F1_No        = prf_no["F1"],
    Precision_Yes = prf_yes["Precision"],
    Recall_Yes    = prf_yes["Recall"],
    F1_Yes        = prf_yes["F1"],
    Macro_F1      = macro_f1
  )
}


#4. VALIDATION: RLR, RF, KNN ___________________________________________________
# RLR
val_pred_rlr <- predict(model_rlr_f1, newdata = val_df_bin)
results_rlr_val <- evaluate_model(
  val_pred_rlr,
  val_df_bin$premium_debt_paid_2023
)

# RF
val_pred_rf <- predict(model_rf_f1, newdata = val_df_bin)
results_rf_val <- evaluate_model(
  val_pred_rf,
  val_df_bin$premium_debt_paid_2023
)

# KNN
val_pred_knn <- predict(model_knn_f1, newdata = val_df_bin)
results_knn_val <- evaluate_model(
  val_pred_knn,
  val_df_bin$premium_debt_paid_2023
)


#5. SUMMARY TABLE FOR VALIDATION RESULTS _______________________________________
val_summary <- data.frame(
  Model        = c("RLR", "Random Forest", "KNN"),
  Precision_No = round(c(
    as.numeric(results_rlr_val$Precision_No),
    as.numeric(results_rf_val$Precision_No),
    as.numeric(results_knn_val$Precision_No)
  ), 3),
  Recall_No    = round(c(
    as.numeric(results_rlr_val$Recall_No),
    as.numeric(results_rf_val$Recall_No),
    as.numeric(results_knn_val$Recall_No)
  ), 3),
  F1_No        = round(c(
    as.numeric(results_rlr_val$F1_No),
    as.numeric(results_rf_val$F1_No),
    as.numeric(results_knn_val$F1_No)
  ), 3),
  Precision_Yes = round(c(
    as.numeric(results_rlr_val$Precision_Yes),
    as.numeric(results_rf_val$Precision_Yes),
    as.numeric(results_knn_val$Precision_Yes)
  ), 3),
  Recall_Yes    = round(c(
    as.numeric(results_rlr_val$Recall_Yes),
    as.numeric(results_rf_val$Recall_Yes),
    as.numeric(results_knn_val$Recall_Yes)
  ), 3),
  F1_Yes        = round(c(
    as.numeric(results_rlr_val$F1_Yes),
    as.numeric(results_rf_val$F1_Yes),
    as.numeric(results_knn_val$F1_Yes)
  ), 3),
  Macro_F1      = round(c(
    as.numeric(results_rlr_val$Macro_F1),
    as.numeric(results_rf_val$Macro_F1),
    as.numeric(results_knn_val$Macro_F1)
  ), 3)
)

val_summary
