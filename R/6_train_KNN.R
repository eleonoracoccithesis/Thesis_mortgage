#0. LOAD PACKAGES ______________________________________________________________
library(caret)
library(dplyr)
library(tidyr)


#1. SUMMARY FUNCTION ___________________________________________________________
f1Summary <- function(data, lev = NULL, model = NULL) {
  positive <- "No"
  data <- data[complete.cases(data[, c("pred", "obs")]), ]
  if (nrow(data) == 0) {
    return(c(Precision = 0, Recall = 0, F1 = 0))
  }
  
  if (is.null(lev)) lev <- levels(data$obs)
  obs  <- factor(data$obs,  levels = lev)
  pred <- factor(data$pred, levels = lev)
  
  cm <- table(obs, pred)
  
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


#2. PREPARE TRAINING DATA ______________________________________________________
train_df_bin <- train_final %>%
  filter(premium_debt_paid_2023 %in% c("1", "2")) %>%
  select(-participant, -year, -split)

# Recode target: "1" = Yes, "2" = No
train_df_bin$premium_debt_paid_2023 <- ifelse(
  train_df_bin$premium_debt_paid_2023 == "2", "No", "Yes"
)
train_df_bin$premium_debt_paid_2023 <- factor(
  train_df_bin$premium_debt_paid_2023,
  levels = c("No", "Yes")
)

# Drop any remaining NAs
train_df_bin <- train_df_bin %>% drop_na()
stopifnot(sum(is.na(train_df_bin)) == 0)

# Check class counts
table(train_df_bin$premium_debt_paid_2023)


#3. CV SETUP ___________________________________________________________________
cv_control <- trainControl(
  method = "cv",
  number = 5,
  classProbs = FALSE,
  savePredictions = TRUE,
  summaryFunction = f1Summary
)


#4. HYPERPARAMETER GRID ________________________________________________________
grid_knn <- expand.grid(
  k = c(3, 5, 7, 9, 11)
)


#5. TRAIN KNN __________________________________________________________________
set.seed(42)

model_knn_f1 <- train(
  premium_debt_paid_2023 ~ .,
  data      = train_df_bin,
  method    = "knn",
  trControl = cv_control,
  metric    = "F1",
  tuneGrid  = grid_knn,
  preProcess = c("center", "scale"),  # very important for KNN
  na.action = na.omit
)


#6. INSPECT RESULTS ____________________________________________________________
print(model_knn_f1)

best_hyperparams_knn <- model_knn_f1$bestTune
print(best_hyperparams_knn)

cv_results_knn <- model_knn_f1$resample
print(cv_results_knn)

mean_F1_knn <- mean(cv_results_knn$F1)
sd_F1_knn   <- sd(cv_results_knn$F1)
cat("\nKNN Mean F1:", round(mean_F1_knn, 3),
    " SD F1:", round(sd_F1_knn, 3), "\n")


#7. T-TEST ON F1 SCORES ________________________________________________________
ttest_F1_knn <- t.test(cv_results_knn$F1, mu = 0.5)
print(ttest_F1_knn)
