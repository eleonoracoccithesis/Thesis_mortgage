## 4. TRAIN REGULARIZED LOGISTIC REGRESSION (RLR) ---------------------------

# 1. LIBRARIES ---------------------------------------------------------------
library(caret)
library(dplyr)
library(tidyr)
library(glmnet)

# 2. SAFE SUMMARY FUNCTION: F1 FOR CLASS "No" --------------------------------
f1Summary <- function(data, lev = NULL, model = NULL) {
  positive <- "No"
  
  # remove rows with NA pred/obs if any
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

# 3. PREPARE TRAINING DATA ---------------------------------------------------
# assumes train_final already exists and is your TRAIN set

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

# Check final class counts
table(train_df_bin$premium_debt_paid_2023)

# 4. CLASS WEIGHTS (HANDLE IMBALANCE) ---------------------------------------
class_counts  <- table(train_df_bin$premium_debt_paid_2023)
class_weights <- 1 / class_counts
class_weights <- class_weights / sum(class_weights)
row_weights   <- class_weights[as.character(train_df_bin$premium_debt_paid_2023)]

# 5. CV SETUP ----------------------------------------------------------------
cv_control <- trainControl(
  method = "cv",
  number = 5,
  classProbs = FALSE,
  savePredictions = TRUE,
  summaryFunction = f1Summary
)

# 6. HYPERPARAMETER GRID -----------------------------------------------------
tune_grid <- expand.grid(
  alpha  = c(0, 0.5, 1),                 # ridge, elastic net, lasso
  lambda = 10^seq(-3, 1, length.out = 10)
)

# 7. TRAIN REGULARIZED LOGISTIC REGRESSION ----------------------------------
set.seed(42)

model_rlr_f1 <- train(
  premium_debt_paid_2023 ~ .,
  data      = train_df_bin,
  method    = "glmnet",
  family    = "binomial",
  weights   = row_weights,
  trControl = cv_control,
  metric    = "F1",
  tuneGrid  = tune_grid,
  na.action = na.omit
)

# 8. INSPECT RESULTS ---------------------------------------------------------
print(model_rlr_f1)

best_hyperparams <- model_rlr_f1$bestTune
print(best_hyperparams)

cv_results_f1 <- model_rlr_f1$resample
print(cv_results_f1)

mean_F1 <- mean(cv_results_f1$F1)
sd_F1   <- sd(cv_results_f1$F1)
cat("\nMean F1:", round(mean_F1, 3),
    " SD F1:", round(sd_F1, 3), "\n")

# 9. T-TEST ON F1 SCORES -----------------------------------------------------
# Example: is mean F1 significantly different from 0.5?
ttest_F1 <- t.test(cv_results_f1$F1, mu = 0.5)
print(ttest_F1)
# If you prefer vs 0, just use: t.test(cv_results_f1$F1, mu = 0)
