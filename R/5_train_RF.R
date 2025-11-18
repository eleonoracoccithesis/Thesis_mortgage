# 1. LIBRARIES ---------------------------------------------------------------
library(caret)
library(dplyr)
library(tidyr)
library(ranger)

# 2. SAFE SUMMARY FUNCTION (same as for RLR) ---------------------------------
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

# 3. PREPARE TRAINING DATA (same as for RLR) --------------------------------
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

# Check class counts
table(train_df_bin$premium_debt_paid_2023)

# 4. CLASS WEIGHTS (handle imbalance) ---------------------------------------
class_counts  <- table(train_df_bin$premium_debt_paid_2023)
class_weights <- 1 / class_counts
class_weights <- class_weights / sum(class_weights)
row_weights   <- class_weights[as.character(train_df_bin$premium_debt_paid_2023)]

# Sanity check
stopifnot(length(row_weights) == nrow(train_df_bin))

# 5. CV SETUP ----------------------------------------------------------------
cv_control <- trainControl(
  method = "cv",
  number = 5,
  classProbs = FALSE,
  savePredictions = TRUE,
  summaryFunction = f1Summary
)

# 6. HYPERPARAMETER GRID FOR RF (ranger) ------------------------------------
p <- ncol(train_df_bin) - 1  # number of predictors

grid_rf <- expand.grid(
  mtry          = pmax(1, floor(c(0.25, 0.5, 0.75) * p)),
  splitrule     = "gini",
  min.node.size = c(1, 5, 10)
)

# 7. TRAIN RANDOM FOREST -----------------------------------------------------
set.seed(42)

model_rf_f1 <- train(
  premium_debt_paid_2023 ~ .,
  data      = train_df_bin,
  method    = "ranger",
  trControl = cv_control,
  metric    = "F1",
  tuneGrid  = grid_rf,
  weights   = row_weights,        # <-- key change: use 'weights', not 'case.weights'
  importance = "impurity",
  na.action = na.omit
)

# 8. INSPECT RESULTS ---------------------------------------------------------
print(model_rf_f1)

best_hyperparams_rf <- model_rf_f1$bestTune
print(best_hyperparams_rf)

cv_results_rf <- model_rf_f1$resample
print(cv_results_rf)

mean_F1_rf <- mean(cv_results_rf$F1)
sd_F1_rf   <- sd(cv_results_rf$F1)
cat("\nRF Mean F1:", round(mean_F1_rf, 3),
    " SD F1:", round(sd_F1_rf, 3), "\n")

# 9. T-TEST ON F1 SCORES (optional) -----------------------------------------
ttest_F1_rf <- t.test(cv_results_rf$F1, mu = 0.5)
print(ttest_F1_rf)
