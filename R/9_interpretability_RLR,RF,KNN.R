#0. LOAD PACKAGES ______________________________________________________________
library(dplyr)
library(ggplot2)
library(broom)
library(pdp)
library(patchwork)
library(tibble)
library(lime)
library(caret)


#1. RLR – COEFFICIENT PLOT _____________________________________________________
# Get coefficients at the best lambda 
best_lambda <- model_rlr_f1$bestTune$lambda

coefs_mat <- coef(model_rlr_f1$finalModel, s = best_lambda)
coefs_df <- data.frame(
  term     = rownames(coefs_mat),
  estimate = as.numeric(coefs_mat)
)

# Keep only non-zero, non-intercept coefficients 
coefs_nonzero <- coefs_df %>%
  filter(term != "(Intercept)") %>%
  filter(abs(estimate) > 1e-6) %>%     
  arrange(desc(abs(estimate)))

n_nonzero <- nrow(coefs_nonzero)
cat("Number of non-zero coefficients:", n_nonzero, "\n")
print(head(coefs_nonzero, 20))  

# Plot top coefficients 
if (n_nonzero == 0) {
  message("No non-zero coefficients at the selected lambda.")
} else {
  coefs_plot <- coefs_nonzero %>% slice_head(n = min(10, n_nonzero))
  
  ggplot(coefs_plot,
         aes(x = reorder(term, estimate), y = estimate)) +
    geom_col(fill = "steelblue") +
    coord_flip() +
    labs(
      title = "Top coefficients – Regularized Logistic Regression",
      x = "Feature (dummy-coded if categorical)",
      y = "Log-odds for class 'No'"
    ) +
    theme_minimal(base_size = 11)
}


#2. RANDOM FOREST – PDPs FOR TOP FEATURES ______________________________________
# Variable importance and top 6 features
imp_rf <- varImp(model_rf_f1, scale = TRUE)$importance %>%
  rownames_to_column("Feature") %>%
  arrange(desc(Overall))

top_feats <- imp_rf$Feature[1:6]   # <- top 6 only

# Training data: predictors only 
X_train_rf <- train_df_bin %>% select(-premium_debt_paid_2023)

# Prediction function returning P(No) 
rf_final <- model_rf_f1$finalModel   

pred_fun_rf <- function(object, newdata) {
  pred_obj <- predict(object, data = newdata)
  p <- pred_obj$predictions
  if (is.factor(p)) {
    as.numeric(p == "No")
  } else if (is.matrix(p) || is.data.frame(p)) {
    as.numeric(p[, "No"])
  } else {
    as.numeric(p)
  }
}

# Build PDP data for each feature and average yhat per grid point
pdp_df <- map_dfr(top_feats, function(f) {
  pd_raw <- partial(
    object   = rf_final,
    pred.var = f,
    train    = X_train_rf,
    pred.fun = pred_fun_rf,
    grid.resolution = 20,
    plot = FALSE
  )
  
  names(pd_raw)[1] <- "value"   # first column = feature values
  
  pd_raw %>%
    group_by(value) %>%
    summarise(yhat = mean(yhat, na.rm = TRUE), .groups = "drop") %>%
    mutate(Feature = f)
})

# keep facets ordered by importance
pdp_df$Feature <- factor(pdp_df$Feature, levels = top_feats)

# Plot
ggplot(pdp_df, aes(x = value, y = yhat)) +
  geom_line(linewidth = 0.9) +
  facet_wrap(~ Feature, scales = "free_x", ncol = 3) +
  labs(
    x = NULL,
    y = "Predicted P(No)"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    strip.text = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )


#3. KNN – LIME EXPLANATIONS ____________________________________________________
# Refit KNN once with best k and probabilities
best_k <- model_knn_f1$bestTune$k

set.seed(42)
ctrl_knn_lime <- trainControl(
  method = "none",
  classProbs = TRUE,
  savePredictions = TRUE
)

model_knn_lime <- train(
  premium_debt_paid_2023 ~ .,
  data      = train_df_bin,
  method    = "knn",
  trControl = ctrl_knn_lime,
  tuneGrid  = data.frame(k = best_k),
  preProcess = c("center", "scale")
)

# Validation predictors (no target) for explanations
X_val_knn <- val_df_bin %>% select(-premium_debt_paid_2023)

# Build LIME explainer
explainer_knn <- lime(
  x     = X_val_knn,
  model = model_knn_lime
)

# Explain first 3 validation observations
lime_expl_knn <- explain(
  X_val_knn[1:3, ],
  explainer = explainer_knn,
  n_features = 5,            
  labels = c("No", "Yes")    
)

# Plot local feature contributions
plot_features(lime_expl_knn)
