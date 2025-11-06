# EDA SCRIPT FOR THESIS: HOUSING AND INCOME PREDICTORS ____________________________

# LOAD PACKAGES ------------------------------------------------------------------
library(dplyr)
library(ggplot2)
library(tidyr)
library(forcats)
library(ggthemes)
library(gridExtra)
library(DescTools)
library(reshape2)
library(ggcorrplot)
library(car)

# 1. TARGET VARIABLE DISTRIBUTION -----------------------------------------------
# Plot distribution of premium_debt_paid_2023
plot_data <- final_data %>%
  mutate(premium_debt_paid_2023 = factor(premium_debt_paid_2023, levels = c(1, 2)))

# Plot
vote_plot <- ggplot(plot_data, aes(x = premium_debt_paid_2023, fill = premium_debt_paid_2023)) +
  geom_bar() +
  geom_text(stat = "count", aes(label = ..count..), vjust = -0.5) +
  scale_fill_manual(values = c("1" = "#2ECC71", "2" = "#E74C3C"),
                    labels = c("1" = "Yes", "2" = "No")) +
  labs(x = "Paid Premium or Debt (2023)", y = "Count", fill = "Target") +
  theme_minimal()

print(vote_plot)


# 2. MISSING VALUE SUMMARY ------------------------------------------------------
missing_df <- final_data %>%
  summarise(across(everything(), ~ sum(is.na(.)))) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "missing_count") %>%
  mutate(missing_percent = round(missing_count / nrow(final_data) * 100, 1)) %>%
  arrange(desc(missing_percent))

write.csv(missing_df, "missing_summary.csv", row.names = FALSE)


# 3. IMPUTATION STRATEGY -----------------------------------------------------
numeric_vars <- grep("^age_\\d{4}$", names(final_data), value = TRUE)

ordinal_vars <- grep("^(satisfaction|ease|expectation|satisfaction_wage)_.*_\\d{4}$", 
                     names(final_data), value = TRUE)

target_var <- "premium_debt_paid_2023"

categorical_vars <- setdiff(names(final_data), 
                            c(numeric_vars, ordinal_vars, target_var))


# Impute numeric: random sample
data_imputed <- final_data

for (col in numeric_vars) {
  missing_idx <- which(is.na(data_imputed[[col]]))
  if (length(missing_idx) > 0) {
    sampled_vals <- sample(data_imputed[[col]][!is.na(data_imputed[[col]])], length(missing_idx), replace = TRUE)
    data_imputed[[col]][missing_idx] <- sampled_vals
  }
}

# Impute ordinal: median
for (col in ordinal_vars) {
  if (any(is.na(data_imputed[[col]]))) {
    data_imputed[[col]][is.na(data_imputed[[col]])] <- median(as.numeric(data_imputed[[col]]), na.rm = TRUE)
  }
}

# Impute categorical: mode
get_mode <- function(x) {
  ux <- na.omit(unique(x))
  ux[which.max(tabulate(match(x, ux)))]
}

for (col in categorical_vars) {
  if (any(is.na(data_imputed[[col]]))) {
    data_imputed[[col]][is.na(data_imputed[[col]])] <- get_mode(data_imputed[[col]])
  }
}

# Save cleaned version
write.csv(data_imputed, "cleaned_data.csv", row.names = FALSE)


# 4. AGE DISTRIBUTION BEFORE AND AFTER IMPUTATION -------------------------------
age_vars <- grep("^age_\\d{4}$", names(final_data), value = TRUE)

age_before <- final_data %>%
  select(all_of(age_vars)) %>%
  pivot_longer(cols = everything(), names_to = "year", values_to = "age") %>%
  mutate(source = "Before")

age_after <- data_imputed %>%
  select(all_of(age_vars)) %>%
  pivot_longer(cols = everything(), names_to = "year", values_to = "age") %>%
  mutate(source = "After")

age_combined <- bind_rows(age_before, age_after)
age_combined$year <- gsub("age_", "", age_combined$year)

# Plot
age_plot <- ggplot(age_combined, aes(x = age, fill = source)) +
  geom_histogram(position = "identity", bins = 40, alpha = 0.6) +
  facet_wrap(~year, scales = "free_y") +
  labs(title = "Age Distribution: Before vs After Imputation", x = "Age", y = "Count") +
  scale_fill_manual(values = c("Before" = "#FF9999", "After" = "#4CAF50")) +
  theme_minimal()

print(age_plot)


# 5. CORRELATION FOR ORDINAL VARIABLES ------------------------------------------
ordinal_2023 <- grep("_2023$", ordinal_vars, value = TRUE)

cor_data <- data_imputed %>%
  select(all_of(ordinal_2023)) %>%
  mutate(across(everything(), as.numeric)) %>%
  drop_na()

spearman_matrix <- cor(cor_data, method = "spearman")

# Plot
ggcorrplot(spearman_matrix, method = "circle", type = "lower",
           lab = TRUE, lab_size = 4,
           colors = c("blue", "white", "red"), tl.cex = 10) +
  theme_minimal()


# 6. CRAMER'S V FOR CATEGORICAL VARIABLES ---------------------------------------
cat_vars_2023 <- grep("_2023$", categorical_vars, value = TRUE)

cramer_data <- data_imputed %>%
  select(all_of(cat_vars_2023)) %>%
  mutate(across(everything(), as.factor)) %>%
  drop_na()

get_cramers_v <- function(x, y) {
  tbl <- table(x, y)
  suppressWarnings(DescTools::CramerV(tbl))
}

cramer_matrix <- outer(cat_vars_2023, cat_vars_2023, Vectorize(function(x, y)
  get_cramers_v(cramer_data[[x]], cramer_data[[y]])))
dimnames(cramer_matrix) <- list(cat_vars_2023, cat_vars_2023)

cramer_df <- melt(cramer_matrix) %>%
  filter(as.numeric(Var1) >= as.numeric(Var2))

# Plot
ggplot(cramer_df, aes(x = Var2, y = Var1, fill = value, label = round(value, 2))) +
  geom_tile(color = "white") +
  geom_text(color = "white", size = 4) +
  scale_fill_gradient2(low = "blue", mid = "purple", high = "red", midpoint = 0.5, limits = c(0, 1)) +
  theme_minimal() +
  labs(x = NULL, y = NULL, title = "Cramer's V - Categorical (2023)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# 7. MULTICOLLINEARITY VIA VIF --------------------------------------------------
predictor_vars <- c(numeric_vars, ordinal_2023)
vif_input <- data_imputed %>%
  select(all_of(predictor_vars)) %>%
  mutate(across(everything(), as.numeric)) %>%
  drop_na()

dummy_target <- rnorm(nrow(vif_input))
vif_model <- lm(dummy_target ~ ., data = vif_input)

vif_values <- vif(vif_model)
vif_df <- data.frame(variable = names(vif_values), VIF = round(vif_values, 2)) %>%
  arrange(desc(VIF))

print(vif_df)
