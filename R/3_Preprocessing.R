#0. LOAD PACKAGES ______________________________________________________________
library(tidyverse)
library(caret)


#1. LOAD DATA _________________________________________________________________
cleaned_data <- read_csv("C:/Users/cocci/Downloads/Study material/DSS_thesis_2026/Thesis_2026_R/R/cleaned_data.csv")


#2. KEEP ALL PREMIUM-DEBT HISTORY, EXCLUDE FROM PIVOT __________________________
debt_static <- cleaned_data %>%
  select(participant, premium_debt_paid_2023)


#3. PIVOT YEAR-SPECIFIC FEATURES INTO LONG FORMAT ______________________________
long_data <- cleaned_data %>%
  select(-matches("^premium_debt_paid_20(1[3-9]|2[0-3])$")) %>%
  pivot_longer(
    cols = matches("_20(1[3-9]|2[0-3])$"),
    names_to = c("feature", "year"),
    names_pattern = "^(.*)_(\\d{4})$"
  ) %>%
  pivot_wider(names_from = feature, values_from = value) %>%
  mutate(year = as.integer(year)) %>%
  left_join(debt_static, by = "participant")


#4. FIX LEAKAGE: MAKE SURE TARGET IS ONLY 2023 _________________________________
long_data <- long_data %>%
  mutate(
    premium_debt_paid_2023 = ifelse(is.na(premium_debt_paid_2023), "Unknown", as.character(premium_debt_paid_2023))
  ) %>%
  filter(premium_debt_paid_2023 != "Unknown")


#5. ASSIGN TEMPORAL SPLIT ______________________________________________________
long_data <- long_data %>%
  mutate(split = case_when(
    year %in% 2013:2016 ~ "train",
    year %in% 2017:2020 ~ "val",
    year %in% 2021:2022 ~ "test",
    year == 2023        ~ "final_test"
  ))


#6. STANDARDIZE ALL NUMERIC FEATURES ___________________________________________
exclude_cols <- c("participant", "year", "split", "premium_debt_paid_2023")

# (robust: exclude any premium_debt_paid_* just in case)
exclude_cols <- unique(c(
  exclude_cols,
  grep("^premium_debt_paid_", names(long_data), value = TRUE)
))

feature_cols <- setdiff(names(long_data), exclude_cols)

preproc <- preProcess(
  long_data %>% filter(split == "train") %>% select(all_of(feature_cols)),
  method = c("center", "scale")
)

scaled_features <- predict(preproc, long_data[, feature_cols])

preprocessed_data <- bind_cols(long_data[, exclude_cols], scaled_features)


#7. FINAL DATASET MERGED _______________________________________________________
preprocessed_data <- bind_cols(long_data[, exclude_cols], scaled_features)


#8. CREATE SPLIT FILES _________________________________________________________
train_final <- preprocessed_data %>% filter(split == "train") 
val_final   <- preprocessed_data %>% filter(split == "val") 
test_final  <- preprocessed_data %>% filter(split == "test")
final_test_2023 <- preprocessed_data %>% filter(split == "final_test")

#9. SAVE FINAL FILES __________________________________________________________
write_csv(train_final, "train_final.csv")
write_csv(val_final, "val_final.csv")
write_csv(test_final, "test_final.csv")
write_csv(final_test_2023, "final_test_2023.csv")

