#1. MERGE VARIABLES INTO ONE DATASET ___________________________________________
# Download packages
library(haven)
library(dplyr)
library(purrr)

# Set working directory 
setwd("C:/Users/cocci/Downloads/Study material/DSS_thesis_2026/Thesis_2026_R")

# List all .sav files
sav_files <- list.files(path = ".", pattern = "\\.sav$", recursive = TRUE, full.names = TRUE)

# Read each file and ensure nomem_encr exists
read_file_safe <- function(file) {
  df <- read_sav(file)
  if (!"nomem_encr" %in% names(df)) {
    stop(paste("Missing 'nomem_encr' in:", file))
  }
  return(df)
}

# Read and merge all files by nomem_encr
merged_data <- sav_files %>%
  map(read_file_safe) %>%
  reduce(full_join, by = "nomem_encr")


# In RStudio:
install.packages("usethis")
usethis::use_git_config(user.name = "Eleonora Cocci", user.email = "e.cocci@tilburguniversity.edu")
hello
