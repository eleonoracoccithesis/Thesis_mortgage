#0. GITUHB and LOAD PACKAGES ___________________________________________________
# In RStudio:
install.packages("usethis")
usethis::use_git_config(user.name = "eleonoracoccithesis", 
                        user.email = "e.cocci@tilburguniversity.edu")

# Download packages
library(haven)
library(dplyr)
library(purrr)

#1. MERGE VARIABLES INTO ONE DATASET ___________________________________________
# Set working directory 
setwd("C:/Users/cocci/Downloads/Study material/DSS_thesis_2026/Thesis_2026_R")

# List all .sav files
sav_files <- list.files(path = ".", pattern = "\\.sav$", 
                        recursive = TRUE, full.names = TRUE)

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


#2. SELECT VARIABLES ___________________________________________________________
# Create variable list 
vars_to_keep <- c(
  
  # 2013
  "nomem_encr", "ch13g002", "ch13g004", "ch13g018", 
  "ch13g022", "ch13g100", "cd14g001", "cd14g003", 
  "cd14g015", "cd14g018", "cw14g001", "cw14g128", 
  "cw14g433", "ci14g006", "ci14g007", "ci14g244", 
  "ci14g248", "ci14g252", "ci14g253", "ci14g261", 
  "ci14g293", "ci14g301", "ci14g355",
  
  # 2014
  "ch15h002", "ch15h004", "ch15h018", "ch15h022",
  "ch15h100", "cd15h001", "cd15h003", "cd15h015",
  "cd15h018", "cw15h001", "cw15h128", "cw15h433",
  "ci15h006", "ci15h007", "ci15h244", "ci15h248",
  "ci15h252", "ci15h253", "ci15h261", "ci15h293",
  "ci15h301", "ci15h355",
  
  # 2015
  "ch16i002", "ch16i004", "ch16i018", "ch16i022",
  "ch16i100", "cd16i001", "cd16i003", "cd16i015",
  "cd16i018", "cw16i001", "cw16i128", "cw16i433",
  "ci16i006", "ci16i007", "ci16i244", "ci16i248",
  "ci16i252", "ci16i253", "ci16i261", "ci16i293",
  "ci16i301", "ci16i355",
  
  # 2016
  "ch17j002", "ch17j004", "ch17j018", "ch17j022",
  "ch17j100", "cd17j001", "cd17j003", "cd17j015",
  "cd17j018", "cw17j001", "cw17j128", "cw17j433",
  "ci17j006", "ci17j007", "ci17j244", "ci17j248",
  "ci17j252", "ci17j253", "ci17j261", "ci17j293",
  "ci17j301", "ci17j355",
  
  # 2017
  "ch18k002", "ch18k004", "ch18k018", "ch18k022",
  "ch18k100", "cd18k001", "cd18k003", "cd18k015",
  "cd18k018", "cw18k001", "cw18k128", "cw18k433",
  "ci18k006", "ci18k007", "ci18k244", "ci18k248",
  "ci18k252", "ci18k253", "ci18k261", "ci18k293",
  "ci18k301", "ci18k355",
  
  # 2018
  "ch19l002", "ch19l004", "ch19l018", "ch19l022",
  "ch19l100", "cd19l091", "cd19l003", "cd19l015",
  "cd19l018", "cw19l001", "cw19l128", "cw19l433",
  "ci19l006", "ci19l007", "ci19l378", "ci19l248",
  "ci19l252", "ci19l253", "ci19l261", "ci19l293",
  "ci19l301", "ci19l355",
  
  # 2019
  "ch20m002", "ch20m004", "ch20m018", "ch20m022",
  "ch20m100", "cd20m091", "cd20m003", "cd20m015",
  "cd20m018", "cw20m001", "cw20m128", "cw20m433",
  "ci20m006", "ci20m007", "ci20m378", "ci20m248",
  "ci20m252", "ci20m253", "ci20m261", "ci20m293",
  "ci20m301", "ci20m355",
  
  # 2020
  "ch21n002", "ch21n004", "ch21n018", "ch21n022",
  "ch21n100", "cd21n091", "cd21n003", "cd21n015",
  "cd21n018", "cw21n001", "cw21n128", "cw21n433",
  "ci21n006", "ci21n007", "ci21n378", "ci21n248",
  "ci21n252", "ci21n253", "ci21n261", "ci21n293",
  "ci21n301", "ci21n355",
  
  # 2021
  "ch22o002", "ch22o004", "ch22o018", "ch22o022",
  "ch22o100", "cd22o091", "cd22o003", "cd22o015",
  "cd22o018", "cw22o001", "cw22o128", "cw22o433",
  "ci22o006", "ci22o007", "ci22o378", "ci22o248",
  "ci22o252", "ci22o253", "ci22o261", "ci22o293",
  "ci22o301", "ci22o355",
  
  # 2022
  "ch23p002", "ch23p004", "ch23p018", "ch23p022",
  "ch23p100", "cd23p091", "cd23p003", "cd23p015",
  "cd23p018", "cw23p001", "cw23p128", "cw23p433",
  "ci23p006", "ci23p007", "ci23p378", "ci23p248",
  "ci23p252", "ci23p253", "ci23p261", "ci23p293",
  "ci23p301", "ci23p355",
  
  # 2023
  "ch24q002", "ch24q004", "ch24q018", "ch24q022",
  "ch24q100", "cd24q091", "cd24q003", "cd24q015",
  "cd24q018", "cw24q001", "cw24q128", "cw24q433",
  "ci24q006", "ci24q007", "ci24q378", "ci24q248",
  "ci24q252", "ci24q253", "ci24q261", "ci24q293",
  "ci24q301", "ci24q355"
  
)

# Filter the merged data
filtered_data <- merged_data %>%
  select(any_of(vars_to_keep))  # Safely select only these columns

# Preview
print(dim(filtered_data))
head(filtered_data)


#4. CHANGE VARIABLES NAMES FOR CLARITY _________________________________________
rename_vector <- c(
  
  "participant" = "nomem_encr",
  
  # 2013
  "age_2013" = "ch13g002",
  "health_description_2013" = "ch13g004",
  "longstanding_disease_2013" = "ch13g018",
  "ability_work_2013" = "ch13g022",
  "normal_partly_work_2013" = "ch13g100",
  "satisfaction_dwelling_2013" = "cd14g001",
  "tenant_subtenant_owner_2013" = "cd14g003",
  "mortgage_dwelling_2013" = "cd14g015",
  "premium_debt_paid_2013" = "cd14g018",
  "paid_job_2013" = "cw14g001",
  "satisfaction_wage_2013" = "cw14g128",
  "sufficient_salary_2013" = "cw14g433",
  "satisfaction_financial_situation_2013" = "ci14g006",
  "satisfaction_economic_situation_NL_2013" = "ci14g007",
  "ease_living_income_2013" = "ci14g244",
  "running_behind_mortgage_2013" = "ci14g248",
  "description_financial_situation_2013" = "ci14g252",
  "household_expenditure_2013" = "ci14g253",
  "expectation_financial_situation_2013" = "ci14g261",
  "arrears_mortgage_2013" = "ci14g293",
  "arrears_mortgage_two_months_2013" = "ci14g301",
  "ease_covering_500euro_expense_2013" = "ci14g355",
  
  # 2014
  "age_2014" = "ch15h002",
  "health_description_2014" = "ch15h004",
  "longstanding_disease_2014" = "ch15h018",
  "ability_work_2014" = "ch15h022",
  "normal_partly_work_2014" = "ch15h100",
  "satisfaction_dwelling_2014" = "cd15h001",
  "tenant_subtenant_owner_2014" = "cd15h003",
  "mortgage_dwelling_2014" = "cd15h015",
  "premium_debt_paid_2014" = "cd15h018",
  "paid_job_2014" = "cw15h001",
  "satisfaction_wage_2014" = "cw15h128",
  "sufficient_salary_2014" = "cw15h433",
  "satisfaction_financial_situation_2014" = "ci15h006",
  "satisfaction_economic_situation_NL_2014" = "ci15h007",
  "ease_living_income_2014" = "ci15h244",
  "running_behind_mortgage_2014" = "ci15h248",
  "description_financial_situation_2014" = "ci15h252",
  "household_expenditure_2014" = "ci15h253",
  "expectation_financial_situation_2014" = "ci15h261",
  "arrears_mortgage_2014" = "ci15h293",
  "arrears_mortgage_two_months_2014" = "ci15h301",
  "ease_covering_500euro_expense_2014" = "ci15h355",
  
  # 2015
  "age_2015" = "ch16i002",
  "health_description_2015" = "ch16i004",
  "longstanding_disease_2015" = "ch16i018",
  "ability_work_2015" = "ch16i022",
  "normal_partly_work_2015" = "ch16i100",
  "satisfaction_dwelling_2015" = "cd16i001",
  "tenant_subtenant_owner_2015" = "cd16i003",
  "mortgage_dwelling_2015" = "cd16i015",
  "premium_debt_paid_2015" = "cd16i018",
  "paid_job_2015" = "cw16i001",
  "satisfaction_wage_2015" = "cw16i128",
  "sufficient_salary_2015" = "cw16i433",
  "satisfaction_financial_situation_2015" = "ci16i006",
  "satisfaction_economic_situation_NL_2015" = "ci16i007",
  "ease_living_income_2015" = "ci16i244",
  "running_behind_mortgage_2015" = "ci16i248",
  "description_financial_situation_2015" = "ci16i252",
  "household_expenditure_2015" = "ci16i253",
  "expectation_financial_situation_2015" = "ci16i261",
  "arrears_mortgage_2015" = "ci16i293",
  "arrears_mortgage_two_months_2015" = "ci16i301",
  "ease_covering_500euro_expense_2015" = "ci16i355",
  
  
  # 2016
  "age_2016" = "ch17j002",
  "health_description_2016" = "ch17j004",
  "longstanding_disease_2016" = "ch17j018",
  "ability_work_2016" = "ch17j022",
  "normal_partly_work_2016" = "ch17j100",
  "satisfaction_dwelling_2016" = "cd17j001",
  "tenant_subtenant_owner_2016" = "cd17j003",
  "mortgage_dwelling_2016" = "cd17j015",
  "premium_debt_paid_2016" = "cd17j018",
  "paid_job_2016" = "cw17j001",
  "satisfaction_wage_2016" = "cw17j128",
  "sufficient_salary_2016" = "cw17j433",
  "satisfaction_financial_situation_2016" = "ci17j006",
  "satisfaction_economic_situation_NL_2016" = "ci17j007",
  "ease_living_income_2016" = "ci17j244",
  "running_behind_mortgage_2016" = "ci17j248",
  "description_financial_situation_2016" = "ci17j252",
  "household_expenditure_2016" = "ci17j253",
  "expectation_financial_situation_2016" = "ci17j261",
  "arrears_mortgage_2016" = "ci17j293",
  "arrears_mortgage_two_months_2016" = "ci17j301",
  "ease_covering_500euro_expense_2016" = "ci17j355",
  
  
  # 2017
  "age_2017" = "ch18k002",
  "health_description_2017" = "ch18k004",
  "longstanding_disease_2017" = "ch18k018",
  "ability_work_2017" = "ch18k022",
  "normal_partly_work_2017" = "ch18k100",
  "satisfaction_dwelling_2017" = "cd18k001",
  "tenant_subtenant_owner_2017" = "cd18k003",
  "mortgage_dwelling_2017" = "cd18k015",
  "premium_debt_paid_2017" = "cd18k018",
  "paid_job_2017" = "cw18k001",
  "satisfaction_wage_2017" = "cw18k128",
  "sufficient_salary_2017" = "cw18k433",
  "satisfaction_financial_situation_2017" = "ci18k006",
  "satisfaction_economic_situation_NL_2017" = "ci18k007",
  "ease_living_income_2017" = "ci18k244",
  "running_behind_mortgage_2017" = "ci18k248",
  "description_financial_situation_2017" = "ci18k252",
  "household_expenditure_2017" = "ci18k253",
  "expectation_financial_situation_2017" = "ci18k261",
  "arrears_mortgage_2017" = "ci18k293",
  "arrears_mortgage_two_months_2017" = "ci18k301",
  "ease_covering_500euro_expense_2017" = "ci18k355",
  
  # 2018
  "age_2018" = "ch19l002",
  "health_description_2018" = "ch19l004",
  "longstanding_disease_2018" = "ch19l018",
  "ability_work_2018" = "ch19l022",
  "normal_partly_work_2018" = "ch19l100",
  "satisfaction_dwelling_2018" = "cd19l091",  #!
  "tenant_subtenant_owner_2018" = "cd19l003",
  "mortgage_dwelling_2018" = "cd19l015",
  "premium_debt_paid_2018" = "cd19l018",
  "paid_job_2018" = "cw19l001",
  "satisfaction_wage_2018" = "cw19l128",
  "sufficient_salary_2018" = "cw19l433",
  "satisfaction_financial_situation_2018" = "ci19l006",
  "satisfaction_economic_situation_NL_2018" = "ci19l007",
  "ease_living_income_2018" = "ci19l378",  #!
  "running_behind_mortgage_2018" = "ci19l248",
  "description_financial_situation_2018" = "ci19l252",
  "household_expenditure_2018" = "ci19l253",
  "expectation_financial_situation_2018" = "ci19l261",
  "arrears_mortgage_2018" = "ci19l293",
  "arrears_mortgage_two_months_2018" = "ci19l301",
  "ease_covering_500euro_expense_2018" = "ci19l355",
  
  # 2019
  "age_2019" = "ch20m002",
  "health_description_2019" = "ch20m004",
  "longstanding_disease_2019" = "ch20m018",
  "ability_work_2019" = "ch20m022",
  "normal_partly_work_2019" = "ch20m100",
  "satisfaction_dwelling_2019" = "cd20m091",  #!
  "tenant_subtenant_owner_2019" = "cd20m003",
  "mortgage_dwelling_2019" = "cd20m015",
  "premium_debt_paid_2019" = "cd20m018",
  "paid_job_2019" = "cw20m001",
  "satisfaction_wage_2019" = "cw20m128",
  "sufficient_salary_2019" = "cw20m433",
  "satisfaction_financial_situation_2019" = "ci20m006",
  "satisfaction_economic_situation_NL_2019" = "ci20m007",
  "ease_living_income_2019" = "ci20m378",  #!
  "running_behind_mortgage_2019" = "ci20m248",
  "description_financial_situation_2019" = "ci20m252",
  "household_expenditure_2019" = "ci20m253",
  "expectation_financial_situation_2019" = "ci20m261",
  "arrears_mortgage_2019" = "ci20m293",
  "arrears_mortgage_two_months_2019" = "ci20m301",
  "ease_covering_500euro_expense_2019" = "ci20m355",
  
  # 2020
  "age_2020" = "ch21n002",
  "health_description_2020" = "ch21n004",
  "longstanding_disease_2020" = "ch21n018",
  "ability_work_2020" = "ch21n022",
  "normal_partly_work_2020" = "ch21n100",
  "satisfaction_dwelling_2020" = "cd21n091",  #!
  "tenant_subtenant_owner_2020" = "cd21n003",
  "mortgage_dwelling_2020" = "cd21n015",
  "premium_debt_paid_2020" = "cd21n018",
  "paid_job_2020" = "cw21n001",
  "satisfaction_wage_2020" = "cw21n128",
  "sufficient_salary_2020" = "cw21n433",
  "satisfaction_financial_situation_2020" = "ci21n006",
  "satisfaction_economic_situation_NL_2020" = "ci21n007",
  "ease_living_income_2020" = "ci21n378",  #!
  "running_behind_mortgage_2020" = "ci21n248",
  "description_financial_situation_2020" = "ci21n252",
  "household_expenditure_2020" = "ci21n253",
  "expectation_financial_situation_2020" = "ci21n261",
  "arrears_mortgage_2020" = "ci21n293",
  "arrears_mortgage_two_months_2020" = "ci21n301",
  "ease_covering_500euro_expense_2020" = "ci21n355",
  
  # 2021
  "age_2021" = "ch22o002",
  "health_description_2021" = "ch22o004",
  "longstanding_disease_2021" = "ch22o018",
  "ability_work_2021" = "ch22o022",
  "normal_partly_work_2021" = "ch22o100",
  "satisfaction_dwelling_2021" = "cd22o091",  #!
  "tenant_subtenant_owner_2021" = "cd22o003",
  "mortgage_dwelling_2021" = "cd22o015",
  "premium_debt_paid_2021" = "cd22o018",
  "paid_job_2021" = "cw22o001",
  "satisfaction_wage_2021" = "cw22o128",
  "sufficient_salary_2021" = "cw22o433",
  "satisfaction_financial_situation_2021" = "ci22o006",
  "satisfaction_economic_situation_NL_2021" = "ci22o007",
  "ease_living_income_2021" = "ci22o378",  #!
  "running_behind_mortgage_2021" = "ci22o248",
  "description_financial_situation_2021" = "ci22o252",
  "household_expenditure_2021" = "ci22o253",
  "expectation_financial_situation_2021" = "ci22o261",
  "arrears_mortgage_2021" = "ci22o293",
  "arrears_mortgage_two_months_2021" = "ci22o301",
  "ease_covering_500euro_expense_2021" = "ci22o355",
  
  # 2022
  "age_2022" = "ch23p002",
  "health_description_2022" = "ch23p004",
  "longstanding_disease_2022" = "ch23p018",
  "ability_work_2022" = "ch23p022",
  "normal_partly_work_2022" = "ch23p100",
  "satisfaction_dwelling_2022" = "cd23p091",  #!
  "tenant_subtenant_owner_2022" = "cd23p003",
  "mortgage_dwelling_2022" = "cd23p015",
  "premium_debt_paid_2022" = "cd23p018",
  "paid_job_2022" = "cw23p001",
  "satisfaction_wage_2022" = "cw23p128",
  "sufficient_salary_2022" = "cw23p433",
  "satisfaction_financial_situation_2022" = "ci23p006",
  "satisfaction_economic_situation_NL_2022" = "ci23p007",
  "ease_living_income_2022" = "ci23p378",  #!
  "running_behind_mortgage_2022" = "ci23p248",
  "description_financial_situation_2022" = "ci23p252",
  "household_expenditure_2022" = "ci23p253",
  "expectation_financial_situation_2022" = "ci23p261",
  "arrears_mortgage_2022" = "ci23p293",
  "arrears_mortgage_two_months_2022" = "ci23p301",
  "ease_covering_500euro_expense_2022" = "ci23p355",
  
  # 2023
  "age_2023" = "ch24q002",
  "health_description_2023" = "ch24q004",
  "longstanding_disease_2023" = "ch24q018",
  "ability_work_2023" = "ch24q022",
  "normal_partly_work_2023" = "ch24q100",
  "satisfaction_dwelling_2023" = "cd24q091",  #!
  "tenant_subtenant_owner_2023" = "cd24q003",
  "mortgage_dwelling_2023" = "cd24q015",
  "premium_debt_paid_2023" = "cd24q018",
  "paid_job_2023" = "cw24q001",
  "satisfaction_wage_2023" = "cw24q128",
  "sufficient_salary_2023" = "cw24q433",
  "satisfaction_financial_situation_2023" = "ci24q006",
  "satisfaction_economic_situation_NL_2023" = "ci24q007",
  "ease_living_income_2023" = "ci24q378",  #!
  "running_behind_mortgage_2023" = "ci24q248",
  "description_financial_situation_2023" = "ci24q252",
  "household_expenditure_2023" = "ci24q253",
  "expectation_financial_situation_2023" = "ci24q261",
  "arrears_mortgage_2023" = "ci24q293",
  "arrears_mortgage_two_months_2023" = "ci24q301",
  "ease_covering_500euro_expense_2023" = "ci24q355"
  
)

# Apply rename
filtered_data <- filtered_data %>% rename(any_of(rename_vector))

# Preview
head(filtered_data)

