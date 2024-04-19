library(readxl) 
#setwd("/Users/gladyswang/Desktop/Spring_2024/DS_Capstone")
sickle_cell <- read.csv("/Users/ritay/Desktop/Capstone/bis687-2024/bis687-2024/curesc_year3_v3.csv")

# filter out variables with same value
print_columns_same_value <- function(df) {
  same_value_columns <- sapply(df, function(col) length(unique(col)) == 1)
  names(df)[same_value_columns]
}
variables_with_same_value <- print_columns_same_value(sickle_cell)
sickle_cell <- sickle_cell[, !names(sickle_cell) %in% variables_with_same_value]

# extract pre-HCT variables
codebook <- read_excel("/Users/ritay/Desktop/Capstone/bis687-2024/bis687-2024/Codebook 2021 Year 3.xlsx")
codebook_prehct <- codebook[codebook['HCT status'] == "pre-HCT",]
completeRows <- complete.cases(codebook_prehct["HCT status"])
codebook_prehct <- codebook_prehct[completeRows, ]
codebook_prehct[codebook_prehct$`Variable name` == "racegp", 'Variable name'] <- 'raceg'
names(sickle_cell) <- tolower(names(sickle_cell))
common_columns <- intersect(names(sickle_cell), codebook_prehct$`Variable name`)
sickle_cell_prehct <- sickle_cell[, common_columns]
names(sickle_cell_prehct) <- toupper(names(sickle_cell_prehct))
sickle_cell_prehct$ACSPSHI <- sickle_cell$acspshi

# remove duplicate variables
duplicate_variables <- c("AGE", "YEARTX", "SCREUNIT", "SCRENUNT", "SALBUNIT", "SALBNUNT", "HB1UNPR")
sickle_cell_prehct <- sickle_cell_prehct[, !names(sickle_cell_prehct) %in% duplicate_variables]

# remove variables with >= 60% missing values
missing_percentage <- function(df, missing_values = c(NA, 99, -9)) {
  total_values <- sapply(df, function(col) length(col))
  missing_count <- sapply(df, function(col) sum(col %in% missing_values))
  missing_percentage <- round((missing_count / total_values) * 100, 2)
  return(missing_percentage)
}

missing_percentages <- missing_percentage(sickle_cell_prehct)
variables_to_remove <- names(missing_percentages)[missing_percentages >= 60]
sickle_cell_cleaned <- sickle_cell_prehct[, !names(sickle_cell_prehct) %in% variables_to_remove]


# remove samples with unrecorded ACSPSHI
sickle_cell_cleaned <- sickle_cell_cleaned[sickle_cell_cleaned$ACSPSHI != 99, ]

library(missForest)
set.seed(12315)

# Perform imputation (this might take some time for larger datasets)
col_names<-c("SCREATPR", "SCREAULN", "HB1PR", "INTSCREPR", "AGEGPFF")
sickle_cell_cleaned[setdiff(names(sickle_cell_cleaned), col_names)] <- lapply(sickle_cell_cleaned[setdiff(names(sickle_cell_cleaned), col_names)], factor)

imputed_data <- missForest(sickle_cell_cleaned)$ximp

sickle_cell_imputed <- imputed_data

