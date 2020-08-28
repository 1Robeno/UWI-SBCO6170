# Project: Product Promotion Analysis for Supermarket X
# File: error_checking.R
# Description: Identify possible errors from correcting UPC being a character value in the Sales Transactions dataset.

###############################################################################
# Function uses to easily load Supermarket X datasets from Github Repository.
file.loading <- function(file_name) {
  file_name <- paste(file_name, ".rds", sep = "")

  url <- paste("https://github.com/1Robeno/UWI-SBCO6170/raw/master/Project_Data/", file_name, sep = "")
  download.file(url, file_name) # Downloads file to working directory

  data_file <- readRDS(file_name) # Loads file into workspace
  return(data_file)
}

# Testing Character Conversion ################################################
store_trns <- file.loading("Store_Transactions_3_Months") # Transactions Details for the specified 3 month period.

unique_upcs <- data.frame(UPC = unique(store_trns$UPC))
unique_upcs$UPC_asNum <- as.numeric(unique_upcs$UPC)

View(unique_upcs[is.na(unique_upcs$UPC_asNum), ]) # Viewing error UPCs

# Assigning error UPCs to vector
error_upc <- unique_upcs$UPC[is.na(unique_upcs$UPC_asNum)]
trg_trns <- store_trns[store_trns$UPC %in% error_upc, ] # Filtering for error Transactions

# Calculaing what percentage of records were affected.
total_records <- length(store_trns$TransactionNum)
error_records <- length(trg_trns$UPC)

percent_of_set <- error_records / total_records

print(percent_of_set)