# Axe Capital Research
# Project: Product Promotion Analysis for Supermarket X
# File: data_testing.R
# Description: Reviewing the data provided to us by Supermarket X.

###############################################################################
# Function uses to easily load Supermarket X datasets from Github Repository.
file.loading <- function(file_name) {
  file_name <- paste(file_name, ".rds", sep = "")

  url <- paste("https://github.com/1Robeno/UWI-SBCO6170/raw/master/Project_Data/", file_name, sep = "") # Assign file dowlaod url o variable/
  download.file(url, file_name) # Downloads file to working directory

  data_file <- readRDS(file_name) # Loads file into workspace
  return(data_file)
}


# Load Available Datasets #####################################################
item_info <- file.loading("Product_Details") # Product Descriptions
special_items <- file.loading("Store_Promotion_Items_2_Months") # Items Listed on preivous Monthly Specials
store_sales <- file.loading("Store_Sales_3_Months") # Sales for stores over the specified 3 month period.
store_trns <- file.loading("Store_Transactions_3_Months") # Transactions Details for the specified 3 month period.

# Investigating the structue of each dataset
str(item_info)
str(special_items)
str(store_sales)
str(store_trns)