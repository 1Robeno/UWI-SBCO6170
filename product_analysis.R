# Project: Product Promotion Analysis for Supermarket X
# File: product_analysis.R
# Description: Identify best selling products for supermarket X. Identify association rules that exists among the transaction data.

library(data.table)
library(dplyr)
library(ggplot2)
library(arules)
library(arulesViz)

###############################################################################
# Function uses to easily load Supermarket X datasets from Github Repository.
file.loading <- function(file_name) {
  file_name <- paste(file_name, ".rds", sep = "")

  url <- paste("https://github.com/1Robeno/UWI-SBCO6170/raw/master/Project_Data/", file_name, sep = "")
  download.file(url, file_name) # Downloads file to working directory

  data_file <- readRDS(file_name) # Loads file into workspace
  return(data_file)
}


###############################################################################
# Identifying the best selling products
store_sales <- file.loading("Store_Sales_3_Months") # Sales for stores over the specified 3 month period.

item_sales <- store_sales %>%
  group_by(UPC) %>%
  summarize(ItemSales = sum(SalesAmt)) %>%
  arrange(desc(ItemSales)) %>%
  mutate(CumulPercent = cumsum(ItemSales / sum(ItemSales)))

item_sales$ItemRank <- plyr::round_any(item_sales$CumulPercent, .1,
                                       f = ceiling) * 10

item_sales$ItemRank <- factor(item_sales$ItemRank, levels = c(1:10))

# Create bar plot showing Item Count or each Rank
rank_plot1 <- ggplot(item_sales, aes(x = ItemRank)) +
  geom_bar(width = 0.9, fill = "steelblue") +
  labs(x = "Item Rank", y = "Item Count", title = "Item Count by Rank") +
  scale_y_continuous(expand = c(0, 0)) +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.45))

# Table View of rank count
rank_count <- item_sales %>%
  group_by(ItemRank) %>%
  summarize(ItemCount = n())

# Item to be focused on for promotion
mid60_items <- item_sales$UPC[item_sales$ItemRank %in% c(3:8)]
mid60_cnt <- length(mid60_items)
mid60_items_chr <- as.character(mid60_items)


###############################################################################
# Loading transaction dataset
store_trns <- file.loading("Store_Transactions_3_Months")

store_trns$UPC <- as.numeric(store_trns$UPC)
store_trns <- store_trns[!is.na(store_trns$UPC), ]

# Creating composite transaction ID.
store_trns$DateReformat <- format(store_trns$Date, "%Y%m%d")
store_trns$TransID <- paste(store_trns$TransactionNum, store_trns$DateReformat,
                           sep = "")

# Review actual participation .................................................
promo_items <- file.loading("Store_Promotion_Items_2_Months")

store_trns$MonthNo <- as.numeric(format(store_trns$Date, "%m")) # Creating a MonthNo column for merge filtering

# Using filter join to select target transactions
promo_trns <- semi_join(store_trns, promo_items, by = c("MonthNo", "UPC"))

# Calculating transaction percent
promo_trns_cnt <- promo_trns %>%
  group_by(MonthNo, Store) %>%
  summarize(PromoTrnsCount = n_distinct(TransID))

total_trns_cnt <- store_trns %>%
  group_by(MonthNo, Store) %>%
  summarize(TrnsCount = n_distinct(TransID))

cnt_merge <- inner_join(promo_trns_cnt, total_trns_cnt,
                        by= c("MonthNo", "Store"))
cnt_merge$PromoPercent <- cnt_merge$PromoTrnsCount / cnt_merge$TrnsCount

avg_participation <- mean(cnt_merge$PromoPercent)


###############################################################################
# Filtering transaction data.
trns_list <- store_trns[UPC %in% mid60_items, ]
trns_list$UPC <- as.character(trns_list$UPC)

remaining_trns <- length(unique(trns_list$TransID)) / length(unique(store_trns$TransID))

# Converting data to transactions class
trns_list$TransID <- factor(trns_list$TransID)
trns_list <- split(trns_list$UPC, trns_list$TransID)

trns_data <- as(trns_list, "transactions")


###############################################################################
# Initial Rules Assessment ....................................................
trns_rules1 <- apriori(trns_data,
                       parameter = list(supp = 0.0001, conf = 0.0001,
                                        minlen = 2, maxlen = 2,
                                        target="rules"))

summary(trns_rules1)

rules1_df <- as(trns_rules1, "data.frame")
rules1_num <- nrow(rules1_df)

rules1_supp_top <- quantile(rules1_df$support, .75)
rules1_conf_top <- quantile(rules1_df$confidence, .75)

rules1_scatter <- ggplot(rules1_df, aes(x = support, y = confidence)) +
  geom_point(color = "red2", position = "jitter") +
  labs(x = "Support", y = "Confidence",
       title = paste("Scatter plot for", rules1_num, "rules")) +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.45))

# Second Rules Assessment .....................................................
trns_rules2 <- subset(trns_rules1, subset = support >= rules1_supp_top)
trns_rules2 <- subset(trns_rules2, subset = confidence >= rules1_conf_top)
trns_rules2 <- subset(trns_rules2, subset = lhs %in% mid60_items_chr)

summary(trns_rules2)

rules2_df <- DATAFRAME(trns_rules2, setStart='', setEnd='', separate = TRUE)
rules2_num <- nrow(rules2_df)

rules2_scatter <- ggplot(rules2_df, aes(x = support, y = confidence)) +
  geom_point(color = "red2", position = "jitter") +
  labs(x = "Support", y = "Confidence",
       title = paste("Scatter plot for", rules2_num, "rules")) +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.45))


###############################################################################
# Assigning Product names and categories
item_info <- file.loading("Product_Details")

rules2_df$LHS <- as.numeric(as.character(rules2_df$LHS))
rules2_df$RHS <- as.numeric(as.character(rules2_df$RHS))

item_info_mod <- item_info[ , c("UPC", "Description", "Category")] %>%
  setnames(c("LHS", "Antecedent", "LHS_Category"))
rules2_df <- left_join(rules2_df, item_info_mod, by = "LHS")

item_info_mod <- setnames(item_info_mod, c("RHS", "Consequent",
                                           "RHS_Category"))

rules2_df <- left_join(rules2_df, item_info_mod, by = "RHS") %>%
  select(LHS, Antecedent, RHS, Consequent, support, confidence, coverage,
         lift, count, LHS_Category, RHS_Category)

# Filtering data to identifiy rules labelled "Different Category"
rules2_df$Cat_Test <- ifelse(rules2_df$LHS_Category == rules2_df$RHS_Category,
                             "Same Category", "Different Category")

final_tbl <- rules2_df[rules2_df$Cat_Test == "Different Category", ]
unique_antecedent <- unique(final_tbl$LHS)