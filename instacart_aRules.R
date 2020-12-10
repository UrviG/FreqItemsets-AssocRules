# Import necessary libraries.
require(arules)

# If using RStudio (interactive session), prompt user to enter path to instacart directory.
# Otherwise, the user must pass this path as an argument when running the script.
if(interactive() == TRUE) {
  data_directory <- readline(prompt = "Enter path to 'instacart-market-basket-analysis' directory: ")
} else{
  args = commandArgs(trailingOnly=TRUE)
  if(length(args) != 1) {
    stop("Please pass path to 'instacart-market-basket-analysis' directory as argument!")
  }
  data_directory = args[1]
}


# Read in data from files.
orders <- read.csv(paste(data_directory, "order_products__train.csv", sep = "/"))
products <- read.csv(paste(data_directory, "products.csv", sep = "/"))
departments <- read.csv(paste(data_directory, "departments.csv", sep = "/"))


# Merge with products & departments datasets with orders to get names.
orders <- merge(orders, products, by = "product_id")
orders <- merge(orders, departments, by = "department_id")


# Get only important columns: order_id, product_name, and department.
# Change order_id field type to factor (category).
orders <- orders[c("order_id", "product_name", "department")]
orders$order_id <- as.factor(orders$order_id)


# split(x, f): rearrange data in x according to factory (y)
# Rearrange products so that each order_id is associated with a list of products.
# Rearrange departments so that each order_id is associated with a list of departments.
prod_transactions <- split(orders$product_name, orders$order_id)
dept_transactions <- split(orders$department, orders$order_id)


# Convert to transactions. 
prod_transactions <- as(prod_transactions, "transactions")
dept_transactions <- as(dept_transactions, "transactions")


# 1. Frequent itemsets for products.
frequent_products <- eclat (prod_transactions,
                            parameter = list(supp = 0.04),
                            control = list(verbose = FALSE))
cat("\n\nFrequent itemsets for products:\n")
inspect(frequent_products)
cat("\n\n")


# 2. Association rules for products
rules_products <- apriori(prod_transactions,
                          parameter = list(supp = 0.001, conf = 0.5),
                          control = list(verbose = FALSE))
rules_products_lift <- sort (rules_products, by="lift",
                             decreasing=TRUE)
cat("\n\nAssociation rules for products:\n")
inspect(rules_products_lift)
cat("\n\n")


# 3. Frequent itemsets for departments
frequent_departments <- eclat (dept_transactions,
                               parameter = list(supp = 0.3),
                               control = list(verbose = FALSE))
cat("\n\nFrequent itemsets for departments:\n")
inspect(frequent_departments)
cat("\n")


# 4. Association rules for departments
rules_deparments <- apriori(dept_transactions,
                            parameter = list(supp = 0.1, conf = 0.88),
                            control = list(verbose = FALSE))
rules_deparments_lift <- sort (rules_deparments, by="lift",
                               decreasing=TRUE)
cat("\n\nAssociation rules for departments:\n")
inspect(rules_deparments_lift)
cat("\n\n")