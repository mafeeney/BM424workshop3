#R script for generating the data that will be used in the BM424 workshops
#This script will be used to generate the files with budget info, which will be named 
#budget_group[group #].csv, where the group numbers are A-M (we have ~10 groups most years, but generate a 
#few extra just in case...)

#This script should be re-run every year to update the year data (will update automatically - should not
#need any manual edits)

#Load required libraries
library(dplyr)

# Create a directory to store the CSV files (if it doesn't exist)
if (!file.exists("data_sets")) {
  dir.create("data_sets")
}

# Loop through letters A to M
for (letter in LETTERS[1:13]) {
  # Generate your data set - budget data for the past 5 years in each town and forecast for the next two

  # Get the current year
  current_year <- as.numeric(format(Sys.Date(), "%Y"))
  
  # Create a vector to store the years
  Years <- c((current_year - 5):(current_year + 2))
  
  # Initialize an empty data frame to store results
  budget <- data.frame()
  
  # Create a data frame for budget
  budget <- data.frame(
    Year = Years,                         # Add the Year column
    Revenue = NA,                         # Add Revenue column (will be filled later)
    Expenditure = NA,                      # Add Expenditure column (will be filled later)
    Category = NA                          # Add Category column (will be filled later)
  )
  
  # Generate random values for Council Tax for each year
  council_tax <- c(sample(30000:40000, 8))
  
  # Add Council Tax data to the budget data frame
  budget$Revenue <- council_tax
  budget$Category <- "Council Tax"        # Add the category label
  
  # Add Business Rates
  # Generate random values for Business Rates for each year
  business_rates <- c(sample(15000:20000, 8))
  
  # Add Business Rates data to a dataframe
  df <- data.frame(
    Year = Years,
    Revenue = business_rates,
    Expenditure = NA, 
    Category = c("Business rates"))
  
  # Combine data frames by row using bind_rows
  budget <- bind_rows(budget, df)
  
  # Add Govt grants
  # Generate random values for Govt grants for each year
  govt_grants <- c(sample(10000:15000, 8))
  
  # Add Govt grants data to a dataframe
  df <- data.frame(
    Year = Years,
    Revenue = govt_grants,
    Expenditure = NA, 
    Category = c("Government grants"))
  
  # Combine data frames by row using bind_rows
  budget <- bind_rows(budget, df)
  
  # Add Parking Fees/Fines
  # Generate random values for Govt grants for each year
  parking <- c(sample(3000:5000, 8))
  
  # Add Parking Fees/Fines data to a dataframe
  df <- data.frame(
    Year = Years,
    Revenue = parking,
    Expenditure = NA, 
    Category = c("Parking Fees/Fines"))
  
  # Combine data frames by row using bind_rows
  budget <- bind_rows(budget, df)

  # Add Rental Income
  # Generate random values for Rental Income for each year
  rent <- c(sample(4000:5000, 8))
  
  # Add Rental Income data to a dataframe
  df <- data.frame(
    Year = Years,
    Revenue = rent,
    Expenditure = NA, 
    Category = c("Rental Income"))
  
  # Combine data frames by row using bind_rows
  budget <- bind_rows(budget, df)
  
  # Add Investment Income
  # Generate random values for Investment Income for each year
  invest <- c(sample(2000:4000, 8))
  
  # Add Investment Income data to a dataframe
  df <- data.frame(
    Year = Years,
    Revenue = invest,
    Expenditure = NA, 
    Category = c("Investment Income"))
  
  # Combine data frames by row using bind_rows
  budget <- bind_rows(budget, df)
  
  # Add License/Permit Fees
  # Generate random values for License/Permit Fees for each year
  licensefees <- c(sample(6000:8000, 8))
  
  # Add License/Permit Fees data to a dataframe
  df <- data.frame(
    Year = Years,
    Revenue = licensefees,
    Expenditure = NA, 
    Category = c("License/Permit Fees"))
  
  # Combine data frames by row using bind_rows
  budget <- bind_rows(budget, df)
  
  # Add Other Income
  # Generate random values for Other Income for each year
  other <- c(sample(100:300, 8))
  
  # Add Other Income data to a dataframe
  df <- data.frame(
    Year = Years,
    Revenue = other,
    Expenditure = NA, 
    Category = c("Other Income"))
  
  # Combine data frames by row using bind_rows
  budget <- bind_rows(budget, df)
  
  # Add Public Safety Expenditures
  # Generate random values for Public Safety Expenditures for each year
  public <- c(sample(25000:30000, 8))
  
  # Add Public Safety Expenditures data to a dataframe
  df <- data.frame(
    Year = Years,
    Revenue = NA,
    Expenditure = public, 
    Category = c("Public Safety Expenditures"))
  
  # Combine data frames by row using bind_rows
  budget <- bind_rows(budget, df)
  
  # Add Education Expenditures
  # Generate random values for Public Safety Expenditures for each year
  education <- c(sample(15000:20000, 8))
  
  # Add Education Expenditures data to a dataframe
  df <- data.frame(
    Year = Years,
    Revenue = NA,
    Expenditure = education, 
    Category = c("Education Expenditures"))
  
  # Combine data frames by row using bind_rows
  budget <- bind_rows(budget, df)
  
  # Add Transportation/Infrastructure Expenditures
  # Generate random values for Transportation/Infrastructure Expenditures for each year
  transport <- c(sample(10000:15000, 8))
  
  # Add Transportation/Infrastructure Expenditures data to a dataframe
  df <- data.frame(
    Year = Years,
    Revenue = NA,
    Expenditure = transport, 
    Category = c("Transportation/Infrastructure Expenditures"))
  
  # Combine data frames by row using bind_rows
  budget <- bind_rows(budget, df)
  
  # Add Social Services Expenditures
  # Generate random values for Social Services Expenditures for each year
  social <- c(sample(12000:15000, 8))
  
  # Add Social Services Expenditures data to a dataframe
  df <- data.frame(
    Year = Years,
    Revenue = NA,
    Expenditure = social, 
    Category = c("Social Services Expenditures"))
  
  # Combine data frames by row using bind_rows
  budget <- bind_rows(budget, df)
  
  # Add Parks/Recreation Expenditures
  # Generate random values for Parks/Recreation Expenditures for each year
  parks <- c(sample(3000:5000, 8))
  
  # Add Parks/Recreation Expenditures data to a dataframe
  df <- data.frame(
    Year = Years,
    Revenue = NA,
    Expenditure = parks, 
    Category = c("Parks/Recreation Expenditures"))
  
  # Combine data frames by row using bind_rows
  budget <- bind_rows(budget, df)
  
  # Add Public Works Expenditures
  # Generate random values for Public Works Expenditures for each year
  works <- c(sample(6000:9000, 8))
  
  # Add Public Works Expenditures data to a dataframe
  df <- data.frame(
    Year = Years,
    Revenue = NA,
    Expenditure = works, 
    Category = c("Public Works Expenditures"))
  
  # Combine data frames by row using bind_rows
  budget <- bind_rows(budget, df)
  
  # Add Administration Expenditures
  # Generate random values for Administration Expenditures for each year
  admin <- c(sample(6000:9000, 8))
  
  # Add Administration Expenditures data to a dataframe
  df <- data.frame(
    Year = Years,
    Revenue = NA,
    Expenditure = admin, 
    Category = c("Administration Expenditures"))
  
  # Combine data frames by row using bind_rows
  budget <- bind_rows(budget, df)
  
  # Add Health Services Expenditures
  # Generate random values for Health Services Expenditures for each year
  health <- c(sample(4000:7000, 8))
  
  # Add Health Services Expenditures data to a dataframe
  df <- data.frame(
    Year = Years,
    Revenue = NA,
    Expenditure = health, 
    Category = c("Health Services Expenditures"))
  
  # Combine data frames by row using bind_rows
  budget <- bind_rows(budget, df)
  
  # Add Contingency Expenditures
  # Generate random values for Contingency Expenditures for each year
  conting <- c(sample(1000:2000, 8))
  
  # Add Contingency Expenditures data to a dataframe
  df <- data.frame(
    Year = Years,
    Revenue = NA,
    Expenditure = conting, 
    Category = c("Contingency Expenditures"))
  
  # Combine data frames by row using bind_rows
  budget <- bind_rows(budget, df)
  
  
  # Print the updated budget data frame
  print(budget)
  
  
  
  # A couple checks to make sure the data look reasonable
  
  # Check if Expenditure > Revenue for the current year
  # Subset the budget data frame for the current year
  current_year_data <- subset(budget, Year == current_year)
  
  # Calculate total revenue and total expenditure for the current year
  total_revenue <- sum(current_year_data$Revenue, na.rm = TRUE)
  total_expenditure <- sum(current_year_data$Expenditure, na.rm = TRUE)
  
  # Check if Expenditure > Revenue for the current year
  if (total_expenditure > total_revenue) {
    # Write results to a CSV file if condition is met
    print("Expenditure > Revenue")
  } else {
    # Print current expenditure and revenue
    print(paste("Current expenditure:", total_expenditure, "and Current revenue:", total_revenue))
  }
  
  
  
  # Initialize vectors to store total revenue and total expenditure for each year
  total_revenue_per_year <- numeric(length(unique(budget$Year)))
  total_expenditure_per_year <- numeric(length(unique(budget$Year)))
  
  # Loop through each unique year
  for (year in unique(budget$Year)) {
    # Subset the budget data frame for the current year
    current_year_data <- subset(budget, Year == year)
    
    # Calculate total revenue and total expenditure for the current year
    total_revenue <- sum(current_year_data$Revenue, na.rm = TRUE)
    total_expenditure <- sum(current_year_data$Expenditure, na.rm = TRUE)
    
    # Print out the results for the current year
    cat("Year:", year, "\tTotal Revenue:", total_revenue, "\tTotal Expenditure:", total_expenditure, "\n")
  }
  
  # Save the data set as a CSV file
  file_name <- paste0("data_sets/budget_group", letter, ".csv")
  write.csv(budget, file = file_name, row.names = FALSE)
}
    






