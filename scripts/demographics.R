#R script for generating the data that will be used in the BM424 workshops
#This script will be used to generate the files with demographics, which will be named 
#demographics_group[group #].csv, where the group numbers are A-M (we have ~10 groups most years, but generate a 
#few extra just in case...)
# Load necessary libraries
library(tidyverse)

# Create a directory to store the CSV files (if it doesn't exist)
if (!file.exists("data_sets")) {
  dir.create("data_sets")
}

# Loop through letters A to M
for (letter in LETTERS[1:13]) {
  # Generate your data set - demographic data for the past 5 years in each town
  
  # Define the Years vector
  # Get the current year
  current_year <- as.numeric(format(Sys.Date(), "%Y"))
  
  # Create a vector to store the years
  Years <- current_year - 4:0
  
  # Initialize an empty data frame to store results
  population_df_long <- data.frame()
  
  # Initialize a variable for the previous year's population
  previous_population <- sample(150000:200000, 1)
  
  # Loop through each year in the Years vector
  for (Year in Years) {
    # If it's not the first year, increase the population
    if (Year != min(Years)) {
      # Increase the population by a random amount (e.g., 1000 to 5000)
      increment <- sample(1000:5000, 1)
      total_population <- previous_population + increment
    } else {
      # For the first year, use the initial random population
      total_population <- previous_population
    }
    
    # Update the previous population for the next iteration
    previous_population <- total_population
    
    # Define the age categories
    age_categories <- c("Age: 0-14", "Age: 15-24", "Age: 25-44", "Age: 45-64", "Age: 65-74", "Age: 75+")
    
    # Define the gender categories
    gender_categories <- c("Gender: Male", "Gender: Female", "Gender: Other")
    
    # Define the employment categories
    employment_categories <- c("Employment: Employed", "Employment: Unemployed", "Employment: Self-Employed")
    
    # Generate age distribution with random noise
    age_proportions <- c(0.08, 0.22, 0.3, 0.25, 0.1, 0.05) * runif(length(age_categories), 0.95, 1.05)
    age_distribution <- round(age_proportions / sum(age_proportions) * total_population)
    age_distribution[length(age_distribution)] <- total_population - sum(age_distribution[-length(age_distribution)])
    
    # Generate gender distribution with random noise
    gender_proportions <- c(0.48, 0.50, 0.02) * runif(length(gender_categories), 0.95, 1.05)
    gender_distribution <- round(gender_proportions / sum(gender_proportions) * total_population)
    gender_distribution[length(gender_distribution)] <- total_population - sum(gender_distribution[-length(gender_distribution)])
    
    # Generate employment status distribution with random noise
    employment_proportions <- c(0.69, 0.09, 0.22) * runif(length(employment_categories), 0.95, 1.05)
    employment_distribution <- round(employment_proportions / sum(employment_proportions) * total_population)
    employment_distribution[length(employment_distribution)] <- total_population - sum(employment_distribution[-length(employment_distribution)])
    
    # Create data frames for age, gender, and employment status categories
    age_data <- data.frame(Year = Year,
                           Category = age_categories,
                           Count = age_distribution)
    
    gender_data <- data.frame(Year = Year,
                              Category = gender_categories,
                              Count = gender_distribution)
    
    employment_data <- data.frame(Year = Year,
                                  Category = employment_categories,
                                  Count = employment_distribution)
    
    # Append the year's data to the combined data frame
    year_data <- rbind(age_data, gender_data, employment_data)
    population_df_long <- rbind(population_df_long, year_data)
  }
  
  # Save the data set as a CSV file
  file_name <- paste0("data_sets/demographics_group", letter, ".csv")
  write.csv(population_df_long, file = file_name, row.names = FALSE)
}

