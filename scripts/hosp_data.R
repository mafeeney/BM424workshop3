#R script for generating the data that will be used in the BM424 workshops
#This script will be used to generate the files with demographic, which will be named 
#demographics_group[group #].csv, where the group numbers are A-M (we have ~10 groups most years, but generate a 
#few extra just in case...)

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
  Years <- c(current_year - 4:0)

  
  
  

  # Save the data set as a CSV file
  file_name <- paste0("data_sets/demographics_group", letter, ".csv")
  write.csv(population_df_long, file = file_name, row.names = FALSE)
}
