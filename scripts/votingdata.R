#R script for generating the data that will be used in the BM424 workshops
#This script will be used to generate the files with voting data, which will be named 
#polling_group[group #].csv, where the group numbers are A-M (we have ~10 groups most years, but generate a 
#few extra just in case...)

# Create a directory to store the CSV files (if it doesn't exist)
if (!file.exists("data_sets")) {
  dir.create("data_sets")
}

# Loop through letters A to M
for (letter in LETTERS[1:13]) {
  # Generate your data set - voting data over the past 5 years for the 5 major political parties in each town

  #set up a random set of 5 political parties for each town, where PossParties is a list of all possible
  #party names, and sample is used to select 5 at random without replacement, and store them in the 
  #variable "Parties"

  PossParties <- c("Sovereign People", "Green Evolution", "Progressive Earth", "Unconditional Unity", 
             "Universal Prosperity", "Revolutionary", "Social Capital", "Environment First", 
             "Wind Power", "Social Redistribution", "Gold", "Vocal Action", "New Progress", 
             "Truth to Power", "Social Redistribution", "New Progress", "Social Change", 
             "Freedom and Green", "Purple", "Thistle", "New Hope", "Wind and Solar Power", 
             "Peace and Prosperity", "Rational Industry", "Social Unity", "Sovereign Democrat", 
             "Future First", "Peace and Change", "Rational Change", "Silver", "Moderate", 
             "Progress and Peace", "Community Change", "Aquamarine", "Modern Union", 
             "Independent Peace", "New Progressive", "Unconditional Green", "Blue and Gold", 
             "Conservative Coalition", "National Renovation", "Progressive Rights", "Unity First", 
             "Maroon", "United Future")

  Parties <- c(sample(PossParties, 5, replace = FALSE))

  #we next will need to set up data with the amount of votes each party has gotten for the past several years
  
  # Get the current year
  current_year <- as.numeric(format(Sys.Date(), "%Y"))
  
  # Create a vector to store the years
  Years <- c(current_year - 4:0)
  
  # Print the Years variable
  print(Years)

  # generate voting data for each party in each of the years
  voting_data <- numeric(0)

  for (Year in Years){
  # Generate four random numbers
  random_numbers <- round(runif(4, min = 0, max = 25), 1)
  
  # Calculate the sum of the first four numbers
  sum_of_four <- sum(random_numbers)
  
  # Calculate the fifth number to make the total sum equal to 100
  fifth_number <- round(100 - sum_of_four, 1)
  
  # Combine the four random numbers and the fifth number
  random_numbers <- c(random_numbers, fifth_number)
  
  # Append the random numbers for the current year to the combined results vector
  voting_data <- c(voting_data, random_numbers)
  }
  

  # Create a data frame with the year, party, and voting data
  
  df <- data.frame(Years = rep(Years, each = 5),
             Parties,
             voting_data)
  
# Save the data set as a CSV file
file_name <- paste0("data_sets/voting_group", letter, ".csv")
write.csv(df, file = file_name, row.names = FALSE)
}





