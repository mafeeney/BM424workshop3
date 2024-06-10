#R script for generating the data that will be used in the BM424 workshops
#This script will be used to generate the files with demographic, which will be named 
#demographics_group[group #].csv, where the group numbers are A-M (we have ~10 groups most years, but generate a 
#few extra just in case...)

# Load required libraries
library(dplyr)

# Create a directory to store the CSV files (if it doesn't exist)
if (!file.exists("data_sets")) {
  dir.create("data_sets")
}

# Define the pathogen names:
# List of pathogens corresponding to letters A-K
pathogens <- c("Legionellosis", "Influenza", "MRSA infection", "Campylobacter infection", "Listeriosis", 
               "Meningitis", "Tuberculosis", "Group A Strep", "Anthrax", "Whooping cough", "Acinetobacter infection", 
               "Aspergillosis")

# Loop through the different pathogens 
for (i in 1:length(pathogens)) {
  # Generate your data set
  
  # Define the pathogen name
  pathogen <- pathogens[i]
  
  # Probabilities for each cause of death based on general data for deaths in Scotland
  probabilities <- c(0.133, 0.15, 0.062, 0.081, 0.117, 0.022, 0.032, 0.024, 0.001, 0.018, 0.01, 0.06)
  
  # Generate fake data for the dataset
  data <- tibble(
    ID = sample(1:20000000, 1000, replace=FALSE),  # ID number
    Date_of_Birth = sample(seq(as.Date('1950/01/01'), as.Date('2000/01/01'), by="day"), 1000, replace = TRUE),  # Date of birth
    # Note: Date of death values should be updated annually
    Date_of_Death = sample(seq(as.Date('2025/02/01'), as.Date('2025/02/28'), by="day"), 1000, replace = TRUE),  # Date of death
    Cause_of_Death = sample(c("Heart Disease", "Cancer", "Stroke", "Respiratory Disease", "Accident", "Other"), 1000, replace = TRUE),
    Gender = sample(c("Male", "Female"), 1000, replace = TRUE),  # Gender
    Race_Ethnicity = sample(c("White", "Asian", "Black", "Other"), 1000, replace = TRUE, prob = c(0.96, 0.02, 0.01, 0.01)),  # Race/Ethnicity adjusted for Scottish census data
    Marital_Status = sample(c("Married", "Single", "Divorced", "Widowed"), 1000, replace = TRUE),  # Marital status
    Occupation = sample(c("Professional", "Blue Collar", "Service", "Homemaker", "Unemployed", "Other"), 1000, replace = TRUE),  # Occupation
    Education_Level = sample(c("Less than High School", "High School Graduate", "Some College", "College Graduate", "Postgraduate"), 1000, replace = TRUE)  # Education level
  )
  
  # Add manually generated deaths due to the pathogen
  # Let's add num_deaths deaths due to the pathogen in the first few days of February 2025
  
  num_deaths1 <- sample(1:4, 1)  # Randomly choose the number of deaths between 1 and 4
  pathogen_deaths1 <- tibble(
    ID = sample(data$ID, num_deaths1, replace = FALSE),  # Select num_deaths1 random IDs
    Date_of_Birth = sample(data$Date_of_Birth, num_deaths1),  # Match Date_of_Birth
    Date_of_Death = as.Date(sample(c("2025-02-01", "2025-02-02", "2025-02-03", "2025-02-04", "2025-02-05"), 
                                   num_deaths1, replace = TRUE)), # Manually set date of death
    Cause_of_Death = rep(pathogen, num_deaths1),  # Set Cause_of_Death to the chosen pathogen
    Gender = sample(data$Gender, num_deaths1),  # Match Gender
    Race_Ethnicity = sample(data$Race_Ethnicity, num_deaths1),  # Match Race_Ethnicity
    Marital_Status = sample(data$Marital_Status, num_deaths1),  # Match Marital_Status
    Occupation = sample(data$Occupation, num_deaths1),  # Match Occupation
    Education_Level = sample(data$Education_Level, num_deaths1)  # Match Education_Level
  )
  
  # Combine the original data with the manually generated deaths
  data <- bind_rows(data, pathogen_deaths1)
  
  # Let's add num_deaths2 deaths due to the pathogen in the next few days of February 2025
  
  num_deaths2 <- sample(2:7, 1)  # Randomly choose the number of deaths between 2 and 7
  pathogen_deaths2 <- tibble(
    ID = sample(data$ID, num_deaths2, replace = FALSE),  # Select num_deaths2 random IDs
    Date_of_Birth = sample(data$Date_of_Birth, num_deaths2),  # Match Date_of_Birth
    Date_of_Death = as.Date(sample(c("2025-02-06", "2025-02-07", "2025-02-08", "2025-02-09", "2025-02-10"), 
                                   num_deaths2, replace = TRUE)), # Manually set date of death
    Cause_of_Death = rep(pathogen, num_deaths2),  # Set Cause_of_Death to the chosen pathogen
    Gender = sample(data$Gender, num_deaths2),  # Match Gender
    Race_Ethnicity = sample(data$Race_Ethnicity, num_deaths2),  # Match Race_Ethnicity
    Marital_Status = sample(data$Marital_Status, num_deaths2),  # Match Marital_Status
    Occupation = sample(data$Occupation, num_deaths2),  # Match Occupation
    Education_Level = sample(data$Education_Level, num_deaths2)  # Match Education_Level
  )
  
  # Combine the original data with the manually generated deaths
  data <- bind_rows(data, pathogen_deaths2)
  
  # Let's add num_deaths3 deaths due to the pathogen in the next few days of February 2025
  
  num_deaths3 <- sample(5:12, 1)  # Randomly choose the number of deaths between 5 and 12
  pathogen_deaths3 <- tibble(
    ID = sample(data$ID, num_deaths3, replace = FALSE),  # Select num_deaths3 random IDs
    Date_of_Birth = sample(data$Date_of_Birth, num_deaths3),  # Match Date_of_Birth
    Date_of_Death = as.Date(sample(c("2025-02-06", "2025-02-07", "2025-02-08", "2025-02-09", "2025-02-10"), 
                                   num_deaths3, replace = TRUE)), # Manually set date of death
    Cause_of_Death = rep(pathogen, num_deaths3),  # Set Cause_of_Death to the chosen pathogen
    Gender = sample(data$Gender, num_deaths3),  # Match Gender
    Race_Ethnicity = sample(data$Race_Ethnicity, num_deaths3),  # Match Race_Ethnicity
    Marital_Status = sample(data$Marital_Status, num_deaths3),  # Match Marital_Status
    Occupation = sample(data$Occupation, num_deaths3),  # Match Occupation
    Education_Level = sample(data$Education_Level, num_deaths3)  # Match Education_Level
  )
  
  # Combine the original data with the manually generated deaths
  data <- bind_rows(data, pathogen_deaths3)
  
  # Let's add num_deaths4 deaths due to the pathogen in the next few days of February 2025
  
  num_deaths4 <- sample(7:15, 1)  # Randomly choose the number of deaths between 5 and 12
  pathogen_deaths4 <- tibble(
    ID = sample(data$ID, num_deaths4, replace = FALSE),  # Select num_deaths4 random IDs
    Date_of_Birth = sample(data$Date_of_Birth, num_deaths4),  # Match Date_of_Birth
    Date_of_Death = as.Date(sample(c("2025-02-11", "2025-02-12", "2025-02-13", "2025-02-14"), 
                                   num_deaths4, replace = TRUE)), # Manually set date of death
    Cause_of_Death = rep(pathogen, num_deaths4),  # Set Cause_of_Death to the chosen pathogen
    Gender = sample(data$Gender, num_deaths4),  # Match Gender
    Race_Ethnicity = sample(data$Race_Ethnicity, num_deaths4),  # Match Race_Ethnicity
    Marital_Status = sample(data$Marital_Status, num_deaths4),  # Match Marital_Status
    Occupation = sample(data$Occupation, num_deaths4),  # Match Occupation
    Education_Level = sample(data$Education_Level, num_deaths4)  # Match Education_Level
  )
  
  # Combine the original data with the manually generated deaths
  data <- bind_rows(data, pathogen_deaths4)
  
  # Let's add num_deaths5 due to the pathogen in the second two weeks of February 2025
  num_deaths5 <- sample(10:18, 1)  # Randomly choose the number of deaths between 10 and 18
  pathogen_deaths5 <- tibble(
    ID = sample(data$ID, num_deaths5, replace = FALSE),  # Select 10 random IDs
    Date_of_Birth = sample(data$Date_of_Birth, num_deaths5),  # Match Date_of_Birth
    Date_of_Death = as.Date(sample(c("2025-02-15", "2025-02-16", "2025-02-17", "2025-02-18", "2025-02-19",
                                     "2025-02-20", "2025-02-21", "2025-02-22", "2025-02-23", "2025-02-24"), 
                                   num_deaths5, replace = TRUE)),
    Cause_of_Death = rep(pathogen, num_deaths5),  # Set Cause_of_Death to the chosen pathogen
    Gender = sample(data$Gender, num_deaths5),  # Match Gender
    Race_Ethnicity = sample(data$Race_Ethnicity, num_deaths5),  # Match Race_Ethnicity
    Marital_Status = sample(data$Marital_Status, num_deaths5),  # Match Marital_Status
    Occupation = sample(data$Occupation, num_deaths5),  # Match Occupation
    Education_Level = sample(data$Education_Level, num_deaths5)  # Match Education_Level
  )
  
  # Combine the original data with the manually generated deaths
  data <- bind_rows(data, pathogen_deaths5)
  
  # Sort the data based on Date_of_Death
  data <- data %>% arrange(Date_of_Death)
  
  # Check the number of deaths caused by the pathogen
  count <- sum(data$Cause_of_Death == pathogen)
  
  # Print the count
  print(count)
  
  # Save the count as a variable so we can access it later
  
  library(yaml)
  
  # Load the existing _variables.yml file
  variables <- yaml::read_yaml("_variables.yml")
  
  # Update the variables list with the new variable
  variables[[paste0("deaths_group_", LETTERS[i])]] <- count
  
  # Add a comment explaining the variable
  comment <- paste0("# Number of deaths due to ", pathogens[i], " for the group ", LETTERS[i], " from 2025-02-01 to 2025-02-28")
  variables[[paste0("comment_", LETTERS[i])]] <- comment
  
  # Save the updated variables list to _variables.yml
  writeLines(yaml::as.yaml(variables), "_variables.yml")
  
  
  
  # If the number of unique pathogens is less than 10, repeat the loop
  if (count < 10) {
    # Skip writing the file and repeat the loop
    next
  }
  
  # Save the dataset to a CSV file
  filename <- paste0("data_sets/deaths_group", LETTERS[i], ".csv")
  write.csv(data, file = filename, row.names = FALSE)
  
}
  






  
  #make sure the data look OK -- plot number of deaths/week and color based on cause of death

  # Load required libraries
  library(ggplot2)
  
  # Read the dataset
  deaths <- read.csv("data_sets/deaths_groupA.csv")  # Assuming you have loaded the dataset for a specific cause of death (e.g., Group A Strep)
  
  # Convert Date_of_Death to Date format
  deaths$Date_of_Death <- as.Date(deaths$Date_of_Death)
  
  # Extract week number from Date_of_Death
  deaths$Week <- as.numeric(format(deaths$Date_of_Death, "%W"))
  
  # Plot the number of deaths each week colored based on cause of death
  ggplot(deaths, aes(x = Week, fill = Cause_of_Death)) +
    geom_bar() +
    labs(title = "Number of Deaths Each Week by Cause of Death",
         x = "Week",
         y = "Number of Deaths") +
    theme_minimal()
  
  
 # or a facet plot for each cause of death
  
  # Load required libraries
  library(ggplot2)
  
  # Read the dataset
  deaths <- read.csv("deaths_groupB.csv")  # Assuming you have loaded the dataset for a specific cause of death (e.g., Group A Strep)
  
  # Convert Date_of_Death to Date format
  deaths$Date_of_Death <- as.Date(deaths$Date_of_Death)
  
  # Plot a scatter plot for each cause of death
  ggplot(deaths, aes(x = Date_of_Death, color = Cause_of_Death)) +
    stat_count() +
    labs(title = "Number of Deaths by Cause of Death",
         x = "Date of Death",
         y = "Number of Deaths") +
    facet_wrap(~ Cause_of_Death, scales = "free_y") +
    theme_minimal()
  
  
  
  
