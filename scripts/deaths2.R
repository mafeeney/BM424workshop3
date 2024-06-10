#R script for generating the data that will be used in the BM424 workshops
#This script will be used to generate the files with demographic, which will be named 
#demographics_group[group #].csv, where the group numbers are A-M (we have ~10 groups most years, but generate a 
#few extra just in case...)

#This script will be used to generate the patient death records in workshop 4, and will vary the amount of 
#observed deaths based on group decisions from workshop 3. we will do this using a vector (scaling_factor) 
#to adjust the num_deaths values. 

#the values in scaling_factor need to be entered by hand 
#the dates need updating

# Load required libraries
library(dplyr)

# Create a directory to store the CSV files (if it doesn't exist)
if (!file.exists("data_sets")) {
  dir.create("data_sets")
}

# Define the pathogen names:
# List of pathogens corresponding to letters A-L
pathogens <- c("Legionellosis", "Influenza", "MRSA infection", "Campylobacter infection", "Listeriosis", 
               "Meningitis", "Tuberculosis", "Group A Strep", "Anthrax", "Whooping cough", "Acinetobacter infection", 
               "Aspergillosis")

# Define the scaling factor and enter values

# Define empty vectors to store the scaling factors for each group
scaling_factors <- vector("numeric", length = 12)

# Enter the scaling factors for each group individually
scaling_factors[1] <- 0.8  # For group A
scaling_factors[2] <- 0.7  # For group B
scaling_factors[3] <- 0.9  # For group C
scaling_factors[4] <- 0.6  # For group D
scaling_factors[5] <- 0.5  # For group E
scaling_factors[6] <- 0.75  # For group F
scaling_factors[7] <- 0.85  # For group G
scaling_factors[8] <- 0.65  # For group H
scaling_factors[9] <- 0.95  # For group I
scaling_factors[10] <- 0.55  # For group J
scaling_factors[11] <- 0.75  # For group K
scaling_factors[12] <- 0.85  # For group L


# Load the existing _variables.yml file (to save variables for each group)
variables <- yaml::read_yaml("_variables.yml")


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
    Date_of_Death = sample(seq(as.Date('2025/01/01'), as.Date('2025/02/28'), by="day"), 1000, replace = TRUE),  # Date of death
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
  
  # Add manually generated deaths due to the pathogen
  # Let's add a child as the first victim
  # Generate a random date of birth for the child within the last 10 years
  child_DOB <- as.Date(sample(seq(Sys.Date() - 3650, Sys.Date(), by = "day"), 1))
  
  # Generate a gender for the child
  child_gender <- sample(c("Male", "Female"), 1)
  
  # Add manually generated deaths due to the pathogen for the child
  child_death <- tibble(
    ID = sample(data$ID, 1),  # Select a random ID
    Date_of_Birth = child_DOB,  # Assign the generated date of birth
    Date_of_Death = as.Date(sample(c("2025-02-01", "2025-02-02", "2025-02-03", "2025-02-04", "2025-02-05"), 1)), # Manually set date of death
    Cause_of_Death = pathogen,  # Set Cause_of_Death to the chosen pathogen
    Gender = child_gender,  # Assign gender
    Race_Ethnicity = sample(data$Race_Ethnicity, 1),  # Match Race_Ethnicity
    Marital_Status = "Single",  # Set marital status to "Single" for the child
    Occupation = "Unemployed",  # Set occupation to "Student" for the child
    Education_Level = "Less than High School"  # Set education level for the child
  )
  
  # Combine the original data with the manually generated deaths for the child
  data <- bind_rows(data, child_death)
  
  
  # Let's add an adult aged 40-60 as the second victim

  # Randomly choose age between 40 and 60 and set other variables
  adult_DOB <- Sys.Date() - sample(40:60*365, 1)
  adult_Marital_Status <- sample(c("Married", "Single", "Divorced", "Widowed"), 1)
  adult_Occupation <- sample(c("Professional", "Blue Collar", "Service", "Homemaker", "Unemployed", "Other"), 1)
  adult_Education_Level <- sample(c("Less than High School", "High School Graduate", "Some College", "College Graduate", "Postgraduate"), 1)
  adult_gender <- sample(c("Male", "Female"), 1)
    
  adult_death <- tibble(
    ID = sample(data$ID, 1),  # Select a random ID
    Date_of_Birth = adult_DOB,  # Assign the generated date of birth
    Date_of_Death = as.Date(sample(c("2025-02-01", "2025-02-02", "2025-02-03", "2025-02-04", "2025-02-05"), 1)), # Manually set date of death
    Cause_of_Death = pathogen,  # Set Cause_of_Death to the chosen pathogen
    Gender = adult_gender, 
    Race_Ethnicity = sample(data$Race_Ethnicity, 1),  # Match Race_Ethnicity
    Marital_Status = adult_Marital_Status,
    Occupation = adult_Occupation,
    Education_Level = adult_Education_Level
  )
    
  # Combine the original data with the adult data
  data <- bind_rows(data, adult_death)


  # save the data to a .csv file in data_sets folder
  
  # Sort the data based on Date_of_Death
  data <- data %>% arrange(Date_of_Death)
  
  # Save the dataset to a CSV file
  filename <- paste0("data_sets/W2deaths_group", LETTERS[i], ".csv")
  write.csv(data, file = filename, row.names = FALSE)
  
  
  
  # save YAML variables with the various data we will need for other files

  
  # Check the number of deaths caused by the pathogen
  count <- sum(data$Cause_of_Death == pathogen)
  
  # Update the variables list with the number of deaths caused by the pathogen
  variables[[paste0("deaths_group_", LETTERS[i], "_w2")]] <- count
  
  # Add a variable for the adult victim data
  # Calculate the adult's age
  adult_age <- round(as.numeric(difftime(Sys.Date(), adult_DOB, units = "days")) / 365.25)
  
  # Define the YAML variable name
  yaml_var1 <- paste0("group_", LETTERS[i], "_adult_chars")
  
  # Get the values for the YAML variable
  values1 <- list(adult_age = adult_age,
                 adult_Marital_Status = adult_Marital_Status,
                 adult_Occupation = adult_Occupation,
                 adult_Education_Level = adult_Education_Level,
                 adult_gender = adult_gender)
  
  # Update or add the YAML variable
  variables[[yaml_var1]] <- values1
  
  # Add a variable for the child victim data
  # Calculate the child's age
  child_age <- round(as.numeric(difftime(Sys.Date(), child_DOB, units = "days")) / 365.25)
  
  # Define the YAML variable name
  yaml_var2 <- paste0("group_", LETTERS[i], "_child_chars")
  
  # Get the values for the YAML variable
  values2 <- list(child_age = child_age, 
                 child_gender = child_gender)
  
  # Update or add the YAML variable
  variables[[yaml_var2]] <- values2
  }

# Add a comment explaining the variables
comment <- paste0("# Data for workshop 2: characteriistics of adult and child victims of the pathogen, and total count of deaths")
variables <- list(comment, variables)

# Print the YAML content before writing to file
print(yaml::as.yaml(variables))

# Write the updated YAML to _variables.yml
yaml::write_yaml(variables, "_variables.yml")




  
  #make sure the data look OK -- plot number of deaths/week and color based on cause of death

  # Load required libraries
  library(ggplot2)
  
  # Read the dataset
  deaths <- read.csv("data_sets/W2deaths_groupA.csv")  # Assuming you have loaded the dataset for a specific cause of death (e.g., Group A Strep)
  
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
  deaths <- read.csv("data_sets/W2deaths_groupC.csv")  # Assuming you have loaded the dataset for a specific cause of death (e.g., Group A Strep)
  
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
  
  
  
  
