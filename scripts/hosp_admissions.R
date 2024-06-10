#R script for generating the data that will be used in the BM424 workshops
#This script will be used to generate the files with hospital admissions data, which will be named 
#hosp_admissions_group[group #].csv, where the group numbers are A-M (we have ~10 groups most years, but generate a 
#few extra just in case...)

# Read the data from admissions.csv which give the codes, descriptions, and rates for NHS hospital admissions
admission_data <- read.csv("admissions.csv")

# Create a directory to store the CSV files (if it doesn't exist)
if (!file.exists("data_sets")) {
  dir.create("data_sets")
}

# Define the pathogen names:
# List of pathogens corresponding to letters A-K
pathogens <- c("Legionellosis", "Influenza", "MRSA infection", "Campylobacter infection", "Listeriosis", 
               "Meningitis", "Tuberculosis", "Group A Strep", "Anthrax", "Whooping cough", "Acinetobacter infection", 
               "Aspergillosis")

# Create the data for the different groups - Loop through the different pathogens 
for (j in 1:length(pathogens)) {
  
  # First create generic admissions data - 1000 entries of code/description pairs sampled
  # from admissions.csv data.
  
  # Set the number of entries in the new dataframe
  num_entries <- 1000
  
  # Sample pairs of codes and descriptions based on the rates
  hosp_admiss <- tibble(
    Code = character(num_entries),
    Description = character(num_entries)
  )
  
  for (i in 1:num_entries) {
    sampled_index <- sample.int(nrow(admission_data), 1, prob = admission_data$Rate)
    hosp_admiss$Code[i] <- admission_data$Code[sampled_index]
    hosp_admiss$Description[i] <- admission_data$Description[sampled_index]
  }
  
  # add admission ID and date
  
  # Function to generate random alphanumeric code
  generate_random_code <- function(n) {
    random_chars <- c(LETTERS, 0:9) # Characters to choose from
    replicate(n, paste0(sample(random_chars, 6, replace = TRUE), collapse = ""))
  }
  
  # Generate random admission IDs
  hosp_admiss$Admission_ID <- generate_random_code(num_entries)
  
  # Generate random admission dates with weighted probability
  date_probabilities <- c(rep(0.05 / 7, 7), rep(0.20 / 7, 7), rep(0.30 / 7, 7), rep(0.45 / 7, 7))
  hosp_admiss$Admission_Date <- sample(seq(as.Date("2025-02-01"), as.Date("2025-02-28"), by = "day"), 
                                       num_entries, replace = TRUE, prob = date_probabilities)
  
  # Next add data for the pathogen-related cases to each dataset
  # Define codes and descriptions for each pathogen
  # Legionnaires' disease A48 --> A30-A49 Other bacterial diseases
  # Influenza --> J09-J18 Influenza & pneumonia        
  # MRSA --> A41 Sepsis due to Staph aureus --> A30-A49 Other bacterial diseases
  # Campylobacter --> A04.5 Campylobacter enteritis --> A00-A09 Intestinal infectious diseases    
  # Listeriosis --> A32 --> A30-A49 Other bacterial diseases
  # Meningitis --> A39 Meningococcal lnfection --> A30-A49 Other bacterial diseases
  # Tuberculosis --> A15-A19 Tuberculosis --> A15-A19 Tuberculosis
  # Group A Strep --> A40 Sepsis due to streptococcus, group A --> A30-A49 Other bacterial diseases
  # Anthrax --> A22 Anthrax --> A20-A28 Certain zoonotic bacterial diseases
  # Whooping cough --> A37 Whooping cough --> A30-A49 Other bacterial diseases
  # Acinetobacter --> A41.9 sepsis unidentified? -- > A30-A49 Other bacterial diseases
  # Aspergillosis --> B44 Aspergillosis --> B35-B49 Mycoses
  codes <- c("A30-A49", "J09-J18", "A30-A49", "A00-A09",
             "A30-A49", "A30-A49", "A15-A19", "A30-A49",
             "A20-A28", "A30-A49", "A30-A49", "B35-B49")
  
  descriptions <- c("Other bacterial diseases", "Influenza & pneumonia", "Other bacterial diseases",
                    "Intestinal infectious diseases", "Other bacterial diseases", "Other bacterial diseases",
                    "Tuberculosis", "Other bacterial diseases", "Certain zoonotic bacterial diseases",
                    "Other bacterial diseases", "Other bacterial diseases", "Mycoses")
  
  # Define Code and Description for the current pathogen
  code <- codes[j]
  description <- descriptions[j]
  
  # Generate data for cases due to the current pathogen
  # Define the number of cases for the pathogen
  num_cases <- sample(5:15, 1)  # Randomly choose the number of cases
  
  # Generate data for each case
  pathogen_cases <- tibble(
    Admission_ID = sample(hosp_admiss$Admission_ID, num_cases, replace = TRUE),  # Take random admission ID for each case
    Admission_Date = sample(seq(as.Date("2025-02-01"), as.Date("2025-02-28"), by = "day"), num_cases, replace = TRUE, prob = date_probabilities),  # Sample admission dates with weighted probability
    Code = rep(code, num_cases),  # Use the same code for all cases
    Description = rep(description, num_cases)  # Use the same description for all cases
  )
  
  # Add the cases to the hosp_admiss dataframe
  hosp_admiss <- bind_rows(hosp_admiss, pathogen_cases)
  
  # Sort the dataframe by Admission_Date
  hosp_admissions_sorted <- hosp_admiss[order(hosp_admiss$Admission_Date), ]
  
  # Define the file name
  file_name <- paste0("data_sets/hospital_admissions_group", LETTERS[j], ".csv")
  
  # Save the dataframe to a CSV file
  write.csv(hosp_admissions_sorted, file = file_name, row.names = FALSE)
  
}

  
  
# To check the data are OK - Get a summary of the frequencies of each cause of admission

  
# Count the occurrences of each cause of admission
admission_counts <- table(hosp_admiss$Description)
  
# Print the counts
print(admission_counts)
  
  
  
  