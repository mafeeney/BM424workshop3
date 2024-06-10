#R script for generating the data that will be used in the BM424 workshops
#This script will be used to generate the files with polling data, which will be named 
#polling_group[group #].csv, where the group numbers are A-M (we have ~10 groups most years, but generate a 
#few extra just in case...)

# Create a directory to store the CSV files (if it doesn't exist)
if (!file.exists("data_sets")) {
  dir.create("data_sets")
}

# Loop through letters A to M
for (letter in LETTERS[1:13]) {
  # Generate your data set - polling data for a set of questions relevant to epidemiology (question text
  #provided as metadata.)
  #question answers to be on a Likert scale (1-5)

  Questions <- c("Q01", "Q02", "Q03", "Q04", "Q05", "Q06","Q07", "Q08", "Q09","Q10", 
                 "Q11", "Q12","Q13", "Q14", "Q15", "Q16", "Q17", "Q18","Q19", "Q20", 
                 "Q21","Q22", "Q23", "Q24","Q25", "Q26", "Q27","Q28", "Q29", "Q30")
  
  # Define Likert categories
  likert_categories <- c("Strongly Disagree", "Disagree", "Neutral", "Agree", "Strongly Agree")

  #we next will need to set up data with the answers to each question

  polling_data <- numeric(0)

  for (Question in Questions) {
    # Determine the type of distribution for the current question
    distribution_type <- sample(c("Random", "Normalish", "Skewed_45", "Skewed_12"), 1)
    
    # Generate polling data based on the distribution type
    if (distribution_type == "Random") {
      # For completely random distribution of answers
      random_numbers <- c(runif(4, min = 0, max = 100), 0)  # Generate four random numbers
      random_numbers <- round(random_numbers / sum(random_numbers) * 100, 1)  # Normalize to sum up to 100
    } else if (distribution_type == "Normalish") {
      # For approximately normal distribution
      normalish_proportions <- c(0.10, 0.20, 0.30, 0.25, 0.15)
      normalish_distribution <- round(normalish_proportions * 100 * runif(1, 0.90, 1.10))
      random_numbers <- normalish_distribution
    } else if (distribution_type == "Skewed_45") {
      # For distribution skewed towards 4-5
      skewed_45_proportions <- c(0.05, 0.05, 0.1, 0.2, 0.6)
      skewed_45_distribution <- round(skewed_45_proportions * 100 * runif(1, 0.90, 1.10))
      random_numbers <- skewed_45_distribution
    } else if (distribution_type == "Skewed_12") {
      # For distribution skewed towards 1-2
      skewed_12_proportions <- c(0.6, 0.2, 0.1, 0.05, 0.05)
      skewed_12_distribution <- round(skewed_12_proportions * 100 * runif(1, 0.90, 1.10))
      random_numbers <- skewed_12_distribution
    }
    
    # Create a data frame for the current question and polling data
    question_data <- data.frame(Question = rep(Question, each = 5),
                                likert_categories,
                                polling_data = random_numbers)
    
    # Append the question's data to the combined data frame
    polling_data <- rbind(polling_data, question_data)
  }
  
# Save the data set as a CSV file
file_name <- paste0("data_sets/polling_group", letter, ".csv")
write.csv(polling_data, file = file_name, row.names = FALSE)
}


#test plot to make sure the data look ok

# Load the ggplot2 package
library(ggplot2)
# Load the dplyr package
library(dplyr)

# Calculate frequencies of each Likert category within each question
frequency_data <- polling_data %>%
  group_by(Question, likert_categories) %>%
  summarise(Frequency = sum(polling_data))

# Define the order of Likert categories
likert_order <- c("Strongly Disagree", "Disagree", "Neutral", "Agree", "Strongly Agree")

# Create a ggplot object with bar plots
plot <- ggplot(frequency_data, aes(x = factor(likert_categories, levels = likert_order), y = Frequency)) +
  geom_bar(stat = "identity", color = "black", fill = "black") +  # Create bar plots with black fill
  facet_wrap(~ Question, scales = "free") +  # Facet by Question
  labs(x = "Likert Category", y = "Frequency") +  # Set axis labels
  theme_bw() +  # Set theme to black and white
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +  # Rotate x-axis labels
  scale_fill_manual(values = c("black"))  # Set bar fill color to black

# Print the plot
print(plot)

