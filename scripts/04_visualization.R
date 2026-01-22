# Import library
library(ggplot2)

# Load the Cleaning Dataset
dataset <- read.csv("C:/Users/user/Downloads/Data analyst/orthotic-prosthetic-analytics/data/cleaned/prosthetics-orthotics-datasets_cleaned.csv", stringsAsFactors = FALSE)

# Verify the Date Format is Correct
dataset$Fitting_Date <- as.Date(dataset$Fitting_Date)
dataset$Latest_Followup_Date <- as.Date(dataset$Latest_Followup_Date)

# Histogram Mobility Score
p1 <- ggplot(dataset, aes(x = Mobility_Score)) +
  geom_histogram(binwidth = 5, fill = "steelblue", color = "white") +
  labs(title = "Distribution of Mobility Scores", x = "Mobility Score", y = "Count")

# Histogram Pain Score
p2 <- ggplot(dataset, aes(x = Pain_Score)) +
  geom_histogram(binwidth = 1, fill = "tomato", color = "white") +
  labs(title = "Distribution of Pain Scores", x = "Pain Score", y = "Count")

# Bar plot Satisfaction Score
p3 <- ggplot(dataset, aes(x = factor(Satisfaction_Score))) +
  geom_bar(fill = "darkgreen") +
  labs(title = "Distribution of Satisfaction Scores", x = "Satisfaction Score", y = "Count")

# Clinical Notes Frequency
p4 <- ggplot(dataset, aes(x = Clinician_Notes)) +
  geom_bar(fill = "purple") +
  labs(title = "Frequency of Clinician Notes", x = "Clinician Notes", y = "Count") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Follow-up duration distribution
dataset$Followup_Duration <- as.numeric(dataset$Latest_Followup_Date - dataset$Fitting_Date)
p5 <- ggplot(dataset, aes(x = Followup_Duration)) +
  geom_histogram(binwidth = 30, fill = "orange", color = "white") +
  labs(title = "Distribution of Follow-up Duration", x = "Duration (days)", y = "Count")

# Save Plots
ggsave("C:/Users/user/Downloads/Data analyst/orthotic-prosthetic-analytics/outputs/mobility_distribution.png", p1, width = 7, height = 5)
ggsave("C:/Users/user/Downloads/Data analyst/orthotic-prosthetic-analytics/outputs/pain_distribution.png", p2, width = 7, height = 5)
ggsave("C:/Users/user/Downloads/Data analyst/orthotic-prosthetic-analytics/outputs/satisfaction_distribution.png", p3, width = 7, height = 5)
ggsave("C:/Users/user/Downloads/Data analyst/orthotic-prosthetic-analytics/outputs/clinician_notes_frequency.png", p4, width = 8, height = 6)
ggsave("C:/Users/user/Downloads/Data analyst/orthotic-prosthetic-analytics/outputs/followup_duration_distribution.png", p5, width = 7, height = 5)
