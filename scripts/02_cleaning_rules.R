# Load a Raw Dataset
dataset <- read.csv("C:/Users/user/Downloads/Data analyst/orthotic-prosthetic-analytics/data/raw/prosthetics-orthotics-datasets_raw.csv", stringsAsFactors = FALSE)

# Verify the date field is in Date format
dataset$Fitting_Date <- as.Date(dataset$Fitting_Date)
dataset$Latest_Followup_Date <- as.Date(dataset$Latest_Followup_Date)

# 1st Rule: Follow-up Date cannot be earlier from Fitting Date
dataset <- subset(dataset, Latest_Followup_Date >= Fitting_Date)

# 2nd Rule: Follow-up Required at Least 6 Months After Fitting
dataset <- subset(dataset, Latest_Followup_Date >= (Fitting_Date + 180))

# 3rd Rule: The score isn't allowed to be negative
dataset$Mobility_Score[dataset$Mobility_Score < 0] <- NA
dataset$Pain_Score[dataset$Pain_Score < 0] <- NA
dataset$Satisfaction_Score[dataset$Satisfaction_Score < 0] <- NA
dataset$Mobility_Change[dataset$Mobility_Change < 0] <- NA
dataset$Pain_Change[dataset$Pain_Change < 0] <- NA
dataset$Satisfaction_Score.1[dataset$Satisfaction_Score.1 < 0] <- NA

# 4th Rule: Pain Score and Pain Change has a maximum value of 10
dataset$Pain_Score[dataset$Pain_Score > 10] <- 10
dataset$Pain_Change[dataset$Pain_Change > 10] <- 10

# 5th Rule: Satisfaction Score and Satisfaction Change has a maximum value of 5
dataset$Satisfaction_Score[dataset$Satisfaction_Score > 5] <- 5
dataset$Satisfaction_Score.1[dataset$Satisfaction_Score.1 > 5] <- 5

# 6th Rule: Remove Duplicate Patient ID
dataset <- dataset[!duplicated(dataset$Patient_ID), ]

# Save the Cleaned CSV File
write.csv(dataset, "C:/Users/user/Downloads/Data analyst/orthotic-prosthetic-analytics/data/cleaned/prosthetics-orthotics-datasets_cleaned.csv", row.names = FALSE)