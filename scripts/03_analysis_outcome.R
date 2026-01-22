# Load the Cleaning Dataset
dataset <- read.csv("C:/Users/user/Downloads/Data analyst/orthotic-prosthetic-analytics/data/cleaned/prosthetics-orthotics-datasets_cleaned.csv", stringsAsFactors = FALSE)

# Verify the Date Format is Correct
dataset$Fitting_Date <- as.Date(dataset$Fitting_Date)
dataset$Latest_Followup_Date <- as.Date(dataset$Latest_Followup_Date)

# Statistical Summary
summary(dataset$Mobility_Score)
summary(dataset$Pain_Score)
summary(dataset$Satisfaction_Score)

# The Clinician Notes Distribution
table(dataset$Clinician_Notes)

# The Average Change of The Score Analysis
mean_mobility_change <- mean(dataset$Mobility_Change - dataset$Mobility_Score, na.rm = TRUE)
mean_pain_change <- mean(dataset$Pain_Change - dataset$Pain_Score, na.rm = TRUE)
mean_satisfaction_change <- mean(dataset$Satisfaction_Score.1 - dataset$Satisfaction_Score, na.rm = TRUE)

cat("Rata-rata perubahan Mobility:", mean_mobility_change, "\n")
cat("Rata-rata perubahan Pain:", mean_pain_change, "\n")
cat("Rata-rata perubahan Satisfaction:", mean_satisfaction_change, "\n")

# Inter-score Correlation Analysis
cor_mobility_pain <- cor(dataset$Mobility_Score, dataset$Pain_Score, use = "complete.obs")
cor_mobility_satisfaction <- cor(dataset$Mobility_Score, dataset$Satisfaction_Score, use = "complete.obs")
cor_pain_satisfaction <- cor(dataset$Pain_Score, dataset$Satisfaction_Score, use = "complete.obs")

cat("Korelasi Mobility vs Pain:", cor_mobility_pain, "\n")
cat("Korelasi Mobility vs Satisfaction:", cor_mobility_satisfaction, "\n")
cat("Korelasi Pain vs Satisfaction:", cor_pain_satisfaction, "\n")

# Follow-up Period Analysis (Number of Days)
dataset$Followup_Duration <- as.numeric(dataset$Latest_Followup_Date - dataset$Fitting_Date)
summary(dataset$Followup_Duration)

# Save The Analysis Results as a .txt File
sink("C:/Users/user/Downloads/Data analyst/orthotic-prosthetic-analytics/outputs/analysis_summary.txt")
cat("=== Summary Statistik ===\n")
print(summary(dataset$Mobility_Score))
print(summary(dataset$Pain_Score))
print(summary(dataset$Satisfaction_Score))

cat("\n=== Distribusi Clinical Notes ===\n")
print(table(dataset$Clinical_Notes))

cat("\n=== Rata-rata Perubahan Skor ===\n")
cat("Mobility:", mean_mobility_change, "\n")
cat("Pain:", mean_pain_change, "\n")
cat("Satisfaction:", mean_satisfaction_change, "\n")

cat("\n=== Korelasi Antar Skor ===\n")
cat("Mobility vs Pain:", cor_mobility_pain, "\n")
cat("Mobility vs Satisfaction:", cor_mobility_satisfaction, "\n")
cat("Pain vs Satisfaction:", cor_pain_satisfaction, "\n")

cat("\n=== Durasi Follow-up (hari) ===\n")
print(summary(dataset$Followup_Duration))
sink()