# Import the Library
library(caret)
set.seed(123)

# Load Dataset
dataset <- read.csv("C:/Users/user/Downloads/Data analyst/orthotic-prosthetic-analytics/data/cleaned/prosthetics-orthotics-datasets_cleaned.csv", stringsAsFactors = FALSE)

# Set a Target as a Factor
dataset$Clinician_Notes <- factor(dataset$Clinician_Notes)

# Filtering Out Rare Categories (Fewer Than 5 Observations)
note_freq <- table(dataset$Clinician_Notes)
rare_levels <- names(note_freq[note_freq < 5])
dataset <- subset(dataset, !(Clinician_Notes %in% rare_levels))
dataset$Clinician_Notes <- droplevels(dataset$Clinician_Notes)

# Train/Test Split With Stratified Sampling
trainIndex <- createDataPartition(dataset$Clinician_Notes, p = 0.8, list = FALSE)
trainData <- dataset[trainIndex, ]
testData  <- dataset[-trainIndex, ]

# Keep the level of factors consistent
trainData$Clinician_Notes <- factor(trainData$Clinician_Notes)
testData$Clinician_Notes  <- factor(testData$Clinician_Notes, levels = levels(trainData$Clinician_Notes))

# Train Random Forest Model by Cross-Validation
ctrl <- trainControl(method = "cv", number = 5, verboseIter = TRUE, allowParallel = FALSE)
model_rf <- train(
  Clinician_Notes ~ Mobility_Score + Pain_Score + Satisfaction_Score,
  data = trainData,
  method = "rf",
  trControl = ctrl
)

# Predict on Test Set
pred <- predict(model_rf, testData)
pred <- factor(pred, levels = levels(testData$Clinician_Notes))

# Confusion Matrix
cm <- confusionMatrix(pred, testData$Clinician_Notes)
print(cm)

# Export as a .txt File
sink("C:/Users/user/Downloads/Data analyst/orthotic-prosthetic-analytics/outputs/confusion_matrix.txt")
print(cm)
sink()

sink("C:/Users/user/Downloads/Data analyst/orthotic-prosthetic-analytics/outputs/model_summary.txt")
print(model_rf)
sink()

# Export as a .png File
# Confusion Matrix Heatmap
cm_table <- as.data.frame(cm$table)
ggplot(cm_table, aes(x = Reference, y = Prediction, fill = Freq)) +
  geom_tile() +
  geom_text(aes(label = Freq), color = "white") +
  scale_fill_gradient(low = "steelblue", high = "darkred") +
  theme_minimal()
ggsave("C:/Users/user/Downloads/Data analyst/orthotic-prosthetic-analytics/outputs/confusion_matrix.png")

# Feature Importance
importance <- varImp(model_rf)
plot(importance)
ggsave("C:/Users/user/Downloads/Data analyst/orthotic-prosthetic-analytics/outputs/feature_importance.png")
