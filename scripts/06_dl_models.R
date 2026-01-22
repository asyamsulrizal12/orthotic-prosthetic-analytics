# Import the Library
library(keras)
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

# Encode features & labels
x <- as.matrix(dataset[, c("Mobility_Score", "Pain_Score", "Satisfaction_Score")])
y <- to_categorical(as.numeric(factor(dataset$Clinician_Notes)) - 1)

# Build Neural Network
model <- keras_model_sequential() %>%
  layer_dense(units = 32, activation = "relu", input_shape = ncol(x)) %>%
  layer_dense(units = 16, activation = "relu") %>%
  layer_dense(units = ncol(y), activation = "softmax")

model %>% compile(
  loss = "categorical_crossentropy",
  optimizer = "adam",
  metrics = "accuracy"
)

# Train Model
history <- model %>% fit(
  x, y,
  epochs = 20,
  batch_size = 32,
  validation_split = 0.2
)

# Evaluate Model
scores <- model %>% evaluate(x, y)

# Save Model
save_model_hdf5(model, "C:/Users/user/Downloads/Data analyst/orthotic-prosthetic-analytics/outputs/models/dl_model.h5")