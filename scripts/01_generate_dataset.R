# Number of Patients
n <- 5360
set.seed(123)

# Patient ID
Patient_ID <- sprintf("P%04d", 1:n)

# Job Categories
Job <- sample(c("Student","Housewife","Labor","Office Worker","Freelance","Unemployed","Retiree"), n, replace=TRUE)

# Gender
Gender <- ifelse(Job == "Housewife", "Female",
                 sample(c("Male","Female"), n, replace=TRUE,
                        prob = c(0.68437521,0.31562479)))

# Age
Age <- sapply(Job, function(j) {
  if (j == "Student") sample(18:24, 1)
  else if (j == "Housewife") sample(22:80, 1)
  else if (j == "Retiree") sample(64:80, 1)
  else sample(18:80, 1)
})

# Primary Diagnosis
diagnosis_list <- c("Partial foot amputation","Through-ankle amputation","Transtibial amputation","Through-knee amputation","Transfemoral amputation","Through-hip amputation","Hemipelvectomy amputation","Hemicorporectomy amputation","Partial-hand amputation","Through-wrist amputation","Transradial amputation","Through-elbow amputation","Transhumeral amputation","Through-shoulder amputation","Forequarter amputation","ACL injury","Cerebral Palsy","Scoliosis","Disc herniation","CTS injury","Fracture"
)

Primary_Diagnosis <- sample(diagnosis_list, n, replace=TRUE)

#Define Amputation Diagnoses for Surgery History
amputation_dx <- c("Partial foot amputation","Through-ankle amputation","Transtibial amputation","Through-knee amputation","Transfemoral amputation","Through-hip amputation","Hemipelvectomy amputation","Hemicorporectomy amputation","Partial-hand amputation","Through-wrist amputation","Transradial amputation","Through-elbow amputation","Transhumeral amputation","Through-shoulder amputation","Forequarter amputation")

Surgery_History <- ifelse(Primary_Diagnosis %in% amputation_dx, "Yes",
                          sample(c("Yes","No"), n, replace=TRUE))

# Amputation Level
Amputation_Level <- ifelse(Primary_Diagnosis %in% amputation_dx, Primary_Diagnosis, "No")

# Amputation Side
Amputation_Side <- ifelse(Primary_Diagnosis %in% amputation_dx,
                          sample(c("Left","Right","Bilateral"), n, replace=TRUE,
                                 prob = c(0.33331582,0.52384672,0.14283746)),
                          "No")

# Cause of Amputation
Cause_of_Amputation <- ifelse(Primary_Diagnosis %in% amputation_dx,
                              sample(c("Trauma","Diabetes","Cancer","Infection"), n,
                                     replace=TRUE,
                                     prob = c(0.3953,0.3128,0.1849,0.1070)),
                              "No")

# Residual Limb Length
Residual_Limb_Length <- ifelse(Primary_Diagnosis %in% amputation_dx,
                               sample(c("Long","Medium","Short"), n, replace=TRUE,
                                      prob = c(0.42183746,0.37492817,0.20323437)),
                               "No")

# Residual Limb Condition
Residual_Limb_Condition <- ifelse(Primary_Diagnosis %in% amputation_dx,
                               sample(c("Healthy","Pressure Sore","Scar Tissue"), n,
                                      replace=TRUE), "No")

# Device Type
Device_Type <- sapply(Primary_Diagnosis, function(dx) {
  if (dx %in% c("Partial-hand amputation","Through-wrist amputation","Transradial amputation","Through-elbow amputation","Transhumeral amputation","Through-shoulder amputation","Forequarter amputation")) {
    sample(c("Passive prosthesis","Body-powered prosthesis","Myoelectric prosthesis"), 1)
  } else if (dx == "ACL injury") {
    sample(c("Functional knee brace","Post-op knee brace","Soft-hinged brace"), 1)
  } else if (dx == "Cerebral Palsy") {
    sample(c("AFO","KAFO","Hip-abducted brace","Resting splint","Dynamic splint","TLSO"), 1)
  } else if (dx == "Scoliosis") {
    sample(c("Conventional brace","Chaneau brace","3D brace"), 1)
  } else if (dx == "Disc herniation") {
    sample(c("LSO","TLSO","Soft collar","Rigid collar"), 1)
  } else if (dx == "CTS injury") {
    sample(c("Resting splint","Dynamic splint"), 1)
  } else if (dx == "Fracture") {
    sample(c("AFO","KAFO","EWHO","Shoulder support","Soft collar","Rigid collar"), 1)
  } else if (dx == "Partial foot amputation") {
    sample(c("Toe filler","Silicone foot prosthesis","Orthosis with toe filler"), 1)
  } else if (dx == "Transtibial amputation") {
    "Transtibial prosthesis"
  } else if (dx == "Through-knee amputation") {
    "Through-knee prosthesis"
  } else if (dx == "Transfemoral amputation") {
    "Transfemoral prosthesis"
  } else if (dx %in% c("Through-hip amputation","Hemipelvectomy amputation","Hemicorporectomy amputation")) {
    sample(c("Canadian-type prosthesis","Saucer-type prosthesis"), 1)
  } else {
    "No device used"
  }
})

# Comorbidities
Comorbidities <- sample(c("None","Diabetes","Hypertension","Cardiovascular Disease"),
                        n, replace=TRUE,
                        prob = c(0.42183746,0.28374621,0.19283746,0.10158887))

# Severity Level
Severity_Level <- sample(c("Mild","Moderate","Severe"), n, replace = TRUE)

# Rehabilitation History
Rehabilitation_History <- sample(c("Yes","No"), n, replace = TRUE)

# Functional Level
Functional_Level <- sample(c("K0","K1","K2","K3","K4"), n, replace = TRUE,
                           prob = c(0.0528,0.1849,0.2739,0.3218,0.1666))

# Balance Assessment
Balance_Assessment <- round(rnorm(n, mean = 45, sd = 10), 0)

# Neurological Status
Neurological_Status <- sapply(Primary_Diagnosis, function(dx) {
  if (dx %in% c("Partial foot amputation","Through-ankle amputation","Transtibial amputation","Through-knee amputation","Transfemoral amputation","Through-hip amputation","Hemipelvectomy amputation","Hemicorporectomy amputation")) {
    sample(c("Normal","Neuropathy","Muscle Weakness"), 1)
  } else if (dx %in% c("Partial-hand amputation","Through-wrist amputation","Transradial amputation","Through-elbow amputation","Transhumeral amputation","Through-shoulder amputation","Forequarter amputation")) {
    sample(c("Normal","Sensory Loss","Muscle Weakness"), 1)
  } else if (dx == "Cerebral Palsy") {
    sample(c("Spasticity","Hyperreflexia","Ataxia","Paralysis"), 1)
  } else if (dx == "ACL injury") {
    sample(c("Normal","Muscle Weakness"), 1)
  } else if (dx == "Disc herniation") {
    sample(c("Neuropathy","Sensory Loss","Muscle Weakness"), 1)
  } else if (dx == "CTS injury") {
    sample(c("Sensory Loss","Muscle Weakness","Neuropathy"), 1)
  } else if (dx == "Scoliosis") {
    sample(c("Normal","Muscle Weakness","Ataxia"), 1)
  } else if (dx == "Fracture") {
    sample(c("Normal","Muscle Weakness"), 1)
  } else {
    "Normal"
  }
})

# Fitting & Usage
Fitting_Date <- sample(seq(as.Date('2015/01/01'), as.Date('2025/05/31'), by="day"), n, replace = TRUE)
Usage_Frequency <- sample(1:12, n, replace = TRUE)
Adjustment_Count <- sample(0:5, n, replace = TRUE)

# Outcomes
Mobility_Score <- round(rnorm(n, mean = 60, sd = 15), 0)
Pain_Score <- round(runif(n, min = 0, max = 10), 1)
Satisfaction_Score <- sample(1:5, n, replace = TRUE)
ADL_Score <- round(rnorm(n, mean = 80, sd = 10), 0)
Complications <- sample(c("None","Pressure Sore","Discomfort","Infection"), n, replace = TRUE)

# Follow-up
Latest_Followup_Date <- sapply(Fitting_Date, function(fd) {
  start_date <- fd + 180
  end_date <- as.Date('2025-12-31')
  if (start_date > end_date) {
    return(end_date)
  } else {
    sample(seq(start_date, end_date, by="day"), 1)
  }
})
Latest_Followup_Date <- as.Date(Latest_Followup_Date, origin="1970-01-01")
Mobility_Change <- round(rnorm(n, mean = 60, sd = 15), 0)
Pain_Change <- round(runif(n, min = 0, max = 10), 1)
Satisfaction_Change <- sample(1:5, n, replace = TRUE)

# Clinical Notes
generate_clinical_notes <- function(mobility_score, mobility_change,
                                    pain_score, pain_change,
                                    satisfaction_score, satisfaction_change) {
  
  notes <- character(length(mobility_score))
  
  for (i in seq_along(mobility_score)) {
    temp <- c()
    if (mobility_score[i] < mobility_change[i]) {
      temp <- c(temp, "Bad progress")
    } else if (mobility_score[i] > mobility_change[i]) {
      temp <- c(temp, "Good progress")
    } else {
      temp <- c(temp, "Stable")
    }
    if (pain_score[i] < pain_change[i]) {
      temp <- c(temp, "No pain reported")
    } else if (pain_score[i] > pain_change[i]) {
      temp <- c(temp, "Pain reported")
    } else {
      temp <- c(temp, "Stable")
    }
    if (satisfaction_score[i] < satisfaction_change[i]) {
      temp <- c(temp, "Need adjustment")
    } else if (satisfaction_score[i] > satisfaction_change[i]) {
      temp <- c(temp, "No need adjustment")
    } else {
      temp <- c(temp, "Stable")
    }
    temp <- unique(temp)
    notes[i] <- paste(temp, collapse = "; ")
  }
  return(notes)
}

Clinician_Notes <- generate_clinical_notes(Mobility_Score, Mobility_Change,
                                          Pain_Score, Pain_Change,
                                          Satisfaction_Score, Satisfaction_Change)
Rehab_Sessions_Count <- sample(0:20, n, replace = TRUE)

# Extra
Height_cm <- round(rnorm(n, mean = 170, sd = 10), 0)
Weight_kg <- round(rnorm(n, mean = 70, sd = 15), 0)
BMI <- round(Weight_kg / ((Height_cm/100)^2), 1)

# Combine all variables into a data frame
dataset <- data.frame(Patient_ID, Age, Gender, Job, Primary_Diagnosis, Comorbidities, Severity_Level, Surgery_History, Rehabilitation_History, Amputation_Level, Amputation_Side, Cause_of_Amputation, Residual_Limb_Length, Residual_Limb_Condition, Functional_Level, Balance_Assessment, Neurological_Status, Device_Type, Fitting_Date, Usage_Frequency, Adjustment_Count, Mobility_Score, Pain_Score, Satisfaction_Score, ADL_Score, Complications, Latest_Followup_Date, Mobility_Change, Pain_Change, Satisfaction_Score, Clinician_Notes, Rehab_Sessions_Count, Height_cm, Weight_kg, BMI)

# Save a csv file
write.csv(dataset, "C:/Users/user/Downloads/Data analyst/orthotic-prosthetic-analytics/data/raw/prosthetics-orthotics-datasets_raw.csv", row.names = FALSE)