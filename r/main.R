library(dplyr)
library(rio)
library(survey)

source("getNHANES.R")
source("evaluateModel.R")

dir <- "../data"

# 2001-2003
variables <- list(c("SEQN", "SDMVPSU", "SDMVSTRA", "WTINT2YR", "WTMEC2YR",
                    "RIAGENDR", "RIDAGEYR", "RIDRETH1", "DMDEDUC2", "DMDMARTL",
                    "DMDHHSIZ", "INDFMPIR"), c("SEQN", "DIQ010"),
                  c("SEQN", "WTSAF2YR", "LBXGLU"), c("SEQN", "URXUCL"),
                  c("SEQN", "SXQ272"), c("SEQN", "HCQ030"), c("SEQN", "MCQ010"),
                  c("SEQN", "SXQ260"), c("SEQN", "LBXHE2"), c("SEQN", "HID010"))
files <- c("DEMO", "DIQ", "L10AM", "L05", "SXQ", "HCQ", "MCQ", "SXQ",
           "L09", "HIQ")
data1 <- getNHANES(years = c(2001, 2003), files = files, dir = dir,
                   variables = variables)
colnames(data1) <- c("id", "psu", "strata", "wtint", "wtmec", "gender", "age",
                     "race", "edu", "mar", "hhsize", "poverty", "diabetes",
                     "wtfast", "glucose", "chlamydiaTest", "chlamydiaAnswer", 
                     "hepatitisC", "asthmaAnswer", "herpesAnswer", "herpesTest",
                     "healthInsurance")

# 2005-2011
variables <- list(c("SEQN", "SDMVPSU", "SDMVSTRA", "WTINT2YR", "WTMEC2YR", 
                    "RIAGENDR", "RIDAGEYR", "RIDRETH1", "DMDEDUC2", "DMDMARTL",
                    "DMDHHSIZ", "INDFMPIR"), c("SEQN", "DIQ010"),
                  c("SEQN", "WTSAF2YR", "LBXGLU"), c("SEQN", "URXUCL"),
                  c("SEQN", "SXQ272"), c("SEQN", "HCQ030"), c("SEQN", "MCQ010"),
                  c("SEQN", "SXQ260"), c("SEQN", "LBXHE2"), c("SEQN", "HIQ011"))
files <- c("DEMO", "DIQ", "GLU", "CHLMDA", "SXQ", "HCQ", "MCQ", "SXQ",
           "HSV", "HIQ")
data2 <- getNHANES(years = c(2005, 2007, 2009, 2011), files = files, dir = dir,
                   variables = variables)
colnames(data2) <- c("id", "psu", "strata", "wtint", "wtmec", "gender", "age",
                     "race", "edu", "mar", "hhsize", "poverty", "diabetes",
                     "wtfast", "glucose", "chlamydiaTest", "chlamydiaAnswer",
                     "hepatitisC", "asthmaAnswer", "herpesAnswer", "herpesTest",
                     "healthInsurance")

# Asthma
variables <- list(c("SEQN", "SPDBRONC"))
files <- c("SPX")
asthmaData <- getNHANES(years = c(2007, 2009, 2011), files = files, dir = dir,
                        variables = variables)
colnames(asthmaData) <- c("id", "asthmaTest")

# Merge datasets
data <- rbind(data1, data2)
data <- left_join(data, asthmaData)

# Create categorical variables
data$gender <- factor(data$gender, levels = c(1,2), labels = c("Male", "Female"))
data$race <- ifelse(data$race %in% c(1,2), 1,
                    ifelse(data$race == 3, 2, 
                           ifelse(data$race == 4, 3,
                                  ifelse(data$race == 5, 4, NA))))
data$race <- factor(data$race, levels = c(1, 2, 3, 4), 
                    labels = c("Hispanic", "White", "Black", "Other"))
data$edu <- ifelse(data$edu %in% c(1,2), 1,
                   ifelse(data$edu %in% c(3,4), 2,
                          ifelse(data$edu %in% 5, 3, NA)))
data$edu <- factor(data$edu, levels = c(1,2,3),
                   labels = c("No High School", "High School", "College"))
data$poverty <- ifelse(data$poverty < 2, 1,
                       ifelse(data$poverty >= 2, 2, NA))
data$poverty <- factor(data$poverty, levels = c(1,2),
                       labels = c("Below Poverty Line", "Above Poverty Line"))
data$ins <- ifelse(data$healthInsurance == 1, 1,
                   ifelse(data$healthInsurance == 2, 0, NA))
data$ins <- factor(data$ins, levels = c(0,1),
                   labels = c("Health Insurance", "No Health Insurance"))

# Define reference group
data$gender <- relevel(data$gender, ref = "Male")
data$race <- relevel(data$race, ref = "White")
data$edu <- relevel(data$edu, ref = "College")
data$poverty <- relevel(data$poverty, ref = "Above Poverty Line")
data$ins <- relevel(data$ins, ref = "Health Insurance")

# Variables to consider in the model
vars <- c("age", "hhsize", "poverty", "gender", "race", "edu", "ins")

# Diabetes
data$diabetesTruth <- ifelse(data$glucose >= 126, 1, 0)
data$undiagnosedDiabetes <- ifelse(data$diabetes == 2 & data$diabetesTruth == 1, 1, 0)
design <- svydesign(id = ~psu, strata = ~strata, weights = ~wtfast, nest = TRUE,
                    data = data[!(is.na(data$wtfast)) & data$age > 20,])
diabetes <- evaluateModel(design, "undiagnosedDiabetes", vars)
diabetesCoef <- data.frame(var = names(diabetes$coefficients),
                           coef = diabetes$coefficients)
write.csv(diabetesCoef, "../data/diabetes_coefficients.txt", row.names = FALSE)

# Hepatitis C
data$undiagnosedHepC <- ifelse(data$hepatitisC == 1, 1, 0)
data$undiagnosedHepC[is.na(data$undiagnosedHepC)] <- 0
design <- svydesign(id = ~psu, strata = ~strata, weights = ~wtmec, nest = TRUE,
                    data = data[!(is.na(data$wtmec)) & data$age > 20,])
hepatitisC <- evaluateModel(design, "undiagnosedHepC", vars)
hepcCoef <- data.frame(var = names(hepc$coefficients),
                       coef = hepc$coefficients)
write.csv(hepcCoef, "../data/hepc_coefficients.txt", row.names = FALSE)

# Asthma
data$asthmaTest <- ifelse(data$asthmaTest == 1, 1,
                          ifelse(data$asthmaTest == 2, 0, NA))
data$asthmaAnswer <- ifelse(data$asthmaAnswer == 1, 1,
                            ifelse(data$asthmaAnswer == 2, 0, NA))
data$asthma <- ifelse(data$asthmaTest == 1 & data$asthmaAnswer == 0, 1, 0)
design <- svydesign(id = ~psu, strata = ~strata, weights = ~wtmec, nest = TRUE,
                    data = data[data$age >20,])
asthma <- evaluateModel(design, "asthma", vars)
asthmaCoef <- data.frame(var = names(asthma$coefficients),
                         coef = asthma$coefficients)
write.csv(asthmaCoef, "../data/asthma_coefficients.txt", row.names = FALSE)

# Herpes
data$herpesTest <- ifelse(data$herpesTest == 1, 1,
                          ifelse(data$herpesTest == 2, 0, NA))
data$herpesAnswer <- ifelse(data$herpesAnswer == 1, 1,
                            ifelse(data$herpesAnswer == 2, 0, NA))
data$undiagnosedHerpes <- ifelse(data$herpesTest == 1 & data$herpesAnswer == 0, 1, 0)
design <- svydesign(id = ~psu, strata = ~strata, weights = ~wtmec, nest = TRUE,
                    data = data[data$age >20,])
herpes <- evaluateModel(design, "undiagnosedHerpes", vars)
herpesCoef <- data.frame(var = names(herpes$coefficients),
                         coef = herpes$coefficients)
write.csv(herpesCoef, "../data/herpes_coefficients.txt", row.names = FALSE)
