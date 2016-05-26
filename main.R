source("evaluateModel.R")

# Read NHANES data
data <- read.csv("../data/NHANES.csv", stringsAsFactors = FALSE)

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
summary(diabetes)

# Hepatitis C
data$undiagnosedHepC <- ifelse(data$hepatitisC == 1, 1, 0)
data$undiagnosedHepC[is.na(data$undiagnosedHepC)] <- 0
design <- svydesign(id = ~psu, strata = ~strata, weights = ~wtmec, nest = TRUE,
                    data = data[!(is.na(data$wtmec)) & data$age > 20,])
hepatitisC <- evaluateModel(design, "undiagnosedHepC", vars)
summary(hepatitisC)

# Asthma
data$asthmaTest <- ifelse(data$asthmaTest == 1, 1,
                          ifelse(data$asthmaTest == 2, 0, NA))
data$asthmaAnswer <- ifelse(data$asthmaAnswer == 1, 1,
                            ifelse(data$asthmaAnswer == 2, 0, NA))
data$asthma <- ifelse(data$asthmaTest == 1 & data$asthmaAnswer == 0, 1, 0)
design <- svydesign(id = ~psu, strata = ~strata, weights = ~wtmec, nest = TRUE,
                    data = data[data$age >20,])
asthma <- evaluateModel(design, "asthma", vars)
summary(asthma)

# Herpes
data$herpesTest <- ifelse(data$herpesTest == 1, 1,
                          ifelse(data$herpesTest == 2, 0, NA))
data$herpesAnswer <- ifelse(data$herpesAnswer == 1, 1,
                            ifelse(data$herpesAnswer == 2, 0, NA))
data$undiagnosedHerpes <- ifelse(data$herpesTest == 1 & data$herpesAnswer == 0, 1, 0)
design <- svydesign(id = ~psu, strata = ~strata, weights = ~wtmec, nest = TRUE,
                    data = data[data$age >20,])
herpes <- evaluateModel(design, "undiagnosedHerpes", vars)
summary(fit)
