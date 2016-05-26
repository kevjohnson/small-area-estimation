library(dplyr)

source("getNHANES.R")

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
data1 <- getNhanes(years = c(2001, 2003), files = files, dir = dir,
                   variables = variables)
colnames(data1) <- c("id", "psu", "strata", "wtint", "wtmec", "gender", "age",
                     "race", "edu", "mar", "hhsize", "poverty", "diabetes",
                     "wtfast", "glucose", "chlamydiaTest", "chlamydiaAnswer", 
                     "hepatitisC", "asthmaAnswer", "herpesAnswer", "herpesTest",
                     "healthInsurance")

# 2005-2011
variables <- list(c("SEQN", "SDMVPSU", "SDMVSTRA", "WTINT2YR", "WTMEC2YR", 
                    "RIAGENDR", "RIDAGEYR", "RIDRETH1", "DMDEDUC2", "DMDMARTL", "DMDHHSIZ", 
                    "INDFMPIR"), c("SEQN", "DIQ010"), c("SEQN", "WTSAF2YR", "LBXGLU"), 
                  c("SEQN", "URXUCL"), c("SEQN", "SXQ272"), c("SEQN", "HCQ030"), c("SEQN",
                                                                                   "MCQ010"), c("SEQN", "SXQ260"), c("SEQN", "LBXHE2"), c("SEQN", "HIQ011"))
files <- c("DEMO", "DIQ", "GLU", "CHLMDA", "SXQ", "HCQ", "MCQ", "SXQ", "HSV", "HIQ")
data2 <- getNhanes(years = c(2005, 2007, 2009, 2011), files = files, dir = dir, variables = variables)
colnames(data2) <- c("id", "psu", "strata", "wtint", "wtmec", "gender", "age",
                     "race", "edu", "mar", "hhsize", "poverty", "diabetes", "wtfast", "glucose",
                     "chlamydiaTest", "chlamydiaAnswer", "hepatitisC", "asthmaAnswer",
                     "herpesAnswer", "herpesTest", "healthInsurance")

# Asthma
variables <- list(c("SEQN", "SPDBRONC"))
files <- c("SPX")
data <- getNhanes(years = c(2007, 2009, 2011), files = files, dir = dir, variables = variables)
colnames(data) <- c("id", "asthmaTest")

out <- rbind(data1, data2)
out <- left_join(out, data)
write_csv(out, "../data/NHANES.csv")
