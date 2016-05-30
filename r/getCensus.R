loadCensusData <- function(geo = c("tract", "county")) {
    require(healthdata)
    require(readr)

    states <- getCensus(survey = "sf1", year = 2010, geo = "state:*",
                        vars = c("P0050001"))
    states <- states$state

    # Race
    vars <- c("P0040001", "P0040003", "P0050003", "P0050004")
    if (geo == "tract") {
        tract <- NULL
        for (s in states) {
            print(s)
            area = paste("state:", s, sep = "")
            tract <- rbind(tract, getCensus(survey = "sf1", year = 2010, 
                                            vars = vars, geo = "tract:*",
                                            area = area))
        }
        tract$geoid <- paste(tract$state, tract$county, tract$tract, sep = "")
        tract <- tract[,c(1:4,8)]
        colnames(tract) <- c("total", "hispanic", "white", "black", "geoid")
        tract$other <- tract$total - rowSums(tract[,c("hispanic", "white",
                                                      "black")])
        tract <- tract[,c("geoid", "white", "black", "hispanic", "other")]
        tract[,2:5] <- t(apply(tract[,2:5], 1, cumsum))
        tract[,2:5] <- tract[,2:5] / tract$other
        write_csv(tract, "../data/race.csv")
    } else {
        county <- getCensus(survey = "sf1", year = 2010, vars = vars,
                            geo = "county:*")
        county$geoid <- paste(county$state, county$county, sep = "")
        county <- county[,c(1:4,7)]
        colnames(county) <- c("total", "hispanic", "white", "black", "geoid")
        county$other <- county$total - rowSums(county[,c("hispanic", "white",
                                                         "black")])
        county <- county[,c("geoid", "white", "black", "hispanic", "other")]
        county[,2:5] <- t(apply(county[,2:5], 1, cumsum))
        county[,2:5] <- county[,2:5] / county$other
        write_csv(county, "../data/race.csv")
    }

    # Gender
    vars <- c("P0120002", "P0120026")
    if (geo == "tract") {
        tract <- NULL
        for (s in states) {
            print(s)
            area = paste("state:", s, sep = "")
            tract <- rbind(tract, getCensus(survey = "sf1", year = 2010, 
                                            vars = vars, geo = "tract:*",
                                            area = area))
        }
        tract$geoid <- paste(tract$state, tract$county, tract$tract, sep = "")
        tract <- tract[,c(1:2,6)]
        colnames(tract) <- c("male", "female", "geoid")
        tract <- tract[,c("geoid", "male", "female")]
        tract[,2:3] <- t(apply(tract[,2:3], 1, cumsum))
        tract[,2:3] <- tract[,2:3] / tract$female
        write_csv(tract, "../data/gender.csv")
    } else {
        county <- getCensus(survey = "sf1", year = 2010, vars = vars,
                            geo = "county:*")
        county$geoid <- paste(county$state, county$county, sep = "")
        county <- county[,c(1:2,5)]
        colnames(county) <- c("male", "female", "geoid")
        county <- county[,c("geoid", "male", "female")]
        county[,2:3] <- t(apply(county[,2:3], 1, cumsum))
        county[,2:3] <- county[,2:3] / county$female
        write_csv(county, "../data/gender.csv")
    }

    # Age
    male <- paste("PCT0", 120023:120105, sep = "")
    female <- paste("PCT0", 120127:120209, sep = "")
    if (geo == "tract") {
        tract <- NULL
        for (s in states) {
            print(s)
            area = paste("state:", s, sep = "")
            tractState <- cbind(getCensus(survey = "sf1", year = 2010,
                                          vars = male[1:50], geo = "tract:*",
                                          area = area),
                                getCensus(survey = "sf1", year = 2010,
                                          vars = male[51:length(male)],
                                          geo = "tract:*", area = area),
                                getCensus(survey = "sf1", year = 2010,
                                          vars = female[1:50], geo = "tract:*",
                                          area = area),
                                getCensus(survey = "sf1", year = 2010,
                                          vars = female[51:length(female)],
                                          geo = "tract:*", area = area)))
            tract <- rbind(tract, tractState)
        }
        tract$geoid <- paste(tract[,51], tract[,52], tract[,53], sep = "")
        tract <- tract[,c(179, 1:50, 54:86, 90:139, 143:175)]
        tract[,2:84] <- tract[,2:84] + tract[,85:167]
        tract <- tract[,1:84]
        colnames(tract) <- c("geoid", as.character(20:102))
        tract[,2:84] <- t(apply(tract[,2:84], 1, cumsum))
        tract[,2:84] <- tract[,2:84] / tract[,84]
        write_csv(tract, "../data/age.csv")
    } else {
        county <- cbind(getCensus(survey = "sf1", year = 2010,
                                  vars = male[1:50], geo = "county:*"),
                        getCensus(survey = "sf1", year = 2010,
                                  vars = male[51:length(male)],
                                  geo = "county:*"),
                        getCensus(survey = "sf1", year = 2010,
                                  vars = female[1:50], geo = "county:*"),
                        getCensus(survey = "sf1", year = 2010,
                                  vars = female[51:length(female)],
                                  geo = "county:*"))
        county$geoid <- paste(county[,51], county[,52], sep = "")
        county <- county[,c(175, 1:50, 53:85, 88:137, 140:172)]
        county[,2:84] <- county[,2:84] + county[,85:167]
        county <- county[,1:84]
        colnames(county) <- c("geoid", as.character(20:102))
        county[,2:84] <- t(apply(county[,2:84], 1, cumsum))
        county[,2:84] <- county[,2:84] / county[,84]
        write_csv(county, "../data/age.csv")
    }

    # Poverty
    vars <- c("C17002_001E", "C17002_008E")
    if (geo == "tract") {
        tract <- NULL
        for (s in states) {
            print(s)
            area = paste("state:", s, sep = "")
            tract <- rbind(tract, getCensus(survey = "acs5", year = 2010,
                                            vars = vars, geo = "tract:*",
                                            area = area))
        }
        tract$geoid <- paste(tract$state, tract$county, tract$tract, sep = "")
        tract$belowPovertyLine <- tract[,1] - tract[,2]
        tract <- tract[,c(6,7,2)]
        colnames(tract) <- c("geoid", "belowPovertyLine", "abovePovertyLine")
        tract[,2:3] <- t(apply(tract[,2:3], 1, cumsum))
        tract[,2:3] <- tract[,2:3] / tract[,3]
        write_csv(tract, "../data/poverty.csv")
    } else {
        county <- getCensus(survey = "acs5", year = 2010, vars = vars,
                            geo = "county:*")
        county$geoid <- paste(county$state, county$county, sep = "")
        county$belowPovertyLine <- county[,1] - county[,2]
        county <- county[,c(5,6,2)]
        colnames(county) <- c("geoid", "belowPovertyLine", "abovePovertyLine")
        county[,2:3] <- t(apply(county[,2:3], 1, cumsum))
        county[,2:3] <- county[,2:3] / county[,3]
        write_csv(county, "../data/poverty.csv")
    }

    # Education
    male <- paste("B15002_", substring(as.character(1003:1018),2,4), "E",
                  sep = "")
    female <- paste("B15002_", substring(as.character(1020:1035),2,4), "E",
                    sep = "")
    vars <- c(male, female)
    if (geo == "tract") {
        tract <- NULL
        for (s in states) {
            print(s)
            area = paste("state:", s, sep = "")
            tract <- rbind(tract, getCensus(survey = "acs5", year = 2010,
                                            vars = vars, geo = "tract:*",
                                            area = area))
        }
        tract$geoid <- paste(tract$state, tract$county, tract$tract, sep = "")
        tract[,1:16] <- tract[,1:16] + tract[,17:32]
        tract <- tract[,c(36,1:16)]
        tract$noHighSchool <- rowSums(tract[,2:9])
        tract$highSchool <- rowSums(tract[,10:12])
        tract$college <- rowSums(tract[,13:17])
        tract <- tract[,c(1,18:20)]
        tract[,2:4] <- t(apply(tract[,2:4], 1, cumsum))
        tract[,2:4] <- tract[,2:4] / tract[,4]
        write_csv(tract, "../data/education.csv")
    } else {
        county <- getCensus(survey = "acs5", year = 2010, vars = vars,
                            geo = "county:*")
        county$geoid <- paste(county$state, county$county, sep = "")
        county[,1:16] <- county[,1:16] + county[,17:32]
        county <- county[,c(35,1:16)]
        county$noHighSchool <- rowSums(county[,2:9])
        county$highSchool <- rowSums(county[,10:12])
        county$college <- rowSums(county[,13:17])
        county <- county[,c(1,18:20)]
        county[,2:4] <- t(apply(county[,2:4], 1, cumsum))
        county[,2:4] <- county[,2:4] / county[,4]
        write_csv(county, "../data/education.csv")
    }

    # Household Size
    owner <- paste("B25009_", substring(as.character(1003:1009),2,4), "E",
                   sep = "")
    renter <- paste("B25009_", substring(as.character(1011:1017),2,4), "E",
                    sep = "")
    vars <- c(owner, renter)
    if (geo == "tract") {
        tract <- NULL
        for (s in states) {
            print(s)
            area = paste("state:", s, sep = "")
            tract <- rbind(tract, getCensus(survey = "acs5", year = 2010,
                                            vars = vars, geo = "tract:*",
                                            area = area))
        }
        tract$geoid <- paste(tract$state, tract$county, tract$tract, sep = "")
        tract[,1:7] <- tract[,1:7] + tract[,8:14]
        tract <- tract[,c(18,1:7)]
        colnames(tract) <- c("geoid", as.character(1:7))
        tract[,2:8] <- t(apply(tract[,2:8], 1, cumsum))
        tract[,2:8] <- tract[,2:8] / tract[,8]
        write_csv(tract, "../data/hhsize.csv")
    } else {
        county <- getCensus(survey = "acs5", year = 2010, vars = vars,
                            geo = "county:*")
        county$geoid <- paste(county$state, county$county, sep = "")
        county[,1:7] <- county[,1:7] + county[,8:14]
        county <- county[,c(17,1:7)]
        colnames(county) <- c("geoid", as.character(1:7))
        county[,2:8] <- t(apply(county[,2:8], 1, cumsum))
        county[,2:8] <- county[,2:8] / county[,8]
        write_csv(county, "../data/hhsize.csv")
    }

    # Health Insurance
    maleNums <- c(1009,1010,1012,1013,1015,1016,1018,1019,1021,1022,1024,1025,
                  1027,1028)
    femaleNums <- c(1037,1038,1040,1041,1043,1044,1046,1047,1049,1050,1052,
                    1053,1055,1056)
    male <- paste("B27001_", substring(as.character(maleNums),2,4), "E",
                  sep = "")
    female <- paste("B27001_", substring(as.character(femaleNums),2,4), "E",
                    sep = "")
    vars <- c(male, female)
    if (geo == "tract") {
        tract <- NULL
        for (s in states) {
            print(s)
            area = paste("state:", s, sep = "")
            tract <- rbind(tract, getCensus(survey = "ac5", year = 2010,
                                            vars = c("B27001_001E"), 
                                            geo = "tract:*", area = area))
        }
    }
}
