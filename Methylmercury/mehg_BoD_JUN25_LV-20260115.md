MeHg model - probabilty of intellectual disability due to prenatal MeHg
exposure
================
LoVa3397
2026-01-15

- [Settings](#settings)
- [Exposure](#exposure)
- [Calculations](#calculations)

``` r
# MeHg model - probabilty of intellectual disability due to prenatal MeHg exposure
# Based on model in FERG1 - Zang et al (2019)
```

# Settings

``` r
rm(list=ls())

# packages
library(tidyverse)
library(mc2d)
library(dplyr)
library(readxl)
library(openxlsx)
library(devtools)
#install_github("brechtdv/FERG2")
library(FERG2)

# settings
set.seed(123)

nvar <- 1e5
nunc <- 1e3

mean_median_ci <-
  function(x) {
    c(mean = mean(x),
      median = median(x),
      quantile(x, probs = c(0.025, 0.975)))
  }

Territories <- read_xlsx("Territories_R_20250221.xlsx")
Flag_territory <- unlist(Territories)
```

# Exposure

``` r
# load exposure data
Exposure <- read.xlsx("final 2-ferg2-raw-data-v6-Methylmercury-v2.xlsx",sheet = "Incidence") #8507
Exposure$FLAG <- if_else(Exposure$HG_LEVEL == "Organic" | Exposure$HG_LEVEL == "Total" | is.na(Exposure$HG_LEVEL),
                         0,
                         5)

Exposure$FLAG_REF_LOCATION <- as.integer(apply(sapply(Flag_territory, function(x) grepl(x, Exposure$REF_LOCATION, ignore.case = TRUE)), 1, any))
Exposure$FLAG_REF_NOTES <- as.integer(apply(sapply(Flag_territory, function(x) grepl(x, Exposure$REF_NOTES, ignore.case = TRUE)), 1, any))
Exposure$FLAG_SOURCE_TITLE <- as.integer(apply(sapply(Flag_territory, function(x) grepl(x, Exposure$SOURCE_TITLE, ignore.case = TRUE)), 1, any))
Exposure$FLAG_TERRITORY <- if_else(Exposure$FLAG_REF_LOCATION + Exposure$FLAG_REF_NOTES + Exposure$FLAG_SOURCE_TITLE >=1 , 1, 0)

Exposure$FLAG <- if_else(Exposure$FLAG_TERRITORY == 1 & Exposure$FLAG == 0, 
                         1, 
                         Exposure$FLAG)
colnames(Exposure[26]) <- "REF_SAMPLE_TYPE"

Exposure$REF_SAMPLE_TYPE <- as.character(Exposure$REF_SAMPLE_TYPE)

Exposure$VALUE_UNIT <- str_trim(Exposure$VALUE_UNIT)

# Standardizing values
samples_clean <- str_trim(Exposure$REF_SAMPLE_TYPE) # Remove extra spaces
samples_clean <- str_to_title(samples_clean) # Capitalize first letter

# Ensuring consistent naming
samples_clean[samples_clean == "Blood"] <- "Blood"
samples_clean[samples_clean == "Hair"] <- "Hair"
samples_clean[samples_clean == "Serum"] <- "Serum"
Exposure$REF_SAMPLE_TYPE  <- samples_clean
Exposure_hair <- Exposure %>% filter(Exposure$REF_SAMPLE_TYPE == "Hair")
Exposure_bloodconverted <- Exposure %>% filter(Exposure$REF_SAMPLE_TYPE == "Blood")
Exposure_serum <- Exposure %>% filter(Exposure$REF_SAMPLE_TYPE == "Serum")
Exposure_hair$VALUE_MEAN <- as.numeric(Exposure_hair$VALUE_MEAN)
Exposure_hair$VALUE_MEDIAN <- as.numeric(Exposure_hair$VALUE_MEDIAN)

Exposure_bloodconverted$VALUE_MEAN <- as.numeric(Exposure_bloodconverted$VALUE_MEAN)
Exposure_bloodconverted$VALUE_MEDIAN <- as.numeric(Exposure_bloodconverted$VALUE_MEDIAN)
Exposure_serum$VALUE_MEAN <- as.numeric(Exposure_serum$VALUE_MEAN)
Exposure_serum$VALUE_MEDIAN <- as.numeric(Exposure_serum$VALUE_MEDIAN)
#use mean if available, if not use median!
for(i in 1:nrow(Exposure_hair)) {
  if (is.na(Exposure_hair$VALUE_MEAN[i])) {
    Exposure_hair$VALUE_MEAN[i] <- Exposure_hair$VALUE_MEDIAN[i]
    
  } else {}
  
}

for(i in 1:nrow(Exposure_bloodconverted)) {
  if (is.na(Exposure_bloodconverted$VALUE_MEAN[i])) {
    Exposure_bloodconverted$VALUE_MEAN[i] <- Exposure_bloodconverted$VALUE_MEDIAN[i]
    
  } else {}
  
}

for(i in 1:nrow(Exposure_serum)) {
  if (is.na(Exposure_serum$VALUE_MEAN[i])) {
    Exposure_serum$VALUE_MEAN[i] <- Exposure_serum$VALUE_MEDIAN[i]
    
  } else {}
  
}

#Convert all units to micrograms per gram
# Create a conversion lookup table
conversion_factors <- list(
  "µg/g" = 1,
  "mg/g" = 1000,
  "mg/100g" = 10,
  "mg/kg" = 1,
  "mg/Kg" = 1,
  "micg/g" =1 ,
  "ng/g" = 0.001,
  "mIU/g" = 1,
  "mIU/kg" = 0.001,
  "ng/mg" = 1,
  "micg/ng" = 0.001,
  "ng/mg" = 1,
  "pg/kg" = 0.000000001,
  "ppb" = 0.001,
  "ppm" = 1,
  "micg/kg" = 0.001,
  "micg/Kg" = 0.001
)

units_to_exclude <- c("micg/gdw","micg","micg/L","mg/dL", "micg/mg", "micmole/g", "mmole/g", "ng/mL", "nmol/g","ppb/mL","NA","micg/L (Whole blood digested in high purity acid and reported as mg/L on a blood density of 1.056.)")

# Exposure_hair <- Exposure_hair[!Exposure_hair$VALUE_UNIT %in% units_to_exclude, ]
Exposure_hair$FLAG <- if_else(Exposure_hair$VALUE_UNIT %in% units_to_exclude & Exposure_hair$FLAG == 0,
                              5,
                              Exposure_hair$FLAG)

# Apply the function to each row
Exposure_hair$converted_value <- 0

for(i in 1:nrow(Exposure_hair)) {
  value = as.numeric(Exposure_hair$VALUE_MEAN[i])
  unit = Exposure_hair$VALUE_UNIT[i]
  
  if (unit %in% names(conversion_factors)) {
    Exposure_hair$converted_value[i] <- value * as.numeric(conversion_factors[[unit]])
    print(unit)
  } else {
    Exposure_hair$converted_value[i] <- value
  }
  
}

#Convert all units to micrograms per Liter
# Create a conversion lookup table
conversion_factors <- list(
  "µg/L" = 1,
  "micg/L" = 1,
  "mg/dL" = 10000,
  "mic/L" = 1,
  "mg/L" =1000,
  "micg/dL" = 10,
  "mcg/micL" = 1000000,
  "micg/mL" = 1000,
  "ng/mL" = 0.001,
  "pg/mL" = 0.000001
  )

units_to_exclude <- c("micg/g",
                      "micg/kg",
                      "micmol/L",
                      "micmole/kg",
                      "micromol/L",
                      "nmole/L",
                      "ng/g",
                      "ng/mg",
                      "nM",
                      "nmol/g",
                      "nmol/L",
                      "nmol/mL",
                      "pg/g",
                      "pg/kg",
                      "ppb/mL",
                      "ppb","ppm")

Exposure_bloodconverted$FLAG <- if_else(Exposure_bloodconverted$VALUE_UNIT %in% units_to_exclude & Exposure_bloodconverted$FLAG == 0,
                                        5,
                                        Exposure_bloodconverted$FLAG)

# Apply the function to each row
Exposure_bloodconverted$converted_value <- 0

for(i in 1:nrow(Exposure_bloodconverted)) {
  value = as.numeric(Exposure_bloodconverted$VALUE_MEAN[i])
  unit = Exposure_bloodconverted$VALUE_UNIT[i]
  
  if (unit %in% names(conversion_factors)) {
    Exposure_bloodconverted$converted_value[i] <- value * as.numeric(conversion_factors[[unit]])
    print(unit)
  } else {
    Exposure_bloodconverted$converted_value[i] <- value
  }
  
}
Exposure_serum$converted_value <- 0

for(i in 1:nrow(Exposure_serum)) {
  value = as.numeric(Exposure_serum$VALUE_MEAN[i])
  unit = Exposure_serum$VALUE_UNIT[i]
  
  if (unit %in% names(conversion_factors)) {
    Exposure_serum$converted_value[i] <- value * as.numeric(conversion_factors[[unit]])
    print(unit)
  } else {
    Exposure_serum$converted_value[i] <- value
  }
  
}

###Spoedcial filter for doubts
#Get converted as VALUE_MEAN
Exposure_hair$VALUE_MEAN <- as.numeric(Exposure_hair$converted_value)
Exposure_hair$FLAG <- if_else(is.na(Exposure_hair$VALUE_MEAN) & Exposure_hair$FLAG == 0,
                              5,
                              Exposure_hair$FLAG)
Exposure_bloodconverted$VALUE_MEAN <- as.numeric(Exposure_bloodconverted$converted_value)
Exposure_bloodconverted$FLAG <- if_else(is.na(Exposure_bloodconverted$VALUE_MEAN) & Exposure_bloodconverted$FLAG == 0,
                              5,
                              Exposure_bloodconverted$FLAG)
Exposure_serum$VALUE_MEAN <- as.numeric(Exposure_serum$converted_value)
Exposure_serum$FLAG <- if_else(is.na(Exposure_serum$VALUE_MEAN) & Exposure_serum$FLAG == 0,
                                        5,
                               Exposure_serum$FLAG)

# Units should be in micrograms per gram

# 250: 1 blood to hair ratio
Exposure_bloodconverted$VALUE_MEAN <- Exposure_bloodconverted$VALUE_MEAN/250

Exposure_bloodconverted$FLAG <- if_else(!complete.cases(Exposure_bloodconverted$VALUE_MEAN) & Exposure_bloodconverted$FLAG == 0,
                                        5,
                                        Exposure_bloodconverted$FLAG)

Exposure_bloodconverted$FLAG <- if_else(is.na(Exposure_bloodconverted$VALUE_MEAN) & Exposure_bloodconverted$FLAG == 0,
                                        5,
                                        Exposure_bloodconverted$FLAG)

#Introduce censoring of results. If results exceed the value of 6.2 ug/g of the converted value, then it will be assigned the value of 6.2 ug/g.

Exposure_bloodconverted$VALUE_MEAN[Exposure_bloodconverted$VALUE_MEAN > 6.2] <- 6.2

Exposure_hair$VALUE_MEAN[Exposure_hair$VALUE_MEAN > 6.2] <- 6.2


#dose response reported in Bellinger et al. 2019. (Axelrad 2007) - deterministic (-0.18 points (95% CI: -0.38, -0.01) for each µg/g increase in maternal hair mercury) 
IQdecrease <- function(x) {
  decrease <- x * -0.18
  return(decrease)
}

Exposure_hair$IQdecrease <- c()

#Hair
for( i in 1:nrow(Exposure_hair)) {
  x <- Exposure_hair[i,]
  x <- as.numeric(x$VALUE_MEAN)
  
  Exposure_hair$IQdecrease[i] <- IQdecrease(x)
}

#Blood
for( i in 1:nrow(Exposure_bloodconverted)) {
  x <- Exposure_bloodconverted[i,]
  x <- as.numeric(x$VALUE_MEAN)
  
  Exposure_bloodconverted$IQdecrease[i] <- IQdecrease(x)
}

# Merge blood and hair in one single dataframe
Exposure_UNK <- subset(Exposure, is.na(REF_SAMPLE_TYPE))
Exposure_UNK$converted_value <- NA
Exposure_UNK$IQdecrease <- NA
Exposure_UNK$FLAG <- 6
Exposure_UNK <- subset(Exposure_UNK, !is.na(SOURCE_ID))
Exposure_serum$IQdecrease <- NA
All_exposure <- rbind(Exposure_hair, Exposure_bloodconverted, Exposure_serum, Exposure_UNK) # file for country consultation

# Lot of data is disaggregated which slows down the model, so for this reason data is aggregated (done in a similar way to brucella)
# write.xlsx(All_exposure, "Methylmercury_20251006.xlsx")
# Cleaned version and _LV version added ISO3 codes when missing, _LSJ1_LV cleaned version with information LEa
All_exposure_clean <- read.xlsx("Methylmercury_20251006_LSJ1_LV.xlsx", sheet = "Sorted on SOURCE ID")
All_exposure_clean$FINAL_FLAG <- if_else(All_exposure_clean$FLAG_DISAGGREGATION == 9 | is.na(All_exposure_clean$FLAG_DISAGGREGATION), 
                                         All_exposure_clean$FLAG, 
                                         if_else(All_exposure_clean$FLAG_DISAGGREGATION == 1,
                                                 5,
                                                 All_exposure_clean$FLAG_DISAGGREGATION))
# Update 23/10 as some didn't get a flag, all were unclear so no decision made and not included in model
All_exposure_clean$FINAL_FLAG <- if_else(is.na(All_exposure_clean$FINAL_FLAG),
                                         5, 
                                         All_exposure_clean$FINAL_FLAG)
All_exposure_clean$FINAL_FLAG <- if_else(All_exposure_clean$FINAL_FLAG == 0 & All_exposure_clean$REF_SAMPLE_TYPE == "Serum",
                                         5,
                                         All_exposure_clean$FINAL_FLAG )

# Remove studies from Brazil, see e-mail Lea 03/09/2025
All_exposure_clean$FINAL_FLAG <- if_else(All_exposure_clean$SOURCE_ID %in% c(1078, 300, 1045, 1159),
                         5,
                         All_exposure_clean$FINAL_FLAG)
All_exposure_clean$FINAL_FLAG <- factor(All_exposure_clean$FINAL_FLAG, 
                                        levels = c(0,1,2,3,4,5,6, 7),
                                        labels = c("Keep data", "Data part of non WHO member states", "No WHO REG2 given",
                                                   "Year before 1990", "yi can't be calcualted", "TF choice to remove", 
                                                   "Excluded by preliminary checks", "Excluded in data cleaning"))

All_exposure_clean$Record_ID <- seq.int(nrow(All_exposure_clean))
Date <- format(Sys.Date(), "%Y%m%d")

Exposureall <- subset(All_exposure_clean, FINAL_FLAG == "Keep data")

```
# Calculations
``` r

#Normal IQ distribution
IQnorm <- rnorm(nvar, mean = 100, sd = 15)

#Empty
IQnormal <- matrix(0, nrow = nrow(Exposureall), ncol = 1)
IQborderline <- matrix(0, nrow = nrow(Exposureall), ncol = 1)
IQmild <- matrix(0, nrow = nrow(Exposureall), ncol = 1)
IQmoderate <- matrix(0, nrow = nrow(Exposureall), ncol = 1)
IQsevere <- matrix(0, nrow = nrow(Exposureall), ncol = 1)
IQprofound <- matrix(0, nrow = nrow(Exposureall), ncol = 1)


#Shift of the decrease
#New ICD11 definitions? Check with WHO!
for ( i in 1:nrow(Exposureall)){
  IQ_decrease <- as.numeric(Exposureall$IQdecrease[i])
  
  normal <- pnorm(85-IQ_decrease, mean = 100, sd = 15, lower.tail = F) - pnorm(85, mean = 100, sd = 15, lower.tail = F) # x >= 85
  IQnormal[i,] <- normal
  
  borderline <-  pnorm(85-IQ_decrease, 100, 15, lower.tail = T) - pnorm(85, 100, 15, lower.tail = T) # x >= 70 & x < 85 Not in FERG1
  IQborderline[i,] <- borderline
  
  mild <- pnorm(70-IQ_decrease, 100, 15, lower.tail = T) - pnorm(70, 100, 15, lower.tail = T) #x >= 50 & x < 70
  IQmild[i,] <- mild
  
  moderate <- pnorm(50-IQ_decrease, 100, 15, lower.tail = T) - pnorm(50, 100, 15, lower.tail = T) #x >= 35 & x < 50
  IQmoderate[i,] <- moderate
  
  severe <- pnorm(35-IQ_decrease, 100, 15, lower.tail = T) - pnorm(35, 100, 15, lower.tail = T) #x >= 20 & x < 35
  IQsevere[i,] <- severe
  
  profound <- pnorm(20-IQ_decrease, 100, 15, lower.tail = T) - pnorm(20, 100, 15, lower.tail = T) #x < 20
  IQprofound[i,] <- profound
  
}

### Keep one single values instead of distributions
IQborderline <- as.data.frame(IQborderline)
IQmild <- as.data.frame(IQmild)
IQmoderate <-as.data.frame( IQmoderate)
IQsevere <- as.data.frame(IQsevere)
IQprofound <- as.data.frame(IQprofound)

IQborderline <- cbind.data.frame(Exposureall[,c("Record_ID", "REF_LOCATION","REF_LOCATION_ISO3","REF_YEAR_START","REF_YEAR_END","SOURCE_YEAR","SOURCE_ID", "REF_SAMPLE_TYPE", "OPT_STUDY_TYPE", "OPT_OTHER_STUDY_TYPE", "VALUE_MEAN", "REF_LOC_LEVEL", "REF_AGE_START", "REF_AGE_END", "REF_SEX", "OPT_SUBPOP", "HG_LEVEL")],IQborderline)
IQmild <- cbind.data.frame(Exposureall[,c("Record_ID", "REF_LOCATION","REF_LOCATION_ISO3","REF_YEAR_START","REF_YEAR_END","SOURCE_YEAR", "SOURCE_ID", "REF_SAMPLE_TYPE", "OPT_STUDY_TYPE", "OPT_OTHER_STUDY_TYPE", "VALUE_MEAN", "REF_LOC_LEVEL", "REF_AGE_START", "REF_AGE_END", "REF_SEX", "OPT_SUBPOP", "HG_LEVEL")],IQmild)
IQmoderate <- cbind.data.frame(Exposureall[,c("Record_ID", "REF_LOCATION","REF_LOCATION_ISO3","REF_YEAR_START","REF_YEAR_END","SOURCE_YEAR", "SOURCE_ID", "REF_SAMPLE_TYPE", "OPT_STUDY_TYPE", "OPT_OTHER_STUDY_TYPE", "VALUE_MEAN", "REF_LOC_LEVEL", "REF_AGE_START", "REF_AGE_END", "REF_SEX", "OPT_SUBPOP", "HG_LEVEL")],IQmoderate)
IQsevere <- cbind.data.frame(Exposureall[,c("Record_ID", "REF_LOCATION","REF_LOCATION_ISO3","REF_YEAR_START","REF_YEAR_END","SOURCE_YEAR", "SOURCE_ID", "REF_SAMPLE_TYPE", "OPT_STUDY_TYPE", "OPT_OTHER_STUDY_TYPE", "VALUE_MEAN", "REF_LOC_LEVEL", "REF_AGE_START", "REF_AGE_END", "REF_SEX", "OPT_SUBPOP", "HG_LEVEL")],IQsevere)
IQprofound <- cbind.data.frame(Exposureall[,c("Record_ID", "REF_LOCATION","REF_LOCATION_ISO3","REF_YEAR_START","REF_YEAR_END","SOURCE_YEAR", "SOURCE_ID", "REF_SAMPLE_TYPE", "OPT_STUDY_TYPE", "OPT_OTHER_STUDY_TYPE", "VALUE_MEAN", "REF_LOC_LEVEL", "REF_AGE_START", "REF_AGE_END", "REF_SEX", "OPT_SUBPOP", "HG_LEVEL")],IQprofound)

IQborderline$Exposure <- IQborderline$VALUE_MEAN
IQborderline$VALUE_MEAN <- NULL
IQmild$Exposure <- IQmild$VALUE_MEAN
IQmild$VALUE_MEAN <- NULL
IQmoderate$Exposure <- IQmoderate$VALUE_MEAN
IQmoderate$VALUE_MEAN <- NULL
IQsevere$Exposure <- IQsevere$VALUE_MEAN
IQsevere$VALUE_MEAN <- NULL
IQprofound$Exposure <- IQprofound$VALUE_MEAN
IQprofound$VALUE_MEAN <- NULL

colnames(IQborderline[3]) <- "Mean"
colnames(IQmild[3]) <- "Mean"
colnames(IQmoderate[3]) <- "Mean"
colnames(IQsevere[3]) <- "Mean"
colnames(IQprofound[3]) <- "Mean"

#combine all incidence in one
IQAll <- cbind.data.frame(IQborderline,IQmild[,"V1"],IQmoderate[,"V1"],IQsevere[,"V1"],IQprofound[,"V1"])
IQAll$Exposure <- Exposureall$VALUE_MEAN
```
