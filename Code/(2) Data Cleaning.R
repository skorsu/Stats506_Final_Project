## R Script for Cleaning the data
## This file is a second part of the STATS 506 Fall 2020 Final Project.
##
## This file contains the code for cleaning the data downloaded from the 
## National Health and Nutrition Examination Survey (NHANES). This script
## must be executed after running (1) Download the data.R
##
## The things that I had done in this file are
##  - Filter out the missing value.
##  - Select the latest record of each respondent's blood pressure and 
##    cluster the respondent into two groups, depending on their 
##    blood pressure.
##  - Classify the obesity status by using the value of the Body 
##    Mass Index (BMI)
##  - Categorize each person into different age groups.
##
## In the end, the result from this file is the cleaned dataset.
##
## Author(s): Suppapat Korsurat, skorsu@umich.edu
## Updated: November 29, 2020

# libraries: ------------------------------------------------------------------
library(dplyr)
library(data.table)

# directories: ----------------------------------------------------------------
path <- "/Users/kevin/506FA20/Stats506_Final_Project/Data"
main_url <- "https://raw.githubusercontent.com/skorsu/Stats506_Final_Project/"
repo <- "main/"

# data: -----------------------------------------------------------------------
data <- suppressWarnings(fread(paste0(main_url, repo, "Data/Data.csv")))

# Blood Pressure: -------------------------------------------------------------
## I will use the lastest Blood Pressure data for each person.[]
bp_col <- c("ID", paste0(c("BPXSY", "BPXDI"), sort(rep(1:4, 2))))

## I applied the method provided here. https://stackoverflow.com/a/53620767
bp_adjusted <- data[, ..bp_col] %>%
  melt(measure = patterns("^BPXSY", "^BPXDI"), value.name = c("SY", "DI")) %>%
  arrange(ID, variable) %>%
  filter(! (is.na(SY) | is.na(DI))) %>%
  group_by(ID) %>%
  slice(n())

## Join the adjusted Blood Preesure back to the main dataset.
data <- data[, -c(paste0(c("BPXSY", "BPXDI"), sort(rep(1:4, 2))))] %>%
  inner_join(bp_adjusted, by = "ID")

## Determine the Hypertension Status
## Source: https://www.heart.org/en/health-topics/high-blood-pressure/
##         understanding-blood-pressure-readings
## Also, the Blood Pressure cannot be 0 for both Systolic and Diastolic.
## Hence, I will filter out the observation that Sytolic < 60 or Diastolic < 40

data <- data[SY > 60 & DI > 40, -c("variable")] %>%
  .[, ":=" (Hypertension = ifelse(SY < 130 & DI < 80, "No", "Yes"))] %>% 
  .[, -c("SY", "DI")]

# Diabetes: -------------------------------------------------------------------
## Delete Unknown from the dataset.
data <- data[Diabetes != "Unknown", ]

# Obesity: --------------------------------------------------------------------
## Source: https://www.mayoclinic.org/diseases-conditions/obesity/
##         symptoms-causes/syc-20375742
## There are some observations that contains error, hence I will delete those 
## observations out from the analysis.

data <- data[BMI != -2, ] %>%
  .[, ":=" (Obesity = ifelse(BMI < 30.0, "No", "Yes"))] %>%
  .[, -c("BMI")]

# Age Group: ------------------------------------------------------------------
## Source: https://www.cdc.gov/nchs/data/databriefs/db289.pdf
## I will consider only the people who above 18. I will classify the age into 4
## groups, similar to the method used in the provided link.

data <- data[Age >= 18, ] %>%
  .[, ":=" (Age_Group = ifelse(Age <= 39, "Adult",
                               ifelse(Age >= 60, "Senior", "Middle-Aged")))] %>%
  .[, -c("Age")]

# Export to csv ---------------------------------------------------------------
write.csv(data, paste0(path, "/Cleaned_data.csv"), row.names = FALSE)

# 79: -------------------------------------------------------------------------
