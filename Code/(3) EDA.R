## R Script for Downloading the data
## This file is a third part of the STATS 506 Fall 2020 Final Project.
##
## (Brief Description)
## Repalce the first line with a concise title for your script.
## Then describe your R script here in a few lines here. This 
## is a good place to document data sources.  
##
## Author(s): Suppapat Korsurat, skorsu@umich.edu
## Updated: November 29, 2020

# libraries: ------------------------------------------------------------------
library(dplyr)
library(data.table)
library(ggplot2)

# directories: ----------------------------------------------------------------
path <- "/Users/kevin/506FA20/Stats506_Final_Project/Data"
main_url <- "https://raw.githubusercontent.com/skorsu/Stats506_Final_Project/"
repo <- "main/"

# data: -----------------------------------------------------------------------
data_loc <- "Data/Cleaned_data.csv"
data <- suppressWarnings(fread(paste0(main_url, repo, data_loc)))

head(data)

table(data$Diabetes, data$Hypertension)
table(data$Diabetes, data$Hypertension, data$Obesity)
table(data$Diabetes, data$Hypertension, data$Age_Group)
table(data$Diabetes, data$Hypertension, data$Gender)

# 79: -------------------------------------------------------------------------
