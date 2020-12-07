## R Script for Downloading the data
## This file is a third part of the STATS 506 Fall 2020 Final Project.
##
## (Brief Description)
## Repalce the forth line with a concise title for your script.
## Then describe your R script here in a few lines here. This 
## is a good place to document data sources.  
##
## Author(s): Suppapat Korsurat, skorsu@umich.edu
## Updated: November 29, 2020

# libraries: ------------------------------------------------------------------
library(dplyr)
library(data.table)
library(ggplot2)

# Directories: ----------------------------------------------------------------
path <- "/Users/kevin/506FA20/Stats506_Final_Project/"
main_url <- "https://raw.githubusercontent.com/skorsu/Stats506_Final_Project/"
repo <- "main/"
data_loc <- "Data/Cleaned_data.csv"

# Data: -----------------------------------------------------------------------
data <- suppressWarnings(fread(paste0(main_url, repo, data_loc)))

## Convert all variables into a factor variables
data$Hypertension <- factor(data$Hypertension)
data$Diabetes <- factor(data$Diabetes)
data$Gender <- factor(data$Gender)
data$Obesity <- factor(data$Obesity)
data$Age_Group <- factor(data$Age_Group)

# Analysis (Overall): ---------------------------------------------------------
summary(glm(Hypertension ~ Diabetes, data, family = binomial()))

# Analysis (Group): -----------------------------------------------------------
summary(glm(Hypertension ~ ., data, family = binomial()))

# 79: -------------------------------------------------------------------------
