## R Script for Downloading the data
## This file is a first part of the STATS 506 Fall 2020 Final Project.
##
## (Brief Description)
## Repalce the first line with a concise title for your script.
## Then describe your R script here in a few lines here. This 
## is a good place to document data sources.  
##
## Author(s): Suppapat Korsurat, skorsu@umich.edu
## Updated: November 29, 2020

# libraries: ------------------------------------------------------------------
## For the problem regarding to the import XPT file into R by using url,
## I apply the method from https://stackoverflow.com/a/53788422
library(haven)
library(dplyr)

# directories: ----------------------------------------------------------------
path <- "/Users/kevin/506FA20/Stats506_Final_Project/Data"
main_url <- "https://wwwn.cdc.gov/Nchs/Nhanes/"

# Create a folder for storing dataset -----------------------------------------
if(! dir.exists(path)){
  dir.create((file.path(path)))
}

# Function for download and select variables ----------------------------------
## Demographic Data
demo_download <- function(part_url){
  url <- paste0(main_url, part_url)
  read_xpt(url) %>%
    mutate(Gender = ifelse(RIAGENDR == 1, "Male", "Female")) %>%
    select(ID = SEQN, Gender, Age = RIDAGEYR)
}

## BMI Data
bmi_download <- function(part_url){
  url <- paste0(main_url, part_url)
  read_xpt(url) %>%
    mutate(BMI = ifelse(is.na(BMXWT) | is.na(BMXHT), -2,
                        BMXWT/((BMXHT/100)^2))) %>%
    select(ID = SEQN, BMI)
}

## Blood Preesure Data
bp_download <- function(part_url){
  url <- paste0(main_url, part_url)
  read_xpt(url) %>%
    select(ID = SEQN, BPXSY1, BPXDI1, BPXSY2, BPXDI2, 
           BPXSY3, BPXDI3, BPXSY4, BPXDI4)
}

## Diabetes Data
diabetes_download <- function(part_url){
  url <- paste0(main_url, part_url)
  read_xpt(url) %>%
    mutate(Diabetes = ifelse(DIQ010 == 1, "Yes",
                             ifelse(DIQ010 == 2, "No", "Unknown"))) %>%
    select(ID = SEQN, Diabetes)
}

# Download and merge the demographic data -------------------------------------
## The last part of the URL for doanloading the data
demo_set <- c("2017-2018/DEMO_J.XPT", "2015-2016/DEMO_I.XPT",
              "2013-2014/DEMO_H.XPT", "2011-2012/DEMO_G.XPT")
bmi_set <- c("2017-2018/BMX_J.XPT", "2015-2016/BMX_I.XPT",
             "2013-2014/BMX_H.XPT", "2011-2012/BMX_G.XPT")
bp_set <- c("2017-2018/BPX_J.XPT", "2015-2016/BPX_I.XPT",
            "2013-2014/BPX_H.XPT", "2011-2012/BPX_G.XPT")
diabetes_set <- c("2017-2018/DIQ_J.XPT", "2015-2016/DIQ_I.XPT",
                  "2013-2014/DIQ_H.XPT", "2011-2012/DIQ_G.XPT")

## Join the data
demo_data <- c()
bmi_data <- c()
bp_data <- c()
diabetes_data <- c()

for(i in 1:4){
  demo_data <- rbind(demo_data, demo_download(demo_set[i]))
  bmi_data <- rbind(bmi_data, bmi_download(bmi_set[i]))
  bp_data <- rbind(bp_data, bp_download(bp_set[i]))
  diabetes_data <- rbind(diabetes_data, diabetes_download(diabetes_set[i]))
}

# Inner joins four tables and export CSV --------------------------------------
inner_join(demo_data, bmi_data, by = "ID") %>%
  inner_join(bp_data, by = "ID") %>%
  inner_join(diabetes_data, by = "ID") %>%
  write.csv(paste0(path, "/Data.csv"), row.names = FALSE)

# 79: -------------------------------------------------------------------------
