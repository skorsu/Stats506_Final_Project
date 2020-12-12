## R Script for performing EDA and building models for analysis.
## This file is a third part of the STATS 506 Fall 2020 Final Project.
##
## This file contains the code for performing exploratory data analysis (EDA) 
## and creating logistic regression models. There are two user-defined 
## functions, which are eda_1var and eda_cross. This file must be executed 
## after (2) Data Cleaning.R
##
## Author(s): Suppapat Korsurat, skorsu@umich.edu
## Updated: November 29, 2020

# libraries: ------------------------------------------------------------------
library(tidyverse)
library(data.table)
library(ggplot2)
library(gridExtra)

# Directories: ----------------------------------------------------------------
path <- "/Users/kevin/506FA20/Stats506_Final_Project/"
main_url <- "https://raw.githubusercontent.com/skorsu/Stats506_Final_Project/"
repo <- "main/"
data_loc <- "Data/Cleaned_data.csv"

# Create a folder for storing figures -----------------------------------------
if(! dir.exists(paste0(path, "Plots"))){
  dir.create((file.path(paste0(path, "Plots"))))
}

# Data: -----------------------------------------------------------------------
data <- suppressWarnings(fread(paste0(main_url, repo, data_loc)))

## Convert the type of variables into factor variables
data <- data.frame(data[,1], sapply(data[,-1], factor))

# Function: -------------------------------------------------------------------

## Function: Number with comma.
### Input: Number that you wish to convert into xx,xxx form
### Output: Number in xx,xxx form

comma_th <- function(number){
  if(number < 1000){
    return(paste0(number))
  }
  else {
    after <- number %% 1000
    if(after <= 9){
      after <- paste0("00", after)
    } else if (after <= 99) {
      after <- paste0("0", after)
    }
    
    before <- floor(number/1000)
    return(paste0(before, ",", after))
  }
}

## Function: Bar chart for one variable.
### Input: (Required) - Variable name 
###        (Optional) - Color of the bar (default = "salmon")
###        (Optional) - Size of the number and percent (default = 3.5)
### Output: Bar chart

#### I have adapted the method provided in https://stackoverflow.com/a/14577878
#### in order to use the function input to be the labels of X axis and title of
#### the plot.

eda_1var <- function(vari, col_bar = "salmon", num_size = 3.5){
  eda_tab <- data %>% 
    group_by({{vari}}) %>% 
    summarise(n = n()) %>% 
    mutate(Percent = 100 * n/sum(n))
  
  eda_tab$n <- unlist(lapply(eda_tab$n, comma_th))
  
  eda_plot <- ggplot(eda_tab, 
                     aes_string(x = names(eda_tab)[1], 
                                y = names(eda_tab)[3])) +
    geom_bar(stat = "identity", width = 0.3, fill = col_bar) +
    theme_bw() +
    geom_text(aes(label = paste0("(", n, ")")), 
              vjust = 1.5, color = "black", size = num_size) +
    geom_text(aes(label = paste0(round(Percent,2), "%")), 
              vjust = -0.25, color = "black", size = num_size) +
    scale_y_continuous(name = "Percent", limits = c(0, 100))
  
  if(! deparse(substitute(vari)) %in% c("Gender", "Age_Group")){
    eda_plot <- eda_plot +
      scale_x_discrete(limits=c("Yes", "No"))
  }
  
  if(deparse(substitute(vari)) == "Age_Group"){
    eda_plot <- eda_plot +
      labs(x = "Age Group", title = "Age Group")
  } else {
    eda_plot <- eda_plot +
      labs(x = deparse(substitute(vari)), title = (deparse(substitute(vari)))) 
  }
}

## Function: Balance Table
### Input: Variable name
### Output: (Number of class for that variable) x 5 tables, providing the 
###         information on the percentage for each class, along with the 
###         percentage by each class of hypertension status.

balance_table <- function(vari, data_used = data){
  
  ## Relevel the factor variables
  data_used$Hypertension <- relevel(data_used$Hypertension, 2)
  data_used$Diabetes <- relevel(data_used$Diabetes, 2)
  data_used$Obesity <- relevel(data_used$Obesity, 2)
  
  ## Initial the table
  bal_tab <- data_used %>%
    group_by({{vari}}, Hypertension) %>%
    summarise(n = n()) %>%
    ungroup() %>%
    spread(Hypertension, n)
  
  ## Calculate the p-value of the test of independence between
  ## hypertension and interested variable.
  pval <- data_used %>% 
    select(Hypertension, {{vari}}) %>% 
    table %>% 
    chisq.test() %>% 
    .$p.value %>%
    round(4)
  
  pval <- c(pval, rep("", dim(bal_tab)[1] - 1))
  
  ## Column for Variable Name
  if(colnames(bal_tab)[1] != "Age_Group"){
    v_name <- c(colnames(bal_tab)[1], rep("", dim(bal_tab)[1] - 1))
    
  } else {
    v_name <- c("Age Group", rep("", dim(bal_tab)[1] - 1))
  }

  ## Create the Summary table
  colnames(bal_tab)[1] <- "Class"
  bal_tab$row_sum <- apply(bal_tab[,-1], 1, sum)
  bal_tab %>% 
    mutate(HY = paste0(comma_th(bal_tab$Yes), " (", 
                       round(100*Yes/row_sum, 2), "%)"),
           HN = paste0(comma_th(bal_tab$No), " (", 
                       round(100*No/row_sum, 2), "%)"),
           by_var = paste0(comma_th(bal_tab$row_sum), " (", 
                           round(100*row_sum/sum(row_sum), 2), "%)"),
           Variable = v_name,
           pval = pval) %>%
    select(Variable, Class,
           "Total (% Total)" = by_var,
           "Hypertension: Yes" = HY, 
           "Hypertension: No" = HN,
           "p-value" = pval) %>%
    return()
}

## Function: Detect the confounding variable.
### Input: Variable of interested
### Output: (Number of class for that variable) x 2 tables, providing the 
###         change in the coefficients.

#### I have applied the method demonstrated in this link.
#### https://rpubs.com/josevilardy/confounding

confound_detect <- function(vari){
  ## Full Model
  all_model <- glm(Hypertension ~ Diabetes + Gender + Obesity + Age_Group, 
                   data, family = binomial())
  all_coeff <- all_model$coefficients[-1]
  
  ## Model with only interested variable
  dummy_data <- data %>% select(Hypertension,{{vari}})
  interested_model <- glm(Hypertension ~ ., 
                          dummy_data, family = binomial())
  interested_coeff <- interested_model$coefficients[-1]
  
  ## Filter only interested coefficients from full model
  all_coeff <- all_coeff[names(all_coeff) %in% names(interested_coeff)]
  
  ## Create the summary table
  if(colnames(dummy_data)[2] != "Age_Group"){
    interested_var = colnames(dummy_data)[2]
  } else {
    interested_var = paste0("Age Group: ", c("Middle-Aged", "Senior"))
  }
  
  data.frame(all_coeff, interested_coeff) %>%
    mutate(Variable = interested_var,
           change = abs(100*(interested_coeff - all_coeff)/all_coeff),
           Change = paste0(round(change, 2), "%")) %>%
    select(Variable, "Change in coefficient" = Change) %>%
    return()
}

# Exploratory Data Analysis ---------------------------------------------------

## Hypertension
print(eda_1var(Hypertension, "aquamarine2"))

ggsave(paste0(path, "Plots/Hypertension.png"), 
       eda_1var(Hypertension, "aquamarine2", 4.5))

## (Output) Table 1: Descriptive Statistics
bal_tab_all <- rbind(suppressWarnings(balance_table(Diabetes)),
                     suppressWarnings(balance_table(Obesity)),
                     suppressWarnings(balance_table(Gender)),
                     suppressWarnings(balance_table(Age_Group)))

# Analysis: -------------------------------------------------------------------
## Create two models: Overall and All variables model.
overall_mod <- glm(Hypertension ~ Diabetes, data, family = binomial())
all_mod <- glm(Hypertension ~ Diabetes + Gender + Obesity + Age_Group, 
               data, family = binomial())

## Detect the confounding variables
confounding_table <- rbind(confound_detect(Gender),
                           confound_detect(Obesity),
                           confound_detect(Age_Group))

## Model without Gender
no_confound_mod <- glm(Hypertension ~ Diabetes + Obesity + Age_Group, 
                       data, family = binomial())

# Result Tables: --------------------------------------------------------------
## (Output) Table 2: Overall Model and All-variables Model.
output_2 <- rbind(summary(overall_mod)$coefficients[-1,c(1,2,4)],
                  rep(-99,3), ## Additional Line for sepereating models
                  summary(all_mod)$coefficients[-1,c(1,2,4)])

rownames(output_2) <- NULL
colnames(output_2) <- c("log_odd", "se", "pval")

model_type <- c("Overall Model", "", "All-variables Model", rep("", 4))
variable_class <- c("Diabetes: Yes", "", "Diabetes: Yes", "Gender: Male",
                    "Obesity: Yes", "Age Group: Middle-Aged", 
                    "Age Group: Senior")

alpha <- 0.05
change <- c(rep(" ", 3), confounding_table[, 2])

output_table2 <- data.frame(model_type, variable_class, output_2) %>%
  mutate(odd_ratio = round(exp(log_odd), 2),
         lower_o = round(exp(log_odd - (abs(qnorm(alpha/2))*se)), 2),
         upper_o = round(exp(log_odd + (abs(qnorm(alpha/2))*se)), 2)) %>%
  mutate(`Odd Ratio` = ifelse(pval == -99, "",
                              paste0(odd_ratio, 
                                     " (", lower_o, ",", upper_o, ")")),
         `p-value` = ifelse(pval == -99, "", round(pval,4)),
         `Percent Change` = change) %>%
  select(Model = model_type, Class = variable_class, 
         `Odd Ratio`, `p-value`, `Percent Change`)

## (Output) Table 3: All-variables Model after removing gender.
output_3 <- rbind(summary(no_confound_mod)$coefficients[-1,c(1,2,4)])

rownames(output_3) <- NULL
colnames(output_3) <- c("log_odd", "se", "pval")

variable_class <- c("Diabetes: Yes", "Obesity: Yes", 
                    "Age Group: Middle-Aged", "Age Group: Senior")

alpha <- 0.05

output_table3 <- data.frame(variable_class, output_3) %>%
  mutate(odd_ratio = round(exp(log_odd), 2),
         lower_o = round(exp(log_odd - (abs(qnorm(alpha/2))*se)), 2),
         upper_o = round(exp(log_odd + (abs(qnorm(alpha/2))*se)), 2)) %>%
  mutate(`Odd Ratio` = ifelse(pval == -99, "",
                              paste0(odd_ratio, 
                                     " (", lower_o, ",", upper_o, ")")),
         `p-value` = ifelse(pval == -99, "", round(pval,4))) %>%
  select(Class = variable_class, `Odd Ratio`, `p-value`)

# 79: -------------------------------------------------------------------------
