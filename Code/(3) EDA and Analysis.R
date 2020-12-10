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
           Variable = v_name) %>%
    select(Variable, Class,
           "By Class (%Class)" = by_var,
           "Hypertension: Yes" = HY, 
           "Hypertension: No" = HN) %>%
    return()
}

## Function for EDA (Hypertension x Diabetes x Group)
eda_cross <- function(vari){
  eda_tab <- data %>%
    mutate(Obesity = ifelse(Obesity == "Yes",
                            "Obesity: Yes", "Obesity: No")) %>%
    mutate(Gender = ifelse(Gender == "Male", 
                           "Gender: Male", "Gender: Female")) %>%
    group_by(Hypertension, Diabetes, {{vari}}) %>%
    summarise(n = n()) %>%
    group_by(Hypertension) %>%
    mutate(Percent = 100 * n/sum(n))
  
  eda_tab$Diabetes <- factor(eda_tab$Diabetes)
  levels(eda_tab$Diabetes) <- c("Diabetes: No", "Diabetes: Yes")
  eda_tab$Diabetes <- relevel(eda_tab$Diabetes, 2)
  
  t1 <- "Hypertension status classified by Diabetes for each "
  
  if(colnames(eda_tab)[3] == "Age_Group"){
    title_plot <- paste0("Age group")
  } else {
    title_plot <- paste0(colnames(eda_tab)[3])
  }
  
  eda_plot <- ggplot(eda_tab, 
                     aes_string(x = names(eda_tab)[1], 
                                y = names(eda_tab)[5])) +
    geom_bar(stat = "identity", width = 0.3, fill = "goldenrod2") +
    theme_bw() +
    geom_text(aes(label = paste0(round(Percent,2), "%")), 
              vjust = 0.05, color = "black", size = 3) +
    scale_y_continuous(name = "Percent", limits = c(0, 100)) +
    scale_x_discrete(name = "Hypertension", limits=c("Yes", "No")) +
    facet_grid(get(colnames(eda_tab)[3]) ~ Diabetes) +
    labs(title = title_plot)
}

# Exploratory Data Analysis ---------------------------------------------------

## Hypertension
print(eda_1var(Hypertension, "aquamarine2"))

ggsave(paste0(path, "Plots/Hypertension.png"), 
       eda_1var(Hypertension, "aquamarine2", 4.5))

## Summary Table
bal_tab_all <- rbind(suppressWarnings(balance_table(Diabetes)),
                     suppressWarnings(balance_table(Obesity)),
                     suppressWarnings(balance_table(Gender)),
                     suppressWarnings(balance_table(Age_Group)))

## Groups
ggsave(paste0(path, "Plots/Group.png"),
       grid.arrange(eda_1var(Diabetes), 
                    eda_1var(Obesity), 
                    eda_1var(Age_Group),
                    nrow = 2))

# EDA (Hypertension x Diabetes): ----------------------------------------------
hd_table <- data %>% 
  group_by(Hypertension, Diabetes) %>% 
  summarise(n = n()) %>%
  group_by(Diabetes) %>%
  mutate(Percent = 100 * n/sum(n))

hd_table$n <- unlist(lapply(hd_table$n, comma_th))

hd_table$Diabetes <- factor(hd_table$Diabetes)
hd_table$Diabetes <- relevel(hd_table$Diabetes, 2)

hypertension_diabetes <- ggplot(hd_table, aes(x = Hypertension, y = Percent)) +
  geom_bar(stat = "identity", width = 0.4, fill = "salmon") +
  facet_grid(. ~ Diabetes, labeller = label_both) +
  theme_bw() +
  geom_text(aes(label = paste0("(", n, ")")), 
            vjust = 1.5, color = "black", size = 3.5) +
  geom_text(aes(label = paste0(round(Percent,2), "%")), 
            vjust = -0.25, color = "black", size = 3.5) +
  scale_y_continuous(name = "Percent", limits = c(0, 100)) +
  scale_x_discrete(limits=c("Yes", "No")) +
  labs(title = "Hypertension status classified by Diabetes")

hypertension_diabetes

ggsave(paste0(path, "Plots/hypertension_diabetes.png"), hypertension_diabetes)

# EDA (Hypertension x Diabetes x Group): --------------------------------------

data

c_p <- grid.arrange(eda_cross(Gender), 
                    eda_cross(Age_Group),
                    eda_cross(Obesity),
                    nrow = 2,
                    top = "Hypertension classified by diabetes and other aspects.")

ggsave(paste0(path, "Plots/cross_all.png"), c_p, width = 15, height = 10)

# Convert all variables into a factor variables -------------------------------
data$Hypertension <- factor(data$Hypertension)
data$Diabetes <- factor(data$Diabetes)
data$Gender <- factor(data$Gender)
data$Obesity <- factor(data$Obesity)
data$Age_Group <- factor(data$Age_Group)

# Analysis: -------------------------------------------------------------------
overall_mod <- glm(Hypertension ~ Diabetes, data, family = binomial())
all_mod <- glm(Hypertension ~ Diabetes + Gender + Obesity + Age_Group, 
               data, family = binomial())

# Create the final result table: ----------------------------------------------
result_table <- rbind(summary(overall_mod)$coefficients[-1,c(1,2,4)],
                      rep(-99,3), ## Additional Line for sepereating tow models
                      summary(all_mod)$coefficients[-1,c(1,2,4)])
rownames(result_table) <- NULL
colnames(result_table) <- c("log_odd", "se", "pval")

model_type <- c("Overall Model", "", "All variables Model", rep("",4))
variable_class <- c("Diabetes: Yes", "", "Diabetes: Yes", "Gender: Male",
                    "Obesity: Yes", "Age Group: Middle-Aged", 
                    "Age Group: Senior")

alpha <- 0.05

output_table <- data.frame(model_type, variable_class, result_table) %>%
  mutate(odd_ratio = round(exp(log_odd), 2),
         lower_o = round(exp(log_odd - (abs(qnorm(alpha/2))*se)), 2),
         upper_o = round(exp(log_odd + (abs(qnorm(alpha/2))*se)), 2),
         ) %>%
  mutate(`Odd Ratio` = ifelse(pval == -99, "",
                              paste0(odd_ratio, 
                                     " (", lower_o, ",", upper_o, ")")),
         `p-value` = ifelse(pval == -99, "", round(pval,4))) %>%
  select(Model = model_type, Class = variable_class, 
         `Odd Ratio`, `p-value`)

# 79: -------------------------------------------------------------------------
