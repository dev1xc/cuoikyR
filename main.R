# # Development version of epicontacts (for transmission chains with a time x-axis)
# pacman::p_install_gh("reconhub/epicontacts@timeline")
# 
# # The package for this handbook, which includes all the example data  
# pacman::p_install_gh("appliedepi/epirhandbook")
# 
# 
# install.packages("devtools")



library(devtools)
# devtools::install_github("rmcelreath/rethinking")

library(shinydashboard)
library(shiny)


library(rio)
library(here)
library(tidyverse)
library(stringr)
library(purrr)
library(gtsummary)
library(broom)
library(lmtest)
library(parameters)
library(see)
library(readxl)
library(augmentedRCBD)
library(AugmenterR)
library(ggplot2)
library(DT)
library(tidyr)
library(readr) # Reading in data
library(dplyr) # Data manipulation
library(tibble) # Data manipulation
library(ggplot2) # Data visualization
library(ggthemes) # Data visualization
library(RColorBrewer) # Data visualization
library(rsconnect)



pacman::p_load(
  rio,          # File import
  here,         # File locator
  skimr,        # get overview of data
  tidyverse,    # data management + ggplot2 graphics 
  gtsummary,    # summary statistics and tests
  rstatix,      # summary statistics and statistical tests
  janitor,      # adding totals and percents to tables
  scales,       # easily convert proportions to percents  
  flextable     # converting tables to pretty images
)

# import the linelist
linelist <- read_rds("./linelist_cleaned.rds")
View(linelist)
n_total <- nrow(linelist)
linelist <- na.omit(linelist)
n_total <- nrow(linelist)
min_date <- min(linelist$date_hospitalisation)
max_date <- max(linelist$date_hospitalisation)



# sum(linelist$outcome == 0) 
# linelist$outcome1 <- linelist$outcome == 0

# linelist$outcome1[linelist$outcome1 == TRUE] <- "Recover"
# linelist$outcome1[linelist$outcome1 == FALSE] <- "Death"
#Duyệt dữ liệu
skim(linelist)

#Thống kê
summary(linelist)
#################3

  


#-------------------------------- Tuyến tính---------------------------------------#
#####################################################################################
## define variables of interest 
explanatory_vars <- c("gender", "fever", "chills", "cough", "aches", "vomit")

## convert dichotomous variables to 0/1 
# linelist <- linelist %>%  
  # mutate(across(                                      
  #   .cols = all_of(c(explanatory_vars, "outcome")),  ## for each column listed and "outcome"
  #   .fns = ~case_when(                              
  #     . %in% c("m", "yes", "Death")   ~ 1,           ## recode male, yes and death to 1
  #     . %in% c("f", "no",  "Recover") ~ 0,           ## female, no and recover to 0
  #     TRUE                            ~ NA_real_)    ## otherwise set to missing
  # )
  # )

## add in age_category to the explanatory vars 
explanatory_vars <- c(explanatory_vars, "age_cat")

## drop rows with missing information for variables of interest 
linelist <- linelist %>% 
  drop_na(any_of(c("outcome", explanatory_vars)))

#### hồi quy tuyến tính
lm_results <- lm(ht_cm ~ age, data = linelist)
summary(lm_results)

tidy(lm_results)

## pull the regression points and observed data in to one dataset
points <- augment(lm_results)

## plot the data using age as the x-axis 
tuyen_tinh_don <- ggplot(points, aes(x = age)) + 
  ## add points for height 
  geom_point(aes(y = ht_cm)) + 
  ## add your regression line 
  geom_line(aes(y = .fitted), colour = "red")
tuyen_tinh_don
#####################################################################################
#####################################################################################
