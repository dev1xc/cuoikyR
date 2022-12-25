# Development version of epicontacts (for transmission chains with a time x-axis)
pacman::p_install_gh("reconhub/epicontacts@timeline")

# The package for this handbook, which includes all the example data  
pacman::p_install_gh("appliedepi/epirhandbook")


install.packages("flextable")

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


sum(linelist$outcome == 0) 
linelist$outcome1 <- linelist$outcome == 0

linelist$outcome1[linelist$outcome1 == TRUE] <- "Recover"
linelist$outcome1[linelist$outcome1 == FALSE] <- "Death"
#Duyệt dữ liệu
skim(linelist)

#Thống kê
summary(linelist)
#################3
linelist %>% 
  get_summary_stats(
    age, wt_kg, ht_cm, ct_blood, temp,  # columns to calculate for
    type = "common")   

linelist %>% tabyl(age_cat)

###################################### Phần trăm#################
linelist %>%               # case linelist
  tabyl(age_cat) %>%       # tabulate counts and proportions by age category
  adorn_pct_formatting()   # convert proportions to percents
#################################################################
#################################################################

linelist %>%                                  
  tabyl(age_cat, gender) %>%                  # counts by age and gender
  adorn_totals(where = "row") %>%             # add total row
  adorn_percentages(denominator = "row") %>%  # convert counts to proportions
  adorn_pct_formatting(digits = 1)            # convert proportions to percents
###################################################################

linelist %>%                                  # case linelist
  tabyl(age_cat, gender) %>%                  # cross-tabulate counts
  adorn_totals(where = "row") %>%             # add a total row
  adorn_percentages(denominator = "col") %>%  # convert to proportions
  adorn_pct_formatting() %>%                  # convert to percents
  adorn_ns(position = "front") %>%            # display as: "count (percent)"
  adorn_title(                                # adjust titles
    row_name = "Age Category",
    col_name = "Gender")

########################################################################

table1 <- linelist %>%
  tabyl(age_cat, gender) %>% 
  adorn_totals(where = "col") %>% 
  adorn_percentages(denominator = "col") %>% 
  adorn_pct_formatting() %>% 
  adorn_ns(position = "front") %>% 
  adorn_title(
    row_name = "Age Category",
    col_name = "Gender",
    placement = "combined") 
table1

table2 <-linelist %>%                  # begin with linelist
  group_by(outcome1) %>%                         # group by outcome 
  count(age_cat) %>%                            # group and count by age_cat, and then remove age_cat grouping
  mutate(percent = scales::percent(n / sum(n))) # calculate percent - note the denominator is by outcome group
table2


age_by_outcome <- linelist %>%                  # begin with linelist
  group_by(outcome) %>%                         # group by outcome 
  count(age_cat) %>%                            # group and count by age_cat, and then remove age_cat grouping
  mutate(percent = scales::percent(n / sum(n))) # calculate percent - note the denominator is by outcome group
table4<-age_by_outcome
table4


totals <- linelist %>% 
  filter(!is.na(outcome) & hospital != "Missing") %>%
  group_by(outcome) %>%                            # Grouped only by outcome, not by hospital    
  summarise(
    N = n(),                                       # These statistics are now by outcome only     
    ct_value = median(ct_blood, na.rm=T))

totals # print table


by_hospital <- linelist %>% 
  filter(!is.na(outcome) & hospital != "Missing") %>%  # Remove cases with missing outcome or hospital
  group_by(hospital, outcome) %>%                      # Group data
  summarise(                                           # Create new summary columns of indicators of interest
    N = n(),                                            # Number of rows per hospital-outcome group     
    ct_value = median(ct_blood, na.rm=T))               # median CT value per group

by_hospital # print table


table_long <- bind_rows(by_hospital, totals) %>% 
  mutate(hospital = replace_na(hospital, "Total"))
table_long



########################################################################

#######################################################################
linelist %>%                      # begin with linelist
  count(age_cat, outcome1) %>%     # group and tabulate counts by two columns
  ggplot()+                       # pass new data frame to ggplot
  geom_col(                     # create bar plot
    mapping = aes(   
      x = outcome1,              # map outcome to x-axis
      fill = age_cat,           # map age_cat to the fill
      y = n))                   # map the counts column `n` to the height
######################################################################
  


#-------------------------------- Tuyến tính---------------------------------------#
#####################################################################################
## define variables of interest 
explanatory_vars <- c("gender", "fever", "chills", "cough", "aches", "vomit")

## convert dichotomous variables to 0/1 
linelist <- linelist %>%  
  mutate(across(                                      
    .cols = all_of(c(explanatory_vars, "outcome")),  ## for each column listed and "outcome"
    .fns = ~case_when(                              
      . %in% c("m", "yes", "Death")   ~ 1,           ## recode male, yes and death to 1
      . %in% c("f", "no",  "Recover") ~ 0,           ## female, no and recover to 0
      TRUE                            ~ NA_real_)    ## otherwise set to missing
  )
  )

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

