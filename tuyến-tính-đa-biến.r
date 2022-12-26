########### Thư viện tuyến tính đa Biến  ###########
library(see)
library(rio)
library(here)
library(stringr)
library(purrr)
library(gtsummary)
library(broom)
library(lmtest)
library(parameters)
library(stats)
library(janitor)
library(flextable)













library(shiny)
library(ggplot2)
library(tools)
library(dplyr)

library(tidyverse)

library(magrittr)
library(ggplot2)
library(shinydashboard)
library(dplyr)    # data.frames
library(sf)       # Spatial

# Interactive Data Viz
library(leaflet)  # Maps
library(dygraphs) # Charts
library(DT)       # tables
library(rvest)    # webscraping


library(dqshiny)    # auto complete
library(shiny)       # Starting Reactivity
library(shinythemes) # themes
library(plotly)
library(DT)


linelist1 <- linelist
## define variables of interest 
explanatory_vars <- c("gender", "fever", "chills", "cough", "aches", "vomit")


## convert dichotomous variables to 0/1 
linelist1 <- linelist1 %>%  
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
linelist1 <- linelist1 %>% 
  drop_na(any_of(c("outcome", explanatory_vars)))








######### hồi quy tuyến tính logistic đơn biến
# arguments for glm()
model <- glm(outcome ~ age_cat, family = "binomial", data = linelist1)
summary(model)

linelist1 %>% 
  mutate(age_cat = fct_relevel(age_cat, "20-29", after = 0)) %>% 
  glm(formula = outcome ~ age_cat, family = "binomial") %>% 
  summary()

model <- glm(outcome ~ age_cat, family = "binomial", data = linelist1) %>% 
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%        # exponentiate and produce CIs
  mutate(across(where(is.numeric), round, digits = 2))  # round all numeric columns

counts_table <- linelist1 %>% 
  janitor::tabyl(age_cat, outcome)


combined <- counts_table %>%           # begin with table of counts
  bind_cols(., model) %>%              # combine with the outputs of the regression 
  select(term, 2:3, estimate,          # select and re-order cols
         conf.low, conf.high, p.value) %>% 
  mutate(across(where(is.numeric), round, digits = 2)) ## round to 2 decimal places




explanatory_vars %>% str_c("outcome ~ ", .)




models <- explanatory_vars %>%       # begin with variables of interest
  str_c("outcome ~ ", .) %>%         # combine each variable into formula ("outcome ~ variable of interest")
  
  # iterate through each univariate formula
  map(                               
    .f = ~glm(                       # pass the formulas one-by-one to glm()
      formula = as.formula(.x),      # within glm(), the string formula is .x
      family = "binomial",           # specify type of glm (logistic)
      data = linelist1)) %>%          # dataset
  
  # tidy up each of the glm regression outputs from above
  map(
    .f = ~tidy(
      .x, 
      exponentiate = TRUE,           # exponentiate 
      conf.int = TRUE)) %>%          # return confidence intervals
  
  # collapse the list of regression outputs in to one data frame
  bind_rows() %>% 
  
  # round all numeric columns
  mutate(across(where(is.numeric), round, digits = 2))





## for each explanatory variable
univ_tab_base <- explanatory_vars %>% 
  map(.f = 
        ~{linelist1 %>%                ## begin with linelist1
            group_by(outcome) %>%     ## group data set by outcome
            count(.data[[.x]]) %>%    ## produce counts for variable of interest
            pivot_wider(              ## spread to wide format (as in cross-tabulation)
              names_from = outcome,
              values_from = n) %>% 
            drop_na(.data[[.x]]) %>%         ## drop rows with missings
            rename("variable" = .x) %>%      ## change variable of interest column to "variable"
            mutate(variable = as.character(variable))} ## convert to character, else non-dichotomous (categorical) variables come out as factor and cant be merged
  ) %>% 
  
  ## collapse the list of count outputs in to one data frame
  bind_rows() %>% 
  
  ## merge with the outputs of the regression 
  bind_cols(., models) %>% 
  
  ## only keep columns interested in 
  select(term, 2:3, estimate, conf.low, conf.high, p.value) %>% 
  
  ## round decimal places
  mutate(across(where(is.numeric), round, digits = 2))


univ_tab <- linelist1 %>% 
  dplyr::select(explanatory_vars, outcome) %>% ## select variables of interest
  
  tbl_uvregression(                         ## produce univariate table
    method = glm,                           ## define regression want to run (generalised linear model)
    y = outcome,                            ## define outcome variable
    method.args = list(family = binomial),  ## define what type of glm want to run (logistic)
    exponentiate = TRUE                     ## exponentiate to produce odds ratios (rather than log odds)
  )

## view univariate results table 
univ_tab






################PHÂN TÍCH ĐA BIẾN ##########



mv_reg <- glm(outcome ~ gender + fever + chills + cough + aches + vomit + age_cat, family = "binomial", data = linelist1)

summary(mv_reg)



glm(outcome ~ gender + age_cat * fever, family = "binomial", data = linelist1)

## run a regression with all variables of interest 
mv_reg <- explanatory_vars %>%  ## begin with vector of explanatory column names
  str_c(collapse = "+") %>%     ## combine all names of the variables of interest separated by a plus
  str_c("outcome ~ ", .) %>%    ## combine the names of variables of interest with outcome in formula style
  glm(family = "binomial",      ## define type of glm as logistic,
      data = linelist1)          ## define your dataset



  model1 <- glm(outcome ~ age_cat, family = "binomial", data = linelist1)
  model2 <- glm(outcome ~ age_cat + gender, family = "binomial", data = linelist1)
  
  lmtest::lrtest(model1, model2)
  
  
  
  
  ## choose a model using forward selection based on AIC
  ## you can also do "backward" or "both" by adjusting the direction
  final_mv_reg <- mv_reg %>%
    step(direction = "forward", trace = FALSE)
  
  
  
  options(scipen=999)

  
  mv_tab_base <- final_mv_reg %>% 
    broom::tidy(exponentiate = TRUE, conf.int = TRUE) %>%  ## get a tidy dataframe of estimates 
    mutate(across(where(is.numeric), round, digits = 2))          ## round   
  
  
  ## show results table of final regression 
  mv_tab <- tbl_regression(final_mv_reg, exponentiate = TRUE)
  mv_tab
  
  
  
  ## combine with univariate results 
  tbl_merge(
    tbls = list(univ_tab, mv_tab),                          # combine
    tab_spanner = c("**Univariate**", "**Multivariable**")) # set header names
  
  
  ## combine univariate and multivariable tables 
  left_join(univ_tab_base, mv_tab_base, by = "term") %>% 
    ## choose columns and rename them
    select( # new name =  old name
      "characteristic" = term, 
      "recovered"      = "0", 
      "dead"           = "1", 
      "univ_or"        = estimate.x, 
      "univ_ci_low"    = conf.low.x, 
      "univ_ci_high"   = conf.high.x,
      "univ_pval"      = p.value.x, 
      "mv_or"          = estimate.y, 
      "mvv_ci_low"     = conf.low.y, 
      "mv_ci_high"     = conf.high.y,
      "mv_pval"        = p.value.y 
    ) %>% 
    mutate(across(where(is.double), round, 2))   
  
  
  
  ## remove the intercept term from your multivariable results
linear_da_bien  <-  mv_tab_base %>% 
    
    #set order of levels to appear along y-axis
    mutate(term = fct_relevel(
      term,
      "vomit", "gender", "fever", "cough", "chills", "aches",
      "age_cat5-9", "age_cat10-14", "age_cat15-19", "age_cat20-29",
      "age_cat30-49", "age_cat50-69", "age_cat70+")) %>%
    
    # remove "intercept" row from plot
    filter(term != "(Intercept)") %>% 
    
    ## plot with variable on the y axis and estimate (OR) on the x axis
    ggplot(aes(x = estimate, y = term)) +
    
    ## show the estimate as a point
    geom_point() + 
    
    ## add in an error bar for the confidence intervals
    geom_errorbar(aes(xmin = conf.low, xmax = conf.high)) + 
    
    ## show where OR = 1 is for reference as a dashed line
    geom_vline(xintercept = 1, linetype = "dashed")
linear_da_bien
  
