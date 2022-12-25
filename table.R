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
################################################################