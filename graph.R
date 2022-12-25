library(readr) # Reading in data
library(dplyr) # Data manipulation
library(tibble) # Data manipulation
library(ggplot2) # Data visualization
library(ggthemes) # Data visualization
library(RColorBrewer) # Data visualization


graph1 <- linelist %>%                      # begin with linelist
  count(age_cat, outcome) %>%     # group and tabulate counts by two columns
  ggplot()+                       # pass new data frame to ggplot
  geom_col(                     # create bar plot
    mapping = aes(   
      x = outcome,              # map outcome to x-axis
      fill = age_cat,           # map age_cat to the fill
      y = n))                   # map the counts column `n` to the height


graph2 <- linelist %>%                      # begin with linelist
  count(gender, outcome) %>%     # group and tabulate counts by two columns
  ggplot()+                       # pass new data frame to ggplot
  geom_col(                     # create bar plot
    mapping = aes(   
      x = outcome,              # map outcome to x-axis
      fill = gender,           # map age_cat to the fill
      y = n))                   # map the counts column `n` to the height




################################################################
table(linelist$outcome)

survival_ratio <- linelist %>%               # case linelist
  tabyl(outcome) %>%       # tabulate counts and proportions by age category
  adorn_pct_formatting() 

survival_ratio 


gender <- linelist %>%
  group_by(gender) %>%
  summarise(Count = n())

gender

age_cat <- linelist %>%
  group_by(age_cat) %>%
  summarise(Count = n())

age_cat

age_cat_ratio <- linelist %>%
  group_by(age_cat, outcome) %>%
  summarise(Count = n()) %>%
  mutate(Percentage = round(Count/sum(Count)*100))

age_cat_ratio

gender_ratio <- linelist %>%
  group_by(gender, outcome) %>%
  summarise(Count = n()) %>%
  mutate(Percentage = round(Count/sum(Count)*100))

gender_ratio

hospital <- linelist %>%
  group_by(hospital) %>%
  summarise(Count = n())

hospital


hospital_ratio <- linelist %>%
  group_by(hospital, outcome) %>%
  summarise(Count = n()) %>%
  mutate(Percentage = round(Count/sum(Count)*100))

hospital_ratio

hospital_gender_ratio <- linelist %>%
  group_by(hospital, gender) %>%
  summarise(Count = n()) %>%
  mutate(Percentage = round(Count/sum(Count)*100))
hospital_gender_ratio

hospital_gender_outcome_ratio <- linelist %>%
  group_by(hospital, gender,outcome) %>%
  summarise(Count = n()) %>%
  mutate(Percentage = round(Count/sum(Count)*100))
hospital_gender_outcome_ratio


graph1 <- linelist %>%
  ggplot() +
  geom_bar(aes(x = age_cat, fill = outcome)) +
  geom_text(data = age_cat, 
            aes(x = age_cat, y = Count, label = Count), 
            position = position_dodge(width=0.9), 
            vjust=-0.25, 
            fontface = "bold") +
  geom_label(data = age_cat_ratio, 
             aes(x = age_cat, y = Count, label = paste0(Percentage, "%"), group = outcome), 
             position = position_stack(vjust = 0.5)) +
  theme_few() +
  theme(plot.title = element_text(hjust = 0.5, size=18, color = "#054354")) +
  ggtitle("Biểu đồ tỉ lệ tuổi và xuất viện") +
  scale_x_discrete(name= "Tuổi") +
  scale_y_continuous(name = "Tổng bệnh nhân") +
  scale_fill_discrete(name = "Outcome", labels = c("Hồi phục", "Chết"))
graph1




graph2 <- linelist %>%
  ggplot() +
  geom_bar(aes(x = gender, fill = outcome)) +
  geom_text(data = gender, 
            aes(x = gender, y = Count, label = Count), 
            position = position_dodge(width=0.9), 
            vjust=-0.25, 
            fontface = "bold") +
  geom_label(data = gender_ratio, 
             aes(x = gender, y = Count, label = paste0(Percentage, "%"), group = outcome), 
             position = position_stack(vjust = 0.5)) +
  theme_few() +
  theme(plot.title = element_text(hjust = 0.5, size=18, color = "#054354")) +
  ggtitle("Biểu đồ tỉ lệ giới tính và xuất viện") +
  scale_x_discrete(name= "Giới tính") +
  scale_y_continuous(name = "Tổng bệnh nhân") +
  scale_fill_discrete(name = "Outcome", labels = c("Hồi phục", "Chết"))
graph2


graph3 <- linelist %>%
  ggplot() +
  geom_bar(aes(x = hospital, fill = outcome)) +
  geom_text(data = hospital, 
            aes(x = hospital, y = Count, label = Count), 
            position = position_dodge(width=0.9), 
            vjust=-0.25, 
            fontface = "bold") +
  geom_label(data = hospital_ratio, 
             aes(x = hospital, y = Count, label = paste0(Percentage, "%"), group = outcome), 
             position = position_stack(vjust = 0.5)) +
  theme_few() +
  theme(plot.title = element_text(hjust = 0.5, size=18, color = "#054354")) +
  ggtitle("Biểu đồ tỉ lệ giữ bệnh viện và xuất viện") +
  scale_x_discrete(name= "Bệnh viện") +
  scale_y_continuous(name = "Tổng bệnh nhân") +
  scale_fill_discrete(name = "Outcome", labels = c("Hồi phục", "Chết"))
graph3


graph4 <- linelist %>%
  ggplot() +
  geom_bar(aes(x = gender, fill = outcome)) +
  facet_wrap(~ hospital) +
  geom_text(data = hospital_gender_ratio, 
            aes(x = gender, y = Count, label = Count), 
            position = position_dodge(width=0.9), 
            vjust= -1.5, 
            fontface = "bold") +
  geom_label(data = hospital_gender_outcome_ratio, 
             aes(x = gender, y = Count, label = paste0(Percentage, "%"), group = outcome), 
             position = position_stack(vjust = 0.5)) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, size=18, color = "#054354")) +
  ggtitle("Tỉ lệ giữa bệnh viện - giới tính - xuất viện") +
  scale_x_discrete(name= "Tỉ lệ giới tính bệnh viện ") +
  scale_y_continuous(name = "Passenger Count", limits = c(0,360)) +
  scale_fill_discrete(name = "Xuất viện", labels = c("Chết", "Sống"))
graph4

