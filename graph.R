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



disease_ratio <- linelist %>%
  group_by(fever, chills, cough, aches, vomit) %>%
  summarise(Count = n()) %>%
  mutate(Percentage = round(Count/sum(Count)*100))
disease_ratio

disease_outcome_ratio <- linelist %>%
  group_by(outcome,fever, chills, cough, aches, vomit) %>%
  summarise(Count = n()) %>%
  mutate(Percentage = round(Count/sum(Count)*100))
disease_outcome_ratio

disease_outcome_gender_ratio <- linelist %>%
  group_by(outcome,gender,fever, chills, cough, aches, vomit) %>%
  summarise(Count = n()) %>%
  mutate(Percentage = round(Count/sum(Count)*100))
disease_outcome_gender_ratio



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
  ggtitle("Bi???u ????? t??? l??? tu???i v?? xu???t vi???n") +
  scale_x_discrete(name= "Tu???i") +
  scale_y_continuous(name = "T???ng b???nh nh??n") +
  scale_fill_discrete(name = "Outcome", labels = c("H???i ph???c", "Ch???t"))
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
  ggtitle("Bi???u ????? t??? l??? gi???i t??nh v?? xu???t vi???n") +
  scale_x_discrete(name= "Gi???i t??nh") +
  scale_y_continuous(name = "T???ng b???nh nh??n") +
  scale_fill_discrete(name = "Outcome", labels = c("H???i ph???c", "Ch???t"))
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
  ggtitle("Bi???u ????? t??? l??? gi???a b???nh vi???n v?? xu???t vi???n") +
  scale_x_discrete(name= "B???nh vi???n") +
  scale_y_continuous(name = "T???ng b???nh nh??n") +
  scale_fill_discrete(name = "Outcome", labels = c("H???i ph???c", "Ch???t"))
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
  ggtitle("T??? l??? gi???a b???nh vi???n - gi???i t??nh - xu???t vi???n") +
  scale_x_discrete(name= "T??? l??? gi???i t??nh b???nh vi???n ") +
  scale_y_continuous(name = "Passenger Count", limits = c(0,360)) +
  scale_fill_discrete(name = "Xu???t vi???n", labels = c("Ch???t", "S???ng"))
graph4

graph5 <- ggplot(data=hospital_gender_ratio , aes(x=hospital, y=Count, group=gender,colour = gender )) +
  geom_line(aes(linetype=gender,lwd = "10px"))+
  theme(plot.title = element_text(hjust = 0.5, size=18, color = "#054354")) +
  ggtitle("T??? l??? gi???a b???nh vi???n - gi???i t??nh")+
  geom_point()
graph5  


graph6 <- ggplot(data=age_cat_ratio, aes(x=age_cat, y=Count, group=outcome,colour = outcome )) +
  geom_line(aes(linetype=outcome,lwd = "10px"))+
  theme(plot.title = element_text(hjust = 0.5, size=18, color = "#054354")) +
  ggtitle("T??? l??? gi???a xu???t vi???n - tu???i")+
  geom_point()
graph6

