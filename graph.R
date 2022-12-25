graph1 <- linelist %>%                      # begin with linelist
  count(age_cat, outcome1) %>%     # group and tabulate counts by two columns
  ggplot()+                       # pass new data frame to ggplot
  geom_col(                     # create bar plot
    mapping = aes(   
      x = outcome1,              # map outcome to x-axis
      fill = age_cat,           # map age_cat to the fill
      y = n))                   # map the counts column `n` to the height


graph2 <- linelist %>%                      # begin with linelist
  count(gender, outcome1) %>%     # group and tabulate counts by two columns
  ggplot()+                       # pass new data frame to ggplot
  geom_col(                     # create bar plot
    mapping = aes(   
      x = outcome1,              # map outcome to x-axis
      fill = gender,           # map age_cat to the fill
      y = n))                   # map the counts column `n` to the height




################################################################

survival <- table(linelist$outcome1) %>%
  as_tibble() %>%
  rename(Survived = Var1, Count = n)

survival


survival_ratio <- prop.table(table(linelist$outcome1)) %>%
  as_tibble() %>%
  rename(Survived = Var1, Percentage = n) %>%
  mutate(Percentage = round(Percentage, 2)*100)

survival_ratio



gender <- linelist %>%
  group_by(gender) %>%
  summarise(Count = n())

gender

gender_ratio <- linelist %>%
  group_by(gender, outcome1) %>%
  summarise(Count = n()) %>%
  mutate(Percentage = round(Count/sum(Count)*100))

gender_ratio



graph2 <- linelist %>%
  ggplot() +
  geom_bar(aes(x = gender, fill = outcome1)) +
  geom_text(data = gender, 
            aes(x = gender, y = Count, label = Count), 
            position = position_dodge(width=0.9), 
            vjust=-0.25, 
            fontface = "bold") +
  geom_label(data = gender_ratio, 
             aes(x = gender, y = Count, label = paste0(Percentage, "%"), group = outcome1), 
             position = position_stack(vjust = 0.5)) +
  theme_few() +
  theme(plot.title = element_text(hjust = 0.5, size=18, color = "#054354")) +
  ggtitle("Titanic Gender Survival Rate") +
  scale_x_discrete(name= "Gender") +
  scale_y_continuous(name = "Passenger Count") +
  scale_fill_discrete(name = "Outcome", labels = c("Did Not Survive", "Survived"))
graph2
