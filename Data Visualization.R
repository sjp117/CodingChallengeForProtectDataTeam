library(dplyr)
library(ggplot2)

# load data
load("Data and Samples/Question 1 - Data/Data.RData")

df <- recruitment_data

# number of subject by source ----
dfSource <- recruitment_data %>%
    count(RecruitSource)

subjBySourceBar <- dfSource %>%
    ggplot(aes(x = reorder(RecruitSource, -n), y = n)) +
    geom_bar(stat = "identity", fill="steelblue") +
    geom_text(aes(label=n),vjust = -0.3) +
    labs(title = "Number of subjects from recruitment source",
         x = "Recruitment Source", y = "Frequency") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 10)) +
    theme(text = element_text(size = 12))

ggsave(filename = "output/subjBySourceBar.png", subjBySourceBar, bg = "white")

# Number of subjects by recruitment source and group ----
dfSourceGroup <- recruitment_data %>%
    count(RecruitSource,Group)

subjBySourceGroup <- dfSourceGroup %>%
    ggplot(aes(x = reorder(RecruitSource, -n, sum), y = n, fill = Group)) +
    geom_bar(stat = "identity") +
    geom_text(aes(label=n),position = position_stack(vjust = 0.5)) +
    labs(title = "Number of subjects by recruitment source and group",
         x = "Recruitment Source", y = "Frequency") +
    scale_fill_brewer(palette="Set2") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 10)) +
    theme(text = element_text(size = 12))
    
ggsave(filename = "output/subjBySourceGroupBar.png", subjBySourceGroup, bg = "white")

# Number of subjects by recruitment source and age ----
dfSourceAge <- recruitment_data %>%
    count(RecruitSource, Age)

# check range of age
summary(dfSourceAge$Age)

# group subjects into age groups
dfSourceAge <- dfSourceAge %>%
    mutate("Age Group" = case_when(Age >= 30 & Age < 40 ~ "30-39",
                                   Age >= 40 & Age < 50 ~ "40-49",
                                   Age >= 50 & Age < 60 ~ "50-59",
                                   Age >= 60 & Age < 70 ~ "60-69",
                                   Age >= 70 & Age < 80 ~ "70-79")) %>%
    group_by(RecruitSource, `Age Group`) %>%
    summarise(n = sum(n))

subjBySourceAge <- dfSourceAge %>%
    ggplot(aes(x = reorder(RecruitSource, -n, sum), y = n, fill = `Age Group`)) +
    geom_bar(stat = "identity") +
    geom_text(aes(label=n),position = position_stack(vjust = 0.5)) +
    labs(title = "Number of subjects by recruitment source and age group",
         x = "Recruitment Source", y = "Frequency") +
    scale_fill_brewer(palette="Set2") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 10)) +
    theme(text = element_text(size = 12))

ggsave(filename = "output/subjBySourceAgeBar.png", subjBySourceAge, bg = "white")

# Number of subjects by recruitment source and gender ----
dfSourceGender <- recruitment_data %>%
    count(RecruitSource, Gender)

subjBySourceGender <- dfSourceGender %>%
    ggplot(aes(x = reorder(RecruitSource, -n, sum), y = n, fill = Gender)) +
    geom_bar(stat = "identity") +
    geom_text(aes(label=n),position = position_stack(vjust = 0.5)) +
    labs(title = "Number of subjects by recruitment source and gender",
         x = "Recruitment Source", y = "Frequency") +
    scale_fill_brewer(palette="Set2") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 10)) +
    theme(text = element_text(size = 12))

ggsave(filename = "output/subjBySourceGenderBar.png", subjBySourceGender, bg = "white")
