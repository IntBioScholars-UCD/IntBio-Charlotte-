# 1. Load the package into this specific R session
library(ggplot2)
library(tidyverse)
library(janitor)# this package is to check if the column names and structure are the same between data sets 
library(lubridate) #this package makes R recognize dates

herbivory = read_csv("../2025 physiologyherbivory data/Data/size/WL2_size_survey_20250609_corrected.csv")%>%
rename(unique_id = Unique.ID)
coredata = read_csv("../2025 physiologyherbivory data/Data/merged_licor_data.csv") %>% 
  rename(survey.date = date) %>%
  mutate(survey.date = ymd(survey.date))
herbivory07232025 = read_csv("../2025 physiologyherbivory data/Data/size/WL2_size_survey_20250718_corrected.csv") %>%
  rename(survey.date = 'survey date')
#because we selected the closest size survey to 7/23/25, we want to update the entry in survey.date to match the coredata date
herbivory07232025$survey.date <- "7/23/25"
herbivory08072025 = read_csv("../2025 physiologyherbivory data/Data/size/WL2_size_survey_20250807_corrected.csv")%>% 
  select(-leaf.number) %>%
  rename(survey.date = 'survey date')
herbivory08252025 = read_csv("../2025 physiologyherbivory data/Data/size/WL2_size_survey_20250828_blocksA-G_corrected.csv")
herbivory09042025 = read_csv("../2025 physiologyherbivory data/Data/size/WL2_size_survey_20250904_blocksH-N_corrected.csv") %>%
  mutate(overhd.diam = ifelse(overhd.diam == "-", NA, overhd.diam)) %>%
  mutate(overhd.diam = as.numeric(overhd.diam))
herbivory09242025 = read_csv("../2025 physiologyherbivory data/Data/size/WL2_size_survey_20250924_blocksA-F_corrected.csv")

compare_df_cols(herbivory07232025, herbivory08072025, herbivory08252025, herbivory09042025, herbivory09242025)

# 2. Merge your data sets into a new object called 'combined_data'
# Glue the tables together by column
all_size = rbind(herbivory07232025, herbivory08072025, herbivory08252025, herbivory09042025, herbivory09242025) %>%
  rename(unique_id = Unique.ID) %>%
  select(unique_id, herbv.scale, survey.date) %>%
  mutate(survey.date = mdy(survey.date))
herb.gsw = left_join(coredata, all_size, by = c("unique_id", "survey.date")) %>%
  mutate(herbv.scale = as.character(herbv.scale))

# 3. Build the plot
# CHANGE 'variable_X' and 'variable_Y' to your specific column names.

#all gsw vs herb (main)
herb.gsw %>% 
  filter(!(gsw>1)) %>%
  filter(!is.na(herbv.scale)) %>%
  ggplot(aes(x = herbv.scale, y = gsw)) + 
  geom_boxplot()

#anova test (reject null because p<0.05) 
herb.gsw.anova = aov(gsw ~ herbv.scale, data = herb.gsw)
summary(herb.gsw.anova)
herb.gsw.tukey = TukeyHSD(herb.gsw.anova)
print(herb.gsw.tukey)

# filter only including parent pop
parent.herb.gsw = herb.gsw %>%
  filter(genotype %in% c("SQ3", "CC", "WL1", "BH", "wv", "YO11", "TM2", "WL2", "DPR", "LV1"))

#parent gsw herbivory
parent.herb.gsw %>% 
  filter(!(gsw>1)) %>%
  filter(!is.na(herbv.scale)) %>%
  ggplot(aes(x = herbv.scale, y = gsw)) + 
  geom_boxplot()

# parent gsw genotypes (use for genotype comparison)
parent.herb.gsw %>% 
  filter(gsw>0) %>%
  group_by(genotype, herbv.scale) %>%
  filter(n()> 1) %>%
  ungroup() %>%
  filter(!genotype %in% c("CC", "DPR", "YO11", "BH")) %>%
  filter(!is.na(herbv.scale)) %>%
  ggplot(aes(x = genotype, y = gsw, fill = herbv.scale)) + 
  geom_boxplot() +
  geom_jitter(width = 0.1)


# herbivory differing by pop
parent.herb.gsw %>% 
  filter(!(gsw>1)) %>%
  filter(!is.na(herbv.scale)) %>%
  ggplot(aes(x = factor(herbv.scale), fill = genotype)) + geom_bar(position = "dodge")


# herb vs gsw
herb.gsw %>%
  filter(!(gsw>1)) %>%
  filter(!is.na(herbv.scale)) %>%
  ggplot(aes(x = herbv.scale, y = gsw)) + 
  geom_boxplot() + 
  facet_wrap(~ survey.date)

# herb vs gsw ANOVA
library(rstatix)
herb.gsw %>%
  filter(!(gsw>1)) %>%
  filter(genotype == "WL2") %>%
  filter(!is.na(herbv.scale)) %>%
  #group_by(survey.date) %>%
  anova_test(gsw ~ herbv.scale)

# herb vs gsw parents ANOVA
library(rstatix)
parent.herb.gsw %>%
  filter(!(gsw>1)) %>%
  filter(!genotype %in% c("CC", "DPR", "YO11")) %>%
  filter(!is.na(herbv.scale)) %>%
  group_by(genotype) %>%
  anova_test(gsw ~ herbv.scale)

# herb vs gsw parents ANOVA by count
parent.herb.gsw %>%
  filter(gsw>0) %>%
  filter(!genotype %in% c("CC", "DPR", "YO11", "BH")) %>%
  filter(!is.na(herbv.scale)) %>%
  group_by(genotype, herbv.scale) %>%
  filter(n()> 1) %>%
  ungroup() %>%
  group_by(genotype) %>%
  anova_test(gsw ~ herbv.scale)

# WL1 F1
herb.gsw %>%
  filter(gsw>0, type.x == "F1") %>%
  filter(str_detect(genotype, "WL1")) %>%
  filter(!is.na(herbv.scale)) %>%
  group_by(genotype, herbv.scale) %>%
  filter(n()> 1) %>%
  ungroup() %>%
  group_by(genotype) %>%
  anova_test(gsw ~ herbv.scale)
  #select(genotype, gsw, herbv.scale)

# WL1 F2


# WL2 F1

# WL2 F2
  
