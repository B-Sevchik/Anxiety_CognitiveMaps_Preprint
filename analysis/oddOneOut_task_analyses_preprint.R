##This script manipulates the data frame for odd one out task analyses, then also runs the ANOVA to determine statistical significance and creates plots for the odd one out task.
#data paths to insert: lines 4, 16, 75

setwd("[INSERT PATH TO GITHUB DIRECTORY IN COMPUTER FILE SYSTEM]")
library(plotrix)
library(ggplot2)
library(gridExtra)
library(dplyr)
library(readr)
library(ggplot2)
library(tidyr)
library(tidyverse)
library(psycho)

#path reference
data_path <- '[INSERT PATH TO original_data FILE FROM GITHUB]'

#load data
df <- read_csv(data_path)
ooo_df <- df %>%
  filter(taskName == 'oddOneOutTest') %>% 
  select(subject, trialCount, RT, acc, partResp, chosenNode:chosenThreatStatus,
         option1CommunityNumber, option1ThreatStatus,
         option2CommunityNumber, option2ThreatStatus,
         option3CommunityNumber, option3ThreatStatus)
ooo_df

# func for finding which community number is unique
get_singleton_comm_num <- function(x){
  tab <- table(strsplit(x, '')[[1]])
  least_com_num <- names(tab)[tab == min(tab)]
  gregexpr(pattern = least_com_num, x)[[1]][1]
}

# calculate which community number is different from the rest
ooo_df <- ooo_df %>% 
  mutate(combined_comnums = paste0(option1CommunityNumber, option2CommunityNumber, option3CommunityNumber),
         target_node = pmap(list(combined_comnums), get_singleton_comm_num),
         target_threat = case_when(
           target_node == 1 ~ option1ThreatStatus,
           target_node == 2 ~ option2ThreatStatus,
           target_node == 3 ~ option3ThreatStatus
         ),
         nontarget_comb = case_when(
           target_node == 1 ~ paste0(option2ThreatStatus, option3ThreatStatus),
           target_node == 2 ~ paste0(option1ThreatStatus, option3ThreatStatus),
           target_node == 3 ~ paste0(option1ThreatStatus, option2ThreatStatus)
         ),
         nontarget_threat = ifelse(grepl("threat", nontarget_comb), "has_threat", "no_threat")) 

#excluded subjects from preprocessing
excluded_subjects <- read_csv('data/excludedSubjects.csv')
ooo_df
ooo_df <- ooo_df %>%
  filter(!subject %in% excluded_subjects$subject)
ooo_df
write_csv(ooo_df, 'data/oddOneOutTask/ooo_df.csv')


# do averaging
subject_acc_ooo_df <- ooo_df %>%
  group_by(subject, target_threat, nontarget_threat) %>%
  summarize(mean_acc = mean(acc))
subject_acc_ooo_df

#figure out how many holes there are in ooo factorial design
sub_mean_df <- subject_acc_ooo_df %>%
  group_by(subject) %>%
  summarize(n_rows = n()) %>%
  group_by(n_rows) %>%
  summarize(n_subjects = n())
sub_mean_df

# add column of each subjects anxiety (using left join). should be code somewhere
df_STAI_ooo <-read_csv('INSERT PATH TO data/STAI.csv FROM GITHUB')
df_STAI_ooo

df_ooo_STAI_simple <- df_STAI_ooo %>%
  select(subjectID, anxiety_level)
df_ooo_STAI_simple

names(df_ooo_STAI_simple) <- c('subject', 'anxiety_level')
df_ooo_STAI_simple

subject_acc_ooo_df <- merge(subject_acc_ooo_df, df_ooo_STAI_simple, by = "subject", all = TRUE)
subject_acc_ooo_df
write_csv(subject_acc_ooo_df, 'data/oddOneOutTask/subject_acc_ooo_df.csv')

#find mean mean acc
subject_acc_ooo_df_mean <- subject_acc_ooo_df %>%
  summarize(mean_mean_acc = mean(mean_acc))
subject_acc_ooo_df_mean


#then run anova
ooo_model_3 <- aov(mean_acc ~ target_threat + anxiety_level + nontarget_threat + 
                     target_threat:anxiety_level + nontarget_threat:anxiety_level +
                     Error(subject/(target_threat + nontarget_threat)), data = subject_acc_ooo_df)
ooo_model_3
result_ooo_model_3 <- summary(ooo_model_3)
result_ooo_model_3
#tidy(ooo_model_3)

library(emmeans)
emm <- emmeans(ooo_model_3, ~ target_threat | anxiety_level)
emm
print(contrast(emm, interaction="pairwise"))


#filter subjects that have no holes in factorial design to see how many holes for omnibus ANOVA
subjects_no_holes <- subject_acc_ooo_df %>%
  group_by(subject) %>%
  summarize(n_rows = n()) %>%
  filter(n_rows >= 4)
subjects_no_holes

subject_acc_ooo_df_filtered <- subject_acc_ooo_df %>%
  filter(subject %in% subjects_no_holes$subject)
subject_acc_ooo_df_filtered
#168 rows instead of 335 when filtered, so about half cut

#how many subjects left for omnibus ANOVA?
unique_subjects_ooo_model2 <- subject_acc_ooo_df_filtered %>%
  distinct(subject) %>%
  nrow()
print(unique_subjects_ooo_model2)
#42, 101 - 42 = 59 subject holes





#PLOTS

#plot for target threat
ooo_acc_box_plot2 <- ggplot(subject_acc_ooo_df, aes(x = target_threat, y = mean_acc, fill=anxiety_level)) +
  geom_boxplot(alpha=0.5, outlier.shape = NA) +
  #geom_jitter(width=0.1 ,height = 0.0) +
  geom_point(position = position_jitterdodge(jitter.width = 0.1)) +
  labs(title = "Mean Accuracy by Target Threat", x = "Target Threat", y = "Mean Accuracy") +
  scale_fill_manual(values = c("high trait anxiety" = "#F3DD1B", "low to moderate trait anxiety" = "#4BACC6"),
                    labels = c("High Trait Anxiety", "Low to Moderate Trait Anxiety"))+
  scale_x_discrete(labels = c('neutral' = 'Neutral', 'threat' = "Threat")) +
  labs(fill = "Anxiety Level")
ggsave("plots/oddOneOutTask/ooo_box_plot2.jpg", ooo_acc_box_plot2)
ooo_acc_box_plot2

#plot for nontarget threat
ooo_acc_box_plot3 <- ggplot(subject_acc_ooo_df, aes(x = nontarget_threat, y = mean_acc, fill=anxiety_level)) +
  geom_boxplot(alpha=0.5, outlier.shape = NA) +
  #geom_jitter(width=0.1 ,height = 0.0) +
  geom_point(position = position_jitterdodge(jitter.width = 0.1)) +
  labs(title = "Mean Accuracy by Nontarget Threat", x = "Nontarget Threat", y = "Mean Accuracy") +
  scale_x_discrete(labels = c("has_threat" = "Has Threat", "no_threat" = "No Threat")) +
  scale_fill_manual(values = c("high trait anxiety" = "#F3DD1B", "low to moderate trait anxiety" = "#4BACC6"),
                    labels = c("High Trait Anxiety", "Low to Moderate Trait Anxiety"))+
  labs(fill = "Anxiety Level")
ggsave("plots/oddOneOutTask/ooo_box_plot3.jpg", ooo_acc_box_plot3)
ooo_acc_box_plot3

#how many subjects with maxTrialCount <= #
subject_ooo_df_ceiling <- subject_acc_ooo_df %>%
  filter(mean_acc >= 0.9)
unique_subjects_ooo_ceiling <- unique(subject_ooo_df_ceiling$subject)
num_unique_subjects_ooo_ceiling <- length(unique_subjects_ooo_ceiling)
print(num_unique_subjects_ooo_ceiling)
