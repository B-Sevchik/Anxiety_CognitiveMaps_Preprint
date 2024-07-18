#This script manipulates the data frame for drag task analyses, then also runs the ANOVA to determine statistical significance and creates plots for drag task.
#DATA PATHS TO CHANGE: lines 5, 15

#SET WORKING DIRECTORY TO WHAT YOU WANT IT TO BE!
setwd("~/Documents/GitHub/Anxiety_CognitiveMaps_Preprint")
library(plotrix)
library(ggplot2)
library(gridExtra)
library(dplyr)
library(readr)
library(ggplot2)
library(tidyr)

#CHANGE DATA PATH TO WHERE THE FILE WITH THE ORIGINAL DATA FILE YOU PULLED FROM GITHUB IS STORED ON YOUR COMPUTER!
df <- read_csv('[INSERT DATA PATH]') #In Github, this is the 'original_data' file

check_answer_df_1 <- df %>%
  filter(sectionType == 'dragTaskCheckAnswerEvent') %>%
  select(subject, trialCount, trialAttempt, RT, nCorrect, slot0Acc, slot0CurrentType, slot0CorrectType, slot0CurrentSRC, slot0CorrectSRC, slot1Acc, 
         slot1CurrentType, slot1CorrectType,slot1CurrentSRC, slot1CorrectSRC,
         slot2Acc, slot2CurrentType, slot2CorrectType, slot2CurrentSRC, slot2CorrectSRC,
         slot3Acc, slot3CurrentType, slot3CorrectType, slot3CurrentSRC, slot3CorrectSRC,
         slot4Acc, slot4CurrentType, slot4CorrectType, slot4CurrentSRC, slot4CorrectSRC,
         slot5Acc, slot5CurrentType, slot5CorrectType, slot5CurrentSRC, slot5CorrectSRC,
         slot6Acc, slot6CurrentType, slot6CorrectType, slot6CurrentSRC, slot6CorrectSRC,
         slot7Acc, slot7CurrentType, slot7CorrectType, slot7CurrentSRC, slot7CorrectSRC,
         slot8Acc, slot8CurrentType, slot8CorrectType, slot8CurrentSRC, slot8CorrectSRC,
         slot9Acc, slot9CurrentType, slot9CorrectType, slot9CurrentSRC, slot9CorrectSRC)

#exclude subjects based on exclusion criteria in preprocessing
excluded_subjects_1 <- read_csv('data/excludedSubjects.csv')
check_answer_df_1 <- check_answer_df_1 %>%
  filter(!(subject %in% excluded_subjects_1$subject))
check_answer_df_1

write_csv(check_answer_df_1, 'data/checkAnswer_1.csv')


# finding trial attempts by trial and subject
trialAttemptsByTrial_1 <- check_answer_df_1 %>% 
  group_by(subject, trialCount) %>% 
  summarize(n_attempts = max(trialAttempt))

included_sub_trials_1 <- trialAttemptsByTrial_1 %>% 
  filter(n_attempts != 1) %>% 
  mutate(combined = paste(subject, trialCount, sep="_"))
unique_included_sub_trials_1 <- data.frame(subject = unique(included_sub_trials_1$subject))
unique_included_sub_trials_1
#only 25 subjects usable, when using the criteria that we filter out the first drag & drop and the last three where they get it all correct

#list of column names for easy reference
correct_type_columns <- colnames(check_answer_df_1)[grepl("CorrectType", colnames(check_answer_df_1))]
correct_src_columns <- colnames(check_answer_df_1)[grepl("CorrectSRC", colnames(check_answer_df_1))]
current_src_columns <- colnames(check_answer_df_1)[grepl("CurrentSRC", colnames(check_answer_df_1))]
current_type_columns <- colnames(check_answer_df_1)[grepl("CurrentType", colnames(check_answer_df_1))]
acc_columns <- colnames(check_answer_df_1)[grepl("Acc", colnames(check_answer_df_1))]

#remove columns we don't need
new_check_answer_df_1 <- check_answer_df_1  %>% 
  select(-correct_type_columns) %>% 
  select(-correct_src_columns) %>% 
  select(-RT) %>% 
  select(-nCorrect) %>% 
  filter(paste(subject, trialCount, sep="_") %in% included_sub_trials_1$combined)

slot_accuracies_1 <- new_check_answer_df_1 %>% 
  select(-current_src_columns) %>% 
  select(-current_type_columns) %>%
  pivot_longer(cols = !c(subject, trialCount, trialAttempt), names_to = "slot", values_to = "acc") %>% 
  mutate(slot = gsub("Acc", "", slot))

slot_srcs_1 <- new_check_answer_df_1 %>% 
  select(-acc_columns) %>% 
  select(-current_type_columns) %>% 
  pivot_longer(cols = !c(subject, trialCount, trialAttempt), names_to = "slot", values_to = "src") %>% 
  mutate(slot = gsub("CurrentSRC", "", slot))

slot_threat_types_1 <- new_check_answer_df_1 %>% 
  select(-acc_columns) %>% 
  select(-current_src_columns) %>% 
  pivot_longer(cols = !c(subject, trialCount, trialAttempt), names_to = "slot", values_to = "threatType") %>% 
  mutate(slot = gsub("CurrentType", "", slot))

#join everything together
slot_images_1 <- slot_srcs_1 %>% 
  left_join(slot_threat_types_1, by=c("subject", "trialCount", "trialAttempt", "slot")) %>% 
  left_join(slot_accuracies_1, by=c("subject", "trialCount", "trialAttempt", "slot")) %>% 
  mutate(src = gsub(".jpg", "", src)) %>% 
  mutate(src = paste(src, "_", threatType, ".jpg", sep="")) %>% 
  select(-threatType)
slot_images_1

#SCORING

first_correct_no_further_mistakes_df_1 <- slot_images_1 %>% 
  filter(acc == 1) %>% 
  group_by(subject, trialCount, src) %>%
  mutate(trialAttempt_lag = lag(trialAttempt)) %>% 
  mutate(no_skip = ifelse(trialAttempt_lag == trialAttempt - 1, TRUE, FALSE),
         no_skip = ifelse(is.na(no_skip), FALSE, no_skip)) %>% 
  filter(no_skip == FALSE) %>% 
  summarise(first_correct_no_further_mistakes_1 = last(trialAttempt))
first_correct_no_further_mistakes_df_1
write_csv(first_correct_no_further_mistakes_df_1, 'data/dragTask/rawScore_1.csv')


#next step after that, group by subject and src (collapse across trials) to find the average accuracy score for each trial
average_scores_1 <- first_correct_no_further_mistakes_df_1 %>%
  group_by(subject, src) %>%
  summarise(n_trials = n(),
            score = mean(first_correct_no_further_mistakes_1))
write_csv(average_scores_1, 'data/dragTask/averageScores_1.csv')

#last step, group by just src to find mean accuracy score for each image (collapse across participants)
src_average_scores_1 <- average_scores_1 %>%
  group_by(src) %>%
  summarise(score_mean = mean(score), 
            score_sem = std.error(score)) %>% 
  mutate(condition = ifelse(grepl("threat", src), "threat", "neutral"))
write_csv(src_average_scores_1, 'data/dragTask/SRCaverageScores_1.csv')

#plot (without anxiety scores)
drag_task_box_plot_1 <- ggplot(src_average_scores_1, aes(x = condition, y = score_mean, fill=condition)) +
  geom_boxplot(alpha=0.5) +
  geom_jitter()
ggsave("plots/dragTask/drag_task_box_plot_1.jpg", drag_task_box_plot_1)
drag_task_box_plot_1

#manipulate data frame for analysis
drag_drop_analysis_df_1 <- average_scores_1 %>% 
  mutate(threattype = ifelse(grepl("threat", src), "threat", "neutral")) %>% 
  group_by(subject, threattype) %>% 
  summarise(mean_score = mean(score))
drag_drop_analysis_df_1

#join in each subjects anxiety score
STAI_drag_drop_df_1 <- read_csv('data/STAI.csv')
STAI_drag_drop_df_1 <- STAI_drag_drop_df_1 %>%
  select(subjectID, anxiety_level, sumVals)
STAI_drag_drop_df_1
colnames(STAI_drag_drop_df_1)[colnames(STAI_drag_drop_df_1) == "subjectID"] <- "subject"
colnames(STAI_drag_drop_df_1)[colnames(STAI_drag_drop_df_1) == "sumVals"] <- "anxiety_score"

drag_drop_analysis_STAI_df_1 <- merge(drag_drop_analysis_df_1, STAI_drag_drop_df_1, by = "subject")
drag_drop_analysis_STAI_df_1
write_csv(drag_drop_analysis_STAI_df_1, 'data/dragTask/analysisdf_1.csv')


#run  rmANOVA
drag_drop_model_1 <- aov(mean_score ~ threattype * anxiety_level + Error(subject/threattype), data = drag_drop_analysis_STAI_df_1)
drag_drop_model_1
result_drag_drop_model_1 <- summary(drag_drop_model_1)
result_drag_drop_model_1
#based on p-values, nothing is significant
library(emmeans)
emm <- emmeans(drag_drop_model_1, ~ threattype | anxiety_level)
emm
print(contrast(emm, interaction="pairwise"))


#PLOTS
#bar chart with error bars
library(ggplot2)

drag_task_bar_chart_1 <- ggplot(drag_drop_analysis_STAI_df_1, aes(x = threattype, y = mean_score, fill = anxiety_level)) +
  stat_summary(fun.y = "mean", geom = "bar", position = "dodge", color = 'black', alpha = 0.5) +  # Setting alpha directly within geom_bar()
  stat_summary(fun.data = "mean_se", geom = "errorbar", position = position_dodge(0.9), width=0.2) +
  labs(title = 'Average Attempts Until Correct by Threat Type',
       x = 'Threat Type',
       y = 'average attempts until correct') +
  labs(fill = 'Anxiety Level') +
  scale_fill_manual(values = c("high trait anxiety" = "#F3DD1B", "low to moderate trait anxiety" = "#4BACC6"))

print(drag_task_bar_chart_1)

ggsave("plots/dragTask/drag_task_bar_chart_1.jpg", drag_task_bar_chart_1)
