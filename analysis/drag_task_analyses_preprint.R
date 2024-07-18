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
df <- read_csv('[INSERT DATA PATH') #In Github, this is the 'original_data' file

check_answer_df <- df %>%
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
excluded_subjects <- read_csv('data/excludedSubjects.csv')
check_answer_df <- check_answer_df %>%
  filter(!subject %in% excluded_subjects$subject)
check_answer_df

write_csv(check_answer_df, 'data/checkAnswer.csv')


# finding trial attempts by trial and subject
trialAttemptsByTrial <- check_answer_df %>% 
  group_by(subject, trialCount) %>% 
  summarize(n_attempts = max(trialAttempt))

included_sub_trials <- trialAttemptsByTrial %>% 
  filter(trialCount != 1 & n_attempts != 1) %>% 
  mutate(combined = paste(subject, trialCount, sep="_"))
unique_included_sub_trials <- data.frame(subject = unique(included_sub_trials$subject))
unique_included_sub_trials
#only 22 subjects usable, when using the criteria that we filter out the first drag & drop and the last three where they get it all correct

#list of column names for easy reference
correct_type_columns <- colnames(check_answer_df)[grepl("CorrectType", colnames(check_answer_df))]
correct_src_columns <- colnames(check_answer_df)[grepl("CorrectSRC", colnames(check_answer_df))]
current_src_columns <- colnames(check_answer_df)[grepl("CurrentSRC", colnames(check_answer_df))]
current_type_columns <- colnames(check_answer_df)[grepl("CurrentType", colnames(check_answer_df))]
acc_columns <- colnames(check_answer_df)[grepl("Acc", colnames(check_answer_df))]

#remove columns we don't need
new_check_answer_df <- check_answer_df  %>% 
  select(-correct_type_columns) %>% 
  select(-correct_src_columns) %>% 
  select(-RT) %>% 
  select(-nCorrect) %>% 
  filter(paste(subject, trialCount, sep="_") %in% included_sub_trials$combined)

slot_accuracies <- new_check_answer_df %>% 
  select(-current_src_columns) %>% 
  select(-current_type_columns) %>%
  pivot_longer(cols = !c(subject, trialCount, trialAttempt), names_to = "slot", values_to = "acc") %>% 
  mutate(slot = gsub("Acc", "", slot))

slot_srcs <- new_check_answer_df %>% 
  select(-acc_columns) %>% 
  select(-current_type_columns) %>% 
  pivot_longer(cols = !c(subject, trialCount, trialAttempt), names_to = "slot", values_to = "src") %>% 
  mutate(slot = gsub("CurrentSRC", "", slot))

slot_threat_types <- new_check_answer_df %>% 
  select(-acc_columns) %>% 
  select(-current_src_columns) %>% 
  pivot_longer(cols = !c(subject, trialCount, trialAttempt), names_to = "slot", values_to = "threatType") %>% 
  mutate(slot = gsub("CurrentType", "", slot))

#join everything together
slot_images <- slot_srcs %>% 
  left_join(slot_threat_types, by=c("subject", "trialCount", "trialAttempt", "slot")) %>% 
  left_join(slot_accuracies, by=c("subject", "trialCount", "trialAttempt", "slot")) %>% 
  mutate(src = gsub(".jpg", "", src)) %>% 
  mutate(src = paste(src, "_", threatType, ".jpg", sep="")) %>% 
  select(-threatType)
slot_images

#SCORING

first_correct_no_further_mistakes_df <- slot_images %>% 
  filter(acc == 1) %>% 
  group_by(subject, trialCount, src) %>%
  mutate(trialAttempt_lag = lag(trialAttempt)) %>% 
  mutate(no_skip = ifelse(trialAttempt_lag == trialAttempt - 1, TRUE, FALSE),
         no_skip = ifelse(is.na(no_skip), FALSE, no_skip)) %>% 
  filter(no_skip == FALSE) %>% 
  summarise(first_correct_no_further_mistakes = last(trialAttempt))
first_correct_no_further_mistakes_df
write_csv(first_correct_no_further_mistakes_df, 'data/dragTask/rawScore.csv')


#next step after that, group by subject and src (collapse across trials) to find the average accuracy score for each trial
average_scores <- first_correct_no_further_mistakes_df %>%
  group_by(subject, src) %>%
  summarise(n_trials = n(),
            score = mean(first_correct_no_further_mistakes))
write_csv(average_scores, 'data/dragTask/averageScores.csv')

#last step, group by just src to find mean accuracy score for each image (collapse across participants)
src_average_scores <- average_scores %>%
  group_by(src) %>%
  summarise(score_mean = mean(score), 
            score_sem = std.error(score)) %>% 
  mutate(condition = ifelse(grepl("threat", src), "threat", "neutral"))
write_csv(src_average_scores, 'data/dragTask/SRCaverageScores.csv')

#plot (without anxiety scores)
drag_task_box_plot <- ggplot(src_average_scores, aes(x = condition, y = score_mean, fill=condition)) +
  geom_boxplot(alpha=0.5) +
  geom_jitter()
ggsave("plots/dragTask/drag_task_box_plot.jpg", drag_task_box_plot)
drag_task_box_plot

#manipulate data frame for analysis
drag_drop_analysis_df <- average_scores %>% 
  mutate(threattype = ifelse(grepl("threat", src), "threat", "neutral")) %>% 
  group_by(subject, threattype) %>% 
  summarise(mean_score = mean(score))
drag_drop_analysis_df

#join in each subjects anxiety score
STAI_drag_drop_df <- read_csv('data/STAI.csv')
STAI_drag_drop_df <- STAI_drag_drop_df %>%
  select(subjectID, anxiety_level, sumVals)
STAI_drag_drop_df
colnames(STAI_drag_drop_df)[colnames(STAI_drag_drop_df) == "subjectID"] <- "subject"
colnames(STAI_drag_drop_df)[colnames(STAI_drag_drop_df) == "sumVals"] <- "anxiety_score"

drag_drop_analysis_STAI_df <- merge(drag_drop_analysis_df, STAI_drag_drop_df, by = "subject")
drag_drop_analysis_STAI_df
write_csv(drag_drop_analysis_STAI_df, 'data/dragTask/analysisdf.csv')


#run  rmANOVA
drag_drop_model <- aov(mean_score ~ threattype * anxiety_level + Error(subject/threattype), data = drag_drop_analysis_STAI_df)
drag_drop_model
result_drag_drop_model <- summary(drag_drop_model)
result_drag_drop_model
#based on p-values, nothing is significant
library(emmeans)
emm <- emmeans(drag_drop_model, ~ threattype | anxiety_level)
emm
print(contrast(emm, interaction="pairwise"))


#PLOTS
#bar chart with error bars
library(ggplot2)

drag_task_bar_chart <- ggplot(drag_drop_analysis_STAI_df, aes(x = threattype, y = mean_score, fill = anxiety_level)) +
  stat_summary(fun.y = "mean", geom = "bar", position = "dodge", color = 'black', alpha = 0.5) +  # Setting alpha directly within geom_bar()
  stat_summary(fun.data = "mean_se", geom = "errorbar", position = position_dodge(0.9), width=0.2) +
  labs(title = 'Average Attempts Until Correct by Threat Type',
       x = 'Threat Type',
       y = 'Attempts-Until-Correct') +
  labs(fill = 'Anxiety Level') +
  scale_fill_manual(values = c("high trait anxiety" = "#F3DD1B", "low to moderate trait anxiety" = "#4BACC6"),
                    labels = c("High Trait Anxiety", "Low to Moderate Trait Anxiety"))

print(drag_task_bar_chart)

ggsave("plots/dragTask/drag_task_bar_chart.jpg", drag_task_bar_chart)
