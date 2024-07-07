##This script manipulates the data frame for illegal transition task analyses, then also runs the ANOVA to determine statistical significance and creates plots for illegal transition task.

setwd("~/Documents/GitHub/Influence-of-Anxiety-and-Threat-on-Cognitive-Map-Learning")
library(plotrix)
library(ggplot2)
library(gridExtra)
library(dplyr)
library(readr)
library(ggplot2)
library(tidyr)
library(tidyverse)
library(psycho)
library(broom)

#path reference
data_path <- '/Users/brookesevchik/Box/Data/Anxiety_Cognitive_Maps/all_participants/combinedData_Anxiety_Cognitive_Maps.csv' 

#load data
df <- read_csv(data_path)
illegal_df <- df %>%
  filter(taskName == 'illegalTransitionTask')
illegal_df

acc_illegal_df <- illegal_df %>%
  select(subject, transitionType, acc, transitionThreatKind, activeNodeThreatStatus, trialCount) %>%
  group_by(transitionType, acc)
acc_illegal_df
write_csv(acc_illegal_df, 'data/illegalTransitionTask/acc_illegal_df.csv')

transition_type_df <- acc_illegal_df %>%
  mutate(acc_descriptor = case_when(
    (transitionType == 'i' & acc == 1) ~ "hits",
    (transitionType == 'i' & acc == 0) ~ "misses",
    (transitionType == 'l' & acc == 0) ~ "false_alarms",
    (transitionType == 'l' & acc == 1) ~ "correct_rejections"
  ))
transition_type_df

transition_type_df <- transition_type_df %>%
  mutate(transition_threat = case_when(
    (transitionThreatKind == 'threat-threat' | transitionThreatKind == 'threat-neutral' | transitionThreatKind == 'neutral-threat') ~ "contains_threat",
    (transitionThreatKind == 'neutral-neutral') ~ "no_threat"
  ))
transition_type_df

#add in prevTrialThreatStatus using trialCount
transition_type_df_prev <- transition_type_df %>%
  arrange(subject, trialCount) %>%
  group_by(subject) %>%
  mutate(prevTrialThreatStatus = ifelse(trialCount == 1, NA, lag(activeNodeThreatStatus)))
transition_type_df_prev

transition_type_df_prev_filtered <- transition_type_df_prev %>%
  filter(!is.na(prevTrialThreatStatus))
transition_type_df_prev_filtered

counts_transitionType_df <- transition_type_df_prev_filtered %>%
  group_by(subject, acc_descriptor, transition_threat, activeNodeThreatStatus, prevTrialThreatStatus) %>%
  summarize(count = n()) %>%
  pivot_wider(names_from = acc_descriptor, values_from = count, values_fill = 0)
counts_transitionType_df

dprime_df <- counts_transitionType_df
dprime.stats <-psycho::dprime(counts_transitionType_df$hits, counts_transitionType_df$false_alarms, counts_transitionType_df$misses, counts_transitionType_df$correct_rejections)
dprime_df$dprime <- dprime.stats$dprime
dprime_df

excluded_subjects <- read_csv('data/excludedSubjects.csv')
dprime_df <- dprime_df %>%
  filter(!subject %in% excluded_subjects$subject)
dprime_df
write_csv(dprime_df, 'data/oddOneOutTask/dprime_df.csv')

#how many unique subjects in dprime df? 101
#unique_subjects_dprime <- unique(dprime_df$subject)
#unique_subjects_dprime_count <- length(unique_subjects_dprime)
#unique_subjects_dprime_count

#add in anxiety levels
STAI_illegal_df <- read_csv('data/STAI.csv')
STAI_illegal_df

illegal_STAI_simple <- STAI_illegal_df %>%
  select(subjectID, anxiety_level)
illegal_STAI_simple

names(illegal_STAI_simple) <- c('subject', 'anxiety_level')
illegal_STAI_simple

#how many unique subjects in illegal STAI simple df? 101
#unique_subjects_illegal <- unique(illegal_STAI_simple$subject)
#unique_subjects_illegal_count <- length(unique_subjects_illegal)
#unique_subjects_illegal_count

#merge anxiety scores into illegal transition df
illegal_anxiety_df <- merge(dprime_df, illegal_STAI_simple, by = "subject", all = TRUE)
illegal_anxiety_df
write_csv(illegal_anxiety_df, 'data/illegalTransitionTask/illegal_analysis_df.csv')

#figure out mean dprime scores
illegal_anxiety_df_mean <- illegal_anxiety_df %>%
  summarize(mean_dprime = mean(dprime))
illegal_anxiety_df_mean
#find mean mean acc
subject_acc_ooo_df_mean <- subject_acc_ooo_df %>%
  summarize(mean_mean_acc = mean(mean_acc))
subject_acc_ooo_df_mean


#run rmANOVA w both activeNodeThreatStatus & prevTrialThreatStatus
library(stats)
# Fit the model
illegal_transition_model4 <- aov(dprime ~ activeNodeThreatStatus * prevTrialThreatStatus * anxiety_level + Error(subject/(prevTrialThreatStatus * activeNodeThreatStatus)), data = illegal_anxiety_df)
result_illegal_transition_model4 <- summary(illegal_transition_model4)
result_illegal_transition_model4

library(emmeans)
emm <- emmeans(illegal_transition_model4, ~ anxiety_level)
print(summary(emm))

library(emmeans)
emm <- emmeans(illegal_transition_model4, ~ activeNodeThreatStatus)
print(summary(emm))



#PLOT - activeNodeThreatStatus
illegal_transition_box_plot2 <- ggplot(illegal_anxiety_df, aes(x = activeNodeThreatStatus, y = dprime, fill = anxiety_level)) +
  geom_boxplot(alpha = 0.5, outlier.shape = NA) +
  #geom_jitter(width = 0.1) +
  geom_point(position = position_jitterdodge(jitter.width = 0.1)) +
  labs(title = "D Prime by Current Threat Type", x = "Current Threat Type", y = "D Prime Score",
       fill = "Anxiety Level") +
  scale_fill_manual(values = c("high trait anxiety" = "#F3DD1B", "low to moderate trait anxiety" = "#4BACC6"),
                    labels = c("High Trait Anxiety", "Low to Moderate Trait Anxiety")) +
  scale_x_discrete(labels = c("neutral" = "Neutral", "threat" = "Threat"))
illegal_transition_box_plot2
ggsave("plots/illegalTransitionTask/illegal_transition_box_plot2.jpg", illegal_transition_box_plot2)
