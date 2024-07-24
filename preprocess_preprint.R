#This script is for preprocessing, it organizes the raw data for each of the tasks from the original data.
#It also excludes the subjects for not meeting requirements for each of the three tasks.
#Run preprocess before you run any other script, as well as download original_data and original_STAI_data_df from Github.
#data paths to insert: line 6, 11

setwd("[INSERT PATH TO GITHUB DIRECTORY IN COMPUTER FILE SYSTEM]")

library (dplyr)
library(tidyverse)

df <- read_csv('[INSERT PATH TO original_data FILE FROM GITHUB]')

#DRAG TASK exclude people from drag task if they have reached the 10th trial and still haven't learned it
drag_df <- df %>%
  filter(taskName == 'networkDragTask') %>%
  select(subject, trialCount, trialAttempt, RT, nCorrect, dragThreat, dragAcc, swappedThreat, swappedAcc,
         slot0Acc, slot0CurrentType, slot0CorrectType, slot0CurrentSRC, slot0CorrectSRC, slot1Acc, 
         slot1CurrentType, slot1CorrectType,slot1CurrentSRC, slot1CorrectSRC,
         slot2Acc, slot2CurrentType, slot2CorrectType, slot2CurrentSRC, slot2CorrectSRC,
         slot3Acc, slot3CurrentType, slot3CorrectType, slot3CurrentSRC, slot3CorrectSRC,
         slot4Acc, slot4CurrentType, slot4CorrectType, slot4CurrentSRC, slot4CorrectSRC,
         slot5Acc, slot5CurrentType, slot5CorrectType, slot5CurrentSRC, slot5CorrectSRC,
         slot6Acc, slot6CurrentType, slot6CorrectType, slot6CurrentSRC, slot6CorrectSRC,
         slot7Acc, slot7CurrentType, slot7CorrectType, slot7CurrentSRC, slot7CorrectSRC,
         slot8Acc, slot8CurrentType, slot8CorrectType, slot8CurrentSRC, slot8CorrectSRC,
         slot9Acc, slot9CurrentType, slot9CorrectType, slot9CurrentSRC, slot9CorrectSRC)
write_csv(drag_df, 'data/dropEvent.csv')

# this gets us how many trials each person had , and how many attempts they took on each attempt
drag_counts <- drag_df %>% 
  group_by(subject, trialCount) %>% 
  summarise(trialAttempts = max(trialAttempt))

# this gets us only those subjects who had a 10th trial, and where they still weren't getting it in 1 attempt on that trial (10 trials but got it in one attempt on that trial is ok)
excluded_drag_subjects <- drag_df %>% 
  filter(trialCount == 10 & trialAttempt > 1) %>%
  select(subject)

excluded_drag_subjects <- excluded_drag_subjects %>% 
  distinct(subject)


#ILLEGAL TRANSITION TASK PERMUTATION-BASED EXCLUSION
illegal_df <- df %>% 
  filter(taskName == "illegalTransitionTask") %>% 
  select(subject, transitionType, transitionThreatKind, trialCount, blockTrialCount, block, RT, acc, activeNodeCommunityCongruency:transitionThreatKind)
write_csv(illegal_df, 'data/invalidTransition.csv')

acc_illegal_df <- illegal_df %>%
  select(subject, transitionType, acc, transitionThreatKind) %>%
  group_by(transitionType, acc)

transition_type_df <- acc_illegal_df %>%
  mutate(acc_descriptor = case_when(
    (transitionType == 'i' & acc == 1) ~ "hits",
    (transitionType == 'i' & acc == 0) ~ "misses",
    (transitionType == 'l' & acc == 0) ~ "false_alarms",
    (transitionType == 'l' & acc == 1) ~ "correct_rejections"
  ))

transition_type_df <- transition_type_df %>%
  mutate(transition_threat = case_when(
    (transitionThreatKind == 'threat-threat' | transitionThreatKind == 'threat-neutral' | transitionThreatKind == 'neutral-threat') ~ "contains_threat",
    (transitionThreatKind == 'neutral-neutral') ~ "no_threat"
  ))
write_csv(transition_type_df, 'data/transitionType.csv')

counts_transitionType_df <- transition_type_df %>%
  group_by(subject, acc_descriptor, transition_threat) %>%
  summarize(count = n()) %>%
  pivot_wider(names_from = acc_descriptor, values_from = count, values_fill = 0)
write_csv(counts_transitionType_df, 'data/counts_transitionType.csv')

dprime_df <- counts_transitionType_df
dprime.stats <-psycho::dprime(counts_transitionType_df$hits, counts_transitionType_df$false_alarms, counts_transitionType_df$misses, counts_transitionType_df$correct_rejections)
dprime_df$dprime <- dprime.stats$dprime
write_csv(dprime_df, 'data/dprime.csv')

excluded_illegal_subs <- dprime_df %>% 
  filter(dprime < 0.3573812)


#ODD ONE OUT TASK exclude anyone below 40% accuracy
ooo_df <- df %>% 
  filter(taskName == "oddOneOutTest") %>% 
  select(subject, trialCount, RT, acc, partResp, chosenNode:chosenThreatStatus,
         option1CommunityNumber, option1ThreatStatus,
         option2CommunityNumber, option2ThreatStatus,
         option3CommunityNumber, option3ThreatStatus)
write_csv(ooo_df, 'data/OddOneOut.csv')

ooo_acc <- ooo_df %>% 
  group_by(subject) %>% 
  summarise(acc = mean(acc, na.rm = TRUE))
write_csv(ooo_acc, 'data/OOOAcc.csv')

excluded_ooo_subs <- ooo_acc %>%
  filter(acc < 0.4)




#JOIN EXCLUDED SUBS DATA FRAMES
excluded_subjects <- excluded_drag_subjects %>% 
  full_join(excluded_illegal_subs, by="subject") %>% 
  full_join(excluded_ooo_subs, by="subject") %>% 
  select(subject)
write_csv(excluded_subjects, 'data/excludedSubjects.csv')

