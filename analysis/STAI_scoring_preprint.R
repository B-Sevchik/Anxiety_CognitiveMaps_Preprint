#This script calculates the anxiety scores for participants.

#set-up
setwd("~/Documents/GitHub/Anxiety_CognitiveMaps_Preprint")
library(tidyverse)
library(dplyr)

#BE SURE TO EDIT PATH NAMES EACH TIME YOU RUN SCRIPT

#path references
data_path <- '/Users/brookesevchik/Documents/GitHub/Anxiety_CognitiveMaps_Preprint/original_STAI_data_df' #EDIT PATH NAME EACH TIME

#load data
STAI_df <- read_csv(data_path)

#getting STAI scores into numeric form
STAI_df <- transform(STAI_df,s1 = as.numeric(s1))
STAI_df <- STAI_df %>%
  mutate(across(paste0('s', 1:20), as.numeric))

#changing non-responses (5) into NaN
STAI_df <- STAI_df %>%
  mutate(across(paste0('s', 1:20), ~ case_when(. == 5 ~ 0, 
                                               TRUE ~ .)))

#reverse score what needs to be reverse scored
STAI_df <- STAI_df %>%
  group_by(subjectID) %>%
  mutate(s1 = ifelse(s1== 0, 0, 5 - s1),
         s3 = ifelse(s3== 0, 0, 5 - s3),
         s6 = ifelse(s6== 0, 0, 5 - s6),
         s7 = ifelse(s7== 0, 0, 5 - s7),
         s10 = ifelse(s10== 0, 0, 5 - s10),
         s13 = ifelse(s13== 0, 0, 5 - s13),
         s14 = ifelse(s14== 0, 0, 5 - s14),
         s16 = ifelse(s16== 0, 0, 5 - s16),
         s19 = ifelse(s19== 0, 0, 5 - s19))

#find how many NaNs (0s) there are
#count the number of 0s in each column
zero_counts <- colSums(STAI_df == 0, na.rm = TRUE)

#sum up the total number of 0s in the dataset
total_zeros <- sum(zero_counts)

#print the total amount of NAs
print(total_zeros)

#get the mean & replace NaNs(0s) w the mean 
STAI_df <- STAI_df %>%
  mutate(meanVals = (s1 + s2 + s3 + s4 + s5 + s6 + s7 + s8 + s9 + s10 + s11 + s12 + s13 + s14 + s15 + s16 + s17 + s18 + s19 + s20)/ 20) %>%
  mutate(across(paste0('s', 1:20), ~ case_when(. == 0 ~ meanVals, 
                                               TRUE ~ .)))

#sum STAI score
STAI_df <-STAI_df %>%
  group_by(subjectID) %>%
  mutate(sumVals = s1 + s2 + s3 + s4 + s5 + s6 + s7 + s8 + s9 + s10 + s11 + s12 + s13 + s14 + s15 + s16 + s17 + s18 + s19 + s20)
STAI_df

#classify anxiety levels based on STAI sum (low, moderate, & high)
STAI_df <- STAI_df %>%
  group_by(subjectID) %>%
  mutate(anxiety_level = case_when(
    sumVals <= 37 ~ 'low trait anxiety',
    sumVals >= 38 & sumVals < 44 ~ 'moderate trait anxiety',
    sumVals >= 44 ~ 'high trait anxiety'
  ))
STAI_df

#how many moderate anxiety?
#participant_m_df <- STAI_df %>%
#filter(anxiety_level == 'moderate trait anxiety')
#participant_m_df
#num_m <- nrow(participant_m_df)
#num_m

#classify anxiety levels (low to moderate vs. high binary)
STAI_df <- STAI_df %>%
  group_by(subjectID) %>%
  mutate(anxiety_level = case_when(
    sumVals < 44  ~ 'low to moderate trait anxiety',
    sumVals >= 44 ~ 'high trait anxiety'
  ))
STAI_df

#filter to only exclude excluded subjects from preprocessing
excluded_subjects <- read_csv('data/excludedSubjects.csv')
STAI_df <- STAI_df %>%
  filter(!subjectID %in% excluded_subjects$subject)
STAI_df

#save out the file
write.csv(STAI_df, 'data/STAI.csv')

#determine how many participants are in each category: low to moderate vs. high anxiety
participant_lm_df <- STAI_df %>%
  filter(anxiety_level == 'low to moderate trait anxiety')
participant_lm_df
num_lm <- nrow(participant_lm_df)
num_lm

participant_h_df <- STAI_df %>%
  filter(anxiety_level == 'high trait anxiety')
participant_h_df
num_h <- nrow(participant_h_df)
num_h
