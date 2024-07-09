library(tidyverse)
library(dplyr)

#load in check_answer_df
setwd("~/Documents/GitHub/Anxiety_CognitiveMaps_Preprint")
check_answer_df <- read_csv('data/checkAnswer.csv')
check_answer_df

attempts_per_trial <- check_answer_df %>% 
  group_by(subject, trialCount) %>% 
  summarise(maxAttempts = max(trialAttempt))
attempts_per_trial

##figure out how many participants completed the task within a certain number of trials

#max trialCount
max_trials <- attempts_per_trial %>%
  group_by(subject) %>%
  summarise(maxTrialCount = max(trialCount))
max_trials

#how many subjects with maxTrialCount <= #
max_trials_result <- max_trials %>% filter(maxTrialCount <= 6)
num_rows_max_trials_result <- nrow(max_trials_result)
print(num_rows_max_trials_result)
#96/101, ~95.04% of participants completed the drag and drop task within 6 trials


filled_out_attempts_per_trial <- expand_grid(
  subject = unique(attempts_per_trial$subject),
  trialCount = c(1:10)
) %>% 
  left_join(attempts_per_trial, by=c("subject", "trialCount")) %>% 
  mutate(maxAttempts = ifelse(is.na(maxAttempts), 1, maxAttempts))

#find overall average max attempts
overall_avg_max <- filled_out_attempts_per_trial %>% 
  group_by(trialCount) %>% 
  summarise(mean_attempt_count = mean(maxAttempts))

p <- ggplot(filled_out_attempts_per_trial, aes(x = trialCount, y=maxAttempts, color=subject)) +
  geom_line() +
  scale_x_continuous(breaks=seq(0, 10, by=2)) +
  xlab("Trial Count") +
  ylab ("Maximum Trial Attempts") +
  theme(legend.position = "none")

#add individual subject traces to plot
for (sub in unique(filled_out_attempts_per_trial$subject)){
  df <- filter(filled_out_attempts_per_trial, subject == sub)
  p <- p + geom_line(data = df, aes(x = trialCount, y = maxAttempts))
}

#add overall average line
p <- p + geom_line(data=overall_avg_max, aes(x = trialCount, y = mean_attempt_count), size=2, color="black")


p
ggsave("plots/dragTask/learning_trajectories.jpg", p)