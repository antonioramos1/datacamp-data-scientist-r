
# Select one poll from which to resample: one_poll
one_poll <- all_polls %>%
  filter(poll == 1) %>%
  select(vote)
  
# Generate 1000 resamples of one_poll: one_poll_boot_30
one_poll_boot_30 <- one_poll %>%
  rep_sample_n(size = 30, replace = TRUE, reps = 1000)

# Compute p-hat for each poll: ex1_props
ex1_props <- all_polls %>% 
  group_by(poll) %>% 
  summarize(prop_yes = mean(vote))
  
# Compute p-hat* for each resampled poll: ex2_props
ex2_props <- one_poll_boot_30 %>%
  summarize(prop_yes = mean(vote))

# Compare variability of p-hat and p-hat*
ex1_props %>% summarize(sd(prop_yes))
ex2_props %>% summarize(sd(prop_yes))


# Resample from one_poll with n = 3: one_poll_boot_3
one_poll_boot_3 <- one_poll %>%
  rep_sample_n(size=3, replace = TRUE, reps = 1000)

# Resample from one_poll with n = 300: one_poll_boot_300
one_poll_boot_300 <- one_poll %>%
  rep_sample_n(size=300, replace = TRUE, reps = 1000)
  
# Compute p-hat* for each resampled poll: ex3_props
ex3_props <- one_poll_boot_3 %>% 
  summarize(prop_yes = mean(vote))
  
# Compute p-hat* for each resampled poll: ex4_props
ex4_props <- one_poll_boot_300 %>% 
  summarize(prop_yes = mean(vote))

# Compare variability of p-hat* for n = 3 vs. n = 300
ex3_props %>% summarize(sd(prop_yes))
ex4_props %>% summarize(sd(prop_yes))


# Recall the variability of sample proportions
ex1_props %>% summarize(sd(prop_yes))
ex2_props %>% summarize(sd(prop_yes))
ex3_props %>% summarize(sd(prop_yes))
ex4_props %>% summarize(sd(prop_yes))

# Create smoothed density curves for all four experiments
ggplot() + 
  geom_density(data = ex1_props, aes(x = prop_yes), col = "black", bw = .1) +
  geom_density(data = ex2_props, aes(x = prop_yes), col = "green", bw = .1) +
  geom_density(data = ex3_props, aes(x = prop_yes), col = "red", bw = .1) +
  geom_density(data = ex4_props, aes(x = prop_yes), col = "blue", bw = .1)



# Compute proportion of votes for Candidate X: props
props <- all_polls %>%
  group_by(poll) %>% 
  summarize(prop_yes = mean(vote))

# Proportion of polls within 2SE
props %>%
  mutate(lower = mean(prop_yes) - 2 * sd(prop_yes),
         upper = mean(prop_yes) + 2 * sd(prop_yes),
         in_CI = prop_yes > lower & prop_yes < upper) %>%
  summarize(mean(in_CI))


# Again, set the one sample that was collected
one_poll <- all_polls %>%
  filter(poll == 1) %>%
  select(vote)
  
# Compute p-hat from one_poll: p_hat
p_hat <- mean(one_poll$vote)

# Bootstrap to find the SE of p-hat: one_poll_boot
one_poll_boot <- one_poll %>%
  rep_sample_n(30, replace = TRUE, reps = 1000) %>%
  summarize(prop_yes_boot = mean(vote))

# Create an interval of plausible values
one_poll_boot %>%
  summarize(lower = p_hat - 2 * sd(prop_yes_boot),
            upper = p_hat + 2 * sd(prop_yes_boot))



# Find the 2.5% and 97.5% of the p-hat values
one_poll_boot %>% 
  summarize(q025_prop = quantile(prop_yes_boot, p = 0.025),
            q975_prop = quantile(prop_yes_boot, p = 0.975))

# Bootstrap t-confidence interval for comparison
one_poll_boot %>%
  summarize(lower = p_hat - 2 * sd(prop_yes_boot),
            upper = p_hat + 2 * sd(prop_yes_boot))



# Recall the bootstrap t-confidence interval
p_hat <- mean(one_poll$vote)
one_poll_boot %>%
  summarize(lower = p_hat - 2 * sd(prop_yes_boot),
            upper = p_hat + 2 * sd(prop_yes_boot))

# Collect a sample of 30 observations from the population
one_poll <- as.tbl(data.frame(vote = rbinom(n = 30, 1, .6)))

# Resample the data using samples of size 300 (an incorrect strategy!)
one_poll_boot_300 <- one_poll %>%
  rep_sample_n(size=300, replace = TRUE, reps = 1000) %>%
  summarize(prop_yes_boot = mean(vote))

# Find the endpoints of the the bootstrap t-confidence interval
one_poll_boot_300 %>%
  summarize(lower = p_hat - 2*sd(prop_yes_boot),
            upper = p_hat + 2*sd(prop_yes_boot))

# Resample the data using samples of size 3 (an incorrect strategy!)
one_poll_boot_3 <- one_poll %>%
  rep_sample_n(size=3, replace = TRUE, reps = 1000) %>%
  summarize(prop_yes_boot = mean(vote)) 

# Find the endpoints of the the bootstrap t-confidence interval 
one_poll_boot_3 %>%
  summarize(lower = p_hat - 2*sd(prop_yes_boot),
            upper = p_hat + 2*sd(prop_yes_boot))


# Collect 30 observations from a population with true proportion of 0.8
one_poll <- as.tbl(data.frame(vote = rbinom(n = 30, size = 1, prob = 0.8)))

# Compute p-hat of new sample: p_hat
p_hat <- mean(one_poll$vote)
one_poll_boot %>%
  summarize(lower = p_hat - 2 * sd(prop_yes_boot),
            upper = p_hat + 2 * sd(prop_yes_boot))

# Resample the 30 observations (with replacement)
one_poll_boot <- one_poll %>%
  rep_sample_n(size=30, replace = TRUE, reps = 1000) %>%
  summarize(prop_yes_boot = mean(vote)) 

# Calculate the bootstrap t-confidence interval
one_poll_boot %>%
  summarize(lower = p_hat - 2 * sd(prop_yes_boot),
            upper = p_hat + 2 * sd(prop_yes_boot))


# Calculate a 95% bootstrap percentile interval
one_poll_boot %>% 
  summarize(q025_prop = quantile(prop_yes_boot, p = 0.025),
            q975_prop = quantile(prop_yes_boot, p = 0.975))

# Calculate a 99% bootstrap percentile interval
one_poll_boot %>% 
  summarize(q005_prop = quantile(prop_yes_boot, p = 0.005),
            q995_prop = quantile(prop_yes_boot, p = 0.995))

# Calculate a 90% bootstrap percentile interval
one_poll_boot %>% 
  summarize(q05_prop = quantile(prop_yes_boot, p = 0.050),
            q95_prop = quantile(prop_yes_boot, p = 0.950))
