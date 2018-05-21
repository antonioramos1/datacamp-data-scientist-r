# Create a contingency table summarizing the data
table(disc)

# Find proportion of each sex who were promoted
disc %>%
  group_by(sex) %>%
  summarise(promoted_prop = mean(promote == "promoted"))


# Sample the entire data frame 5 times
disc %>%
  rep_sample_n(size = nrow(disc), reps = 5) 

# Shuffle the promote variable within replicate
disc %>%
  rep_sample_n(size = nrow(disc), reps = 5) %>%
  mutate(prom_perm = sample(promote)) 

# Find the proportion of promoted in each replicate and sex
disc %>%
  rep_sample_n(size = nrow(disc), reps = 5) %>%
  mutate(prom_perm = sample(promote)) %>%
  group_by(replicate, sex) %>%
  summarize(prop_prom_perm = mean(prom_perm == "promoted"),
            prop_prom = mean(promote == "promoted")) 

# Difference in proportion of promoted across sex grouped by gender
disc %>%
  rep_sample_n(size = nrow(disc), reps = 5) %>%
  mutate(prom_perm = sample(promote)) %>%
  group_by(replicate, sex) %>%
  summarize(prop_prom_perm = mean(prom_perm == "promoted"),
            prop_prom = mean(promote == "promoted"))   %>%
  summarize(diff_perm = diff(prop_prom_perm),
            diff_orig = diff(prop_prom))  # male - female



# Create a data frame of differences in promotion rates
disc_perm <- disc %>%
  rep_sample_n(size = nrow(disc), reps = 1000) %>%
  mutate(prom_perm = sample(promote)) %>%
  group_by(replicate, sex) %>%
  summarize(prop_prom_perm = mean(prom_perm == "promoted"),
            prop_prom = mean(promote == "promoted")) %>%
  summarize(diff_perm = diff(prop_prom_perm),
            diff_orig = diff(prop_prom))  # male - female


# Histogram of permuted differences
ggplot(disc_perm, aes(x = diff_perm)) + 
  geom_histogram(binwidth = 0.01) +
  geom_vline(aes(xintercept = diff_orig), col = "red")



# Find the 0.90, 0.95, and 0.99 quantiles of diff_perm
disc_perm %>% 
  summarize(q.90 = quantile(diff_perm, p = 0.90),
            q.95 = quantile(diff_perm, p = 0.95),
            q.99 = quantile(diff_perm, p = 0.99))


# Find the 0.01, 0.05, and 0.10 quantiles of diff_perm
disc_perm %>% 
  summarise(q.01 = quantile(diff_perm, p = 0.01),
            q.05 = quantile(diff_perm, p = 0.05),
            q.10 = quantile(diff_perm, p = 0.10))


# Tabulate the small and big data frames
disc_small %>% 
  select(sex, promote) %>%
  table()
disc_big %>% 
  select(sex, promote) %>%
  table()

# Plot the distributions of permuted differences
ggplot(disc_small_perm, aes(x = diff_perm)) + 
  geom_histogram(binwidth = 0.01) +
  geom_vline(aes(xintercept = diff_orig), col = "red")

ggplot(disc_big_perm, aes(x = diff_perm)) + 
  geom_histogram(binwidth = 0.01) +
  geom_vline(aes(xintercept = diff_orig), col = "red")



# Recall the quantiles associated with the original dataset
disc_perm %>% 
  summarize(q.90 = quantile(diff_perm, p = 0.90),
            q.95 = quantile(diff_perm, p = 0.95),
            q.99 = quantile(diff_perm, p = 0.99))

# Calculate the quantiles associated with the small dataset
disc_small_perm %>% 
  summarize(q.90 = quantile(diff_perm, p = 0.90),
            q.95 = quantile(diff_perm, p = 0.95),
            q.99 = quantile(diff_perm, p = 0.99))


# Calculate the quantiles associated with the big dataset
disc_big_perm %>% 
  summarize(q.90 = quantile(diff_perm, p = 0.90),
            q.95 = quantile(diff_perm, p = 0.95),
            q.99 = quantile(diff_perm, p = 0.99))


# Calculate the p-value for the original dataset
disc_perm %>%
  summarize(mean(diff_orig <= diff_perm))

# Calculate the p-value for the small dataset
disc_small_perm %>%
  summarize(mean(diff_orig <= diff_perm))

# Calculate the p-value for the big dataset
disc_big_perm %>%
  summarize(mean(diff_orig <= diff_perm))



# Recall the original data
disc %>% 
  select(sex, promote) %>%
  table()

# Tabulate the new data
disc_new %>% 
  select(sex, promote) %>%
  table()

# Plot the distribution of the original permuted differences
ggplot(disc_perm, aes(x = diff_perm)) + 
  geom_histogram() +
  geom_vline(aes(xintercept = diff_orig), col = "red")

# Plot the distribution of the new permuted differences
ggplot(disc_new_perm, aes(x = diff_perm)) + 
  geom_histogram() +
  geom_vline(aes(xintercept = diff_orig), col = "red")


# Find the p-value from the original data
disc_perm %>%
  summarize(mean(diff_orig <= diff_perm))

# Find the p-value from the new data
disc_new_perm %>%
  summarize(mean(diff_orig <= diff_perm))


# Calculate the two-sided p-value
disc_perm %>%
  summarize(mean(diff_orig <= diff_perm)*2)

