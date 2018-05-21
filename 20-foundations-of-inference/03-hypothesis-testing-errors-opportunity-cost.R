# Tabulate the data
opportunity %>%
  select(decision, group) %>%
  table()

# Find the proportion who bought the DVD in each group
opportunity %>%
  group_by(group) %>%
  summarize(buy_prop = mean(decision == "buyDVD"))


# Create a barplot
ggplot(opportunity, aes(x = group, fill = decision)) + 
  geom_bar(position = "fill")


# Data frame of differences in purchase rates after permuting
opp_perm <- opportunity %>%
  rep_sample_n(size = nrow(opportunity), reps = 1000) %>%
  mutate(dec_perm = sample(decision)) %>%
  group_by(replicate, group) %>%
  summarize(prop_buy_perm = mean(dec_perm == "buyDVD"),
            prop_buy = mean(decision == "buyDVD")) %>%
  summarize(diff_perm = diff(prop_buy_perm),
            diff_orig = diff(prop_buy))  # treatment - control


# Histogram of permuted differences
ggplot(opp_perm, aes(x = diff_perm)) + 
  geom_histogram(binwidth = .005) +
  geom_vline(aes(xintercept = diff_orig), col = "red")


# Calculate the p-value
opp_perm %>%
  summarize(mean(diff_perm <= diff_orig))


# Calculate the two-sided p-value
opp_perm %>%
  summarize(mean(diff_perm <= diff_orig)*2)

