# TEAM-SIZES.R
#
# This script plots the mean team size overall, and among papers with at least
# one male author and at least one female author
#
# Ben Davies
# March 2022

# Compute mean team sizes by year
keys = c('All', '1+ female author', '1+ male author')
team_sizes = teams %>%
  {bind_rows(
    mutate(., Papers = keys[1]),
    mutate(filter(., women > 0), Papers = keys[2]),
    mutate(filter(., men > 0), Papers = keys[3])
  )} %>%
  mutate(Papers = factor(Papers, keys)) %>%
  group_by(Year = year, Papers) %>%
  summarise(mean = mean(total),
            ci = get_bootstrap_ci(total),
            mean_ex_solo = mean(total[total > 1])) %>%
  ungroup() %>%
  unnest('ci')

# Create plot
p = team_sizes %>%
  ggplot(aes(Year, mean, group = Papers)) +
  geom_line(aes(lty = Papers)) +
  labs(x = 'Publication year',
       y = 'Mean team size') +
  guides(lty = guide_legend(title.hjust = 1, label.position = 'left')) +
  theme(legend.justification = c(1, 0),
        legend.position = c(1, 0))
p
ggsave('figures/team-sizes.pdf', width = 6, height = 3)
ggsave('figures/team-sizes.jpeg', width = 6, height = 3, dpi = 400)

# Create plot with 95% confidence intervals
p + geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper), alpha = 0.2)
ggsave('figures/team-sizes-ci.pdf', width = 6, height = 4)

# Save plotted data
team_sizes %>%
  mutate_if(is.double, round, 2) %>%
  write_csv('figures/team-sizes.csv')
