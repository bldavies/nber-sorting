# TEAM-SIZES.R
#
# This script plots the mean team size overall, and among papers with at least
# one male author and at least one female author
#
# Ben Davies
# January 2022

# Compute mean team sizes by year
keys = c('All', '1+ female author', '1+ male author')
team_sizes = paper_authors %>%
  left_join(authors) %>%
  mutate(female = ifelse(female < 0, NA, female)) %>%
  count(paper, female) %>%
  mutate(female = case_when(female == 0 ~ 'males',
                            female == 1 ~ 'females',
                            T ~ 'unknown')) %>%
  spread(female, n, fill = 0) %>%
  mutate(total = females + males + unknown) %>%
  left_join(papers) %>%
  {bind_rows(
    mutate(., Papers = keys[1]),
    mutate(filter(., females > 0), Papers = keys[2]),
    mutate(filter(., males > 0), Papers = keys[3])
  )} %>%
  mutate(Papers = factor(Papers, keys)) %>%
  group_by(Year = year, Papers) %>%
  summarise(mean = mean(total),
            mean_ex_solo = mean(total[total > 1])) %>%
  ungroup()

# Create plot
team_sizes %>%
  ggplot(aes(Year, mean)) +
  geom_line(aes(lty = Papers)) +
  labs(x = 'Publication year',
       y = 'Mean team size') +
  guides(lty = guide_legend(title.hjust = 1, label.position = 'left')) +
  theme(legend.justification = c(1, 0),
        legend.position = c(1, 0))
ggsave('figures/team-sizes.pdf', width = 6, height = 3)

# Save plotted data
team_sizes %>%
  mutate_if(is.double, round, 2) %>%
  write_csv('figures/team-sizes.csv')
