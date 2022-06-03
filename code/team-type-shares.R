# TEAM-TYPE-SHARES.R
#
# This script plots the share of all-female, all-male, and mixed co-authorship
# teams by publication year.
#
# Ben Davies
# March 2022

# Compute team type frequencies by publication year
team_type_shares = team_types %>%
  filter(type != 'Unknown') %>%
  count(Year = year, Type = type) %>%
  add_count(Year, wt = n, name = 'n_tot') %>%
  mutate(`% of teams` = 100 * n / n_tot) %>%
  select(Year, Type, Count = n, `% of teams`) %>%
  complete(Year, Type, fill = list(Count = 0, `% of teams` = 0))

# Create plot
team_type_shares %>%
  ggplot(aes(Year, `% of teams`)) +
  geom_line(aes(lty = Type)) +
  labs(x = 'Publication year',
       lty = 'Team type') +
  guides(lty = guide_legend(title.hjust = 1, label.position = 'left')) +
  scale_linetype_manual(values = c('dotted', 'dashed', 'solid')) +
  theme(legend.justification = c(1, 1),
        legend.position = c(1, 1))
ggsave('figures/team-type-shares.pdf', width = 6, height = 4)

# Save plotted data
team_type_shares %>%
  mutate_if(is.double, round, 2) %>%
  write_csv('figures/team-type-shares.csv')
