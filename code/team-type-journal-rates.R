# TEAM-TYPE-JOURNAL-RATES.R
#
# This script plots the share of papers published by all-female, all-male, and
# mixed co-authorship teams that appeared (or will appear) in peer-reviewed
# journals, by working paper publication year.
#
# Ben Davies
# March 2022

# Compute share of papers appearing journals, by year and team type
team_type_journal_rates = team_types %>%
  mutate(journal = ifelse(is.na(outlet), 0, outlet <= 2)) %>%
  group_by(Year = year, Type = type) %>%
  summarise(`% in journals` = mean(100 * journal),
            ci = get_bootstrap_ci(100 * journal)) %>%
  ungroup() %>%
  unnest('ci')

# Create plot
team_type_journal_rates %>%
  filter(Type != 'Unknown') %>%
  filter(!(Year < 1985 & Type != 'All male')) %>%
  ggplot(aes(Year, `% in journals`, group = Type)) +
  geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper), alpha = 0.2) +
  geom_line(aes(lty = Type)) +
  labs(x = 'Working paper publication year',
       lty = 'Team type') +
  scale_linetype_manual(values = c('dotted', 'dashed', 'solid')) +
  theme(legend.justification = c(0, 0),
        legend.position = c(0, 0))
ggsave('figures/team-type-journal-rates.pdf', width = 6, height = 4)

# Save plotted data
team_type_journal_rates %>%
  mutate_if(is.double, round, 2) %>%
  write_csv('figures/team-type-journal-rates.csv')
