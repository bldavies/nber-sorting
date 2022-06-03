# NODE-TRENDS.R
#
# This script plots several trends among authors in the co-authorship network.
#
# Ben Davies
# March 2022

# Compute node property trends
node_trends = node_properties %>%
  bind_rows() %>%
  mutate(Gender = case_when(female == 1 ~ 'Female',
                            female == 0 ~ 'Male',
                            T ~ 'Unknown')) %>%
  bind_rows(mutate(., Gender = 'Any')) %>%
  group_by(Year = year, Gender) %>%
  mutate(`Authors (000s)` = n() / 1e3) %>%
  ungroup() %>%
  select(Year, Gender, `Authors (000s)`,
         `Mean co-authors (degree)` = degree,
         `Mean co-authored papers` = strength,
         `Mean repeat co-authors` = degree_many) %>%
  mutate_if(is.double, as.numeric) %>%
  gather(Series, value, -Year, -Gender, factor_key = T) %>%
  group_by(Year, Gender, Series) %>%
  summarise(mean = mean(value),
            ci = get_bootstrap_ci(value)) %>%
  ungroup() %>%
  unnest('ci')

# Create plot
node_trends %>%
  filter(Gender != 'Unknown') %>%
  select(-starts_with('ci')) %>%
  ggplot(aes(Year, mean)) +
  geom_line(aes(lty = Gender)) +
  labs(y = NULL) +
  facet_wrap(~Series, scales = 'free_y') +
  scale_linetype_manual(values = c('solid', 'dotted', 'dashed', 'dotdash')) +
  theme(legend.justification = c(0, 1),
        legend.position = c(0, 1))
ggsave('figures/node-trends.pdf', width = 6, height = 4)
ggsave('figures/node-trends.jpeg', width = 6, height = 4, dpi = 400)

# Create plot with 95% confidence intervals
node_trends %>%
  filter(Gender != 'Unknown') %>%
  filter(Series != 'Authors (000s)') %>%
  ggplot(aes(Year, mean, group = Gender)) +
  geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper), alpha = 0.2) +
  geom_line(aes(lty = Gender)) +
  labs(y = NULL) +
  facet_wrap(~Series, scales = 'free_y', ncol = 1) +
  scale_linetype_manual(values = c('solid', 'dotted', 'dashed', 'dotdash')) +
  theme(legend.justification = c(0, 1),
        legend.position = c(0, 1))
ggsave('figures/node-trends-ci.pdf', width = 6, height = 8)

# Save plotted data
node_trends %>%
  mutate_if(is.double, round, 2) %>%
  write_csv('figures/node-trends.csv')
