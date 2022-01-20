# NODE-TRENDS.R
#
# This script plots several trends among authors in the co-authorship network.
#
# Ben Davies
# January 2022

# Compute node property trends
node_trends = node_properties %>%
  bind_rows() %>%
  mutate(Sex = case_when(female == 1 ~ 'Female',
                         female == 0 ~ 'Male',
                         T ~ 'Unknown')) %>%
  bind_rows(mutate(., Sex = 'Any')) %>%
  group_by(Year = year, Sex) %>%
  summarise(`Authors (000s)` = n() / 1e3,
            # `Mean solo papers` = mean(papers - strength),
            `Mean co-authors (degree)` = mean(degree),
            `Mean co-authored papers` = mean(strength),
            `Mean repeat co-authors` = mean(degree_many)) %>%
  ungroup()

# Create plot
node_trends %>%
  filter(Sex != 'Unknown') %>%
  gather(key, value, -Year, -Sex, factor_key = T) %>%
  ggplot(aes(Year, value)) +
  geom_line(aes(lty = Sex)) +
  labs(y = NULL) +
  facet_wrap(~key, scales = 'free_y') +
  scale_linetype_manual(values = c('solid', 'dotted', 'dashed', 'dotdash')) +
  theme(legend.justification = c(0, 1),
        legend.position = c(0, 1))
ggsave('figures/node-trends.pdf', width = 6, height = 4)

# Save plotted data
node_trends %>%
  mutate_if(is.double, round, 2) %>%
  write_csv('figures/node-trends.csv')
