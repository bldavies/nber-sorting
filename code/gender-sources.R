# GENDER-SOURCES.R
#
# This script tabulates the number of male and female authors identified using
# each data source.
#
# Ben Davies
# March 2022

# Create table
authors %>%
  mutate(source = pmax(0, female_source),
         female = replace(female, female == -1, NA)) %>%
  count(female, source) %>%
  mutate(female = c('Male', 'Female')[female + 1],
         female = replace(female, is.na(female), 'Unknown'),
         source = c('Manual', 'SSA', 'Facebook')[source + 1]) %>%
  bind_rows(mutate(., female = 'Total')) %>%
  count(Gender = female, source, wt = n) %>%
  spread(source, n, fill = 0) %>%
  select(Gender, SSA, Facebook, Manual) %>%
  slice(1:2, 4, 3) %>%
  mutate(Total = rowSums(select(., where(is.numeric)))) %>%
  mutate_if(is.numeric, ~ifelse(. == 0, '-', format(., big.mark = ','))) %>%
  kable(align = 'lrrrr', format = 'latex', booktabs = T, linesep = '') %>%
  row_spec(3, hline_after = T) %>%
  write_lines('tables/gender-sources.tex')
