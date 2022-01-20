# SEX-SOURCES.R
#
# This script tabulates the number of male and female authors identified using
# each data source.
#
# Ben Davies
# January 2022

# Create table
authors %>%
  mutate(source = pmax(0, sex_source),
         female = replace(female, female == -1, NA)) %>%
  count(female, source) %>%
  mutate(female = c('Male', 'Female')[female + 1],
         female = replace(female, is.na(female), 'Unknown'),
         source = c('Manual', 'SSA', 'Facebook')[source + 1]) %>%
  bind_rows(mutate(., female = 'Total')) %>%
  count(Sex = female, source, wt = n) %>%
  spread(source, n, fill = 0) %>%
  select(Sex, SSA, Facebook, Manual) %>%
  slice(1:2, 4, 3) %>%
  mutate(Total = rowSums(select(., where(is.numeric)))) %>%
  kable(digits = 2, format.args = list(big.mark = ','),
        format = 'latex', booktabs = T, linesep = '') %>%
  row_spec(3, hline_after = T) %>%
  write_lines('tables/sex-sources.tex')
