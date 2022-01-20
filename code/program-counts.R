# PROGRAM-COUNTS.R
#
# This script tabulates the number of working papers published by research
# program and decade.
#
# Ben Davies
# January 2022

# Create table
papers %>%
  mutate(decade = paste0(floor(year / 10) * 10, 's')) %>%
  left_join(paper_programs) %>%
  add_count(paper) %>%
  {bind_rows(., mutate(., decade = 'Overall'))} %>%
  left_join(programs) %>%
  mutate(Program = if_else(!is.na(program), program_desc, 'None')) %>%
  {bind_rows(., mutate(., Program = 'Total'))} %>%
  count(decade, Program, wt = 1 / n) %>%
  select(Program, decade, n) %>%
  spread(decade, n, fill = 0) %>%
  arrange(Program == 'Total', Program == 'None', -Overall) %>%
  kable(digits = 0, format.args = list(big.mark = ','),
        format = 'latex', booktabs = T, linesep = '') %>%
  row_spec(nrow(programs) + 1, hline_after = T) %>%
  write_lines('tables/program-counts.tex')
