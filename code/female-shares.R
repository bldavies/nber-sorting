# FEMALE-SHARES.R
#
# This script tabulates the percentage of females identified as females in
# each research program and decade.
#
# Ben Davies
# January 2022

# Count authors by program and sex
decade_sex_counts = papers %>%
  left_join(paper_programs) %>%
  left_join(paper_authors) %>%
  left_join(authors) %>%
  mutate(Decade = paste0(floor(year / 10) * 10, 's'),
         female = ifelse(female < 0, NA, female),
         sex = case_when(female == 0 ~ 'Male',
                         female == 1 ~ 'Female',
                         T ~ 'Unknown')) %>%
  {bind_rows(., mutate(., Decade = 'Overall'))} %>%
  distinct(Decade, program, author, sex) %>%
  left_join(programs) %>%
  mutate(Program = if_else(!is.na(program), program_desc, 'None')) %>%
  {bind_rows(., mutate(., Program = 'Overall'))} %>%
  {bind_rows(., mutate(., subfield = 'Overall'))} %>%
  mutate(subfield = replace(subfield, is.na(subfield), 'None'),
         Program = replace(Program, subfield == 'Overall', 'Overall')) %>%
  distinct(subfield, Program, Decade, author, sex) %>%
  count(Subfield = subfield, Program, Decade, sex) %>%
  spread(sex, n, fill = 0) %>%
  mutate(Total = Female + Male + Unknown)

# Create table
decade_sex_counts %>%
  mutate(share = 100 * Female / (Female + Male)) %>%
  select(Subfield, Program, Decade, share) %>%
  spread(Decade, share) %>%
  arrange(Subfield == 'Overall', Subfield, Program == 'Overall', -Overall) %>%
  filter(Subfield != 'None') %>%
  mutate(Subfield = replace(Subfield, Subfield == lag(Subfield), ''),
         Program = replace(Program, Subfield == 'Overall', '')) %>%
  mutate_if(is.double, function(x) ifelse(is.na(x), '-', sprintf('%.1f', x))) %>%
  kable(align = 'llrrrrrr', format = 'latex', booktabs = T, linesep = '') %>%
  {strsplit(., '\n')[[1]]} %>%
  {c(.[1:3],
     ' & & \\multicolumn{6}{c}{\\% unique authors estimated as female}\\\\', '\\cmidrule(lr){3-8}', .[4:5],
     .[6:11], '\\cmidrule(lr){2-8}', .[12], '\\midrule',
     .[13:26], '\\cmidrule(lr){2-8}', .[27], '\\midrule',
     .[28:30])} %>%
  write_lines('tables/female-shares.tex')
