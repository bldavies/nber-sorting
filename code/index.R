# INDEX.R
#
# This script generates figures and tables presented in the paper.
#
# Ben Davies
# January 2022


# Initialization ----

# Load packages
library(bldr)
library(dplyr)
library(ggplot2)
library(igraph)
library(kableExtra)
library(knitr)
library(purrr)
library(readr)
library(tidyr)

# Set ggplot theme
set_ggtheme()

# Suppress dplyr group_by messages
options(dplyr.summarise.inform = F)

# Isolate data on papers published between 1973 and 2019
papers = nberwp::papers %>%
  filter(year <= 2019)
paper_authors = nberwp::paper_authors %>%
  filter(paper %in% papers$paper)
paper_programs = nberwp::paper_programs %>%
  filter(paper %in% papers$paper)
authors = nberwp::authors %>%
  filter(author %in% paper_authors$author)
programs = nberwp::programs %>%
  filter(program %in% paper_programs$program) %>%
  mutate(subfield = ifelse(program_category %in% c('Finance', 'Macro/International'), 'Macro/Finance', program_category))


# Descriptives ----

source('code/program-counts.R')

source('code/sex-sources.R')

source('code/female-shares.R')

source('code/team-sizes.R')


# Network construction ----

# Set horizon
horizon = 10
years = (min(papers$year) + horizon - 1):max(papers$year)

# Construct paper-author correspondences used to define each network
net_paper_authors = vector('list', length(years))
for (t in seq_along(years)) {
  net_paper_authors[[t]] = papers %>%
    filter(year <= years[t] & year > years[t] - horizon) %>%
    select(paper) %>%
    left_join(paper_authors, by = 'paper') %>%
    add_count(paper)
}

# Define function for extracting edge sets from paper-author correspondences
get_edge_sets = function(d) {
  d %>%
    filter(n > 1) %>%
    {left_join(., ., by = c('paper', 'n'))} %>%
    filter(author.x < author.y) %>%
    group_by(author.x, author.y) %>%
    summarise(weight = sum(1 / (n - 1)),
              repeats = n()) %>%
    ungroup()
}

# Compute node and edge sets, and implied networks
node_sets = map(net_paper_authors, ~unique(.$author))
edge_sets = map(net_paper_authors, get_edge_sets)
nets = map2(edge_sets, node_sets, ~graph_from_data_frame(..1, directed = F, ..2))

# Compute node properties
node_properties = map(seq_along(nets), function(i) {
  tibble(
    author = V(nets[[i]])$name,
    degree = degree(nets[[i]]),
    degree_many = strength(nets[[i]], weights = (E(nets[[i]])$repeats > 1)),
    strength = strength(nets[[i]])
  ) %>%
    left_join(
      net_paper_authors[[i]] %>%
        group_by(author) %>%
        summarise(papers = n(),
                  papers_frac = sum(1 / n)),
      by = 'author'
    ) %>%
    mutate(year = years[i]) %>%
    left_join(authors, by = 'author')
})

# Assign node properties to igraph objects
for (i in seq_along(nets)) {
  tmp = node_properties[[i]] %>%
    slice(which(author == V(nets[[i]])$name))
  V(nets[[i]])$female = tmp$female
  V(nets[[i]])$degree = tmp$degree
  V(nets[[i]])$degree_many = tmp$degree_many
  V(nets[[i]])$papers = tmp$papers
  V(nets[[i]])$papers_frac = tmp$papers_frac
}


# Co-authorship patterns ----

source('code/node-trends.R')

# Identify sets of males and females
males = filter(authors, female == 0)$author
females = filter(authors, female == 1)$author

# Identify subnetworks of males, females, and authors with known sexes
known_nets = map(nets, ~induced_subgraph(., V(.)$name %in% c(males, females)))
male_nets = map(known_nets, ~induced_subgraph(., V(.)$name %in% males))
female_nets = map(known_nets, ~induced_subgraph(., V(.)$name %in% females))

# Construct subfield networks
subfield_nets = programs %>%
  distinct(subfield) %>%
  drop_na() %>%
  crossing(i = seq_along(years)) %>%
  mutate(npa = map2(i, subfield, ~filter(net_paper_authors[[..1]], paper %in% filter(paper_programs, program %in% filter(programs, subfield == ..2)$program)$paper)),
         edge_set = map(npa, get_edge_sets),
         node_set = map(npa, ~unique(.$author)),
         net = map2(edge_set, node_set, ~graph_from_data_frame(..1, directed = F, ..2)),
         net = map(net, function(G) {
           V(G)$female = V(G)$name %in% females
           G
         }),
         known_net = map(net, ~induced_subgraph(., V(.)$name %in% c(males, females))),
         male_net = map(known_net, ~induced_subgraph(., V(.)$name %in% males)),
         female_net = map(known_net, ~induced_subgraph(., V(.)$name %in% females)))

source('code/assortativity-coefficients.R')

source('code/coleman-indices.R')


# Finishing up ----

# Save session info
save_session_info('logs/index.txt')
