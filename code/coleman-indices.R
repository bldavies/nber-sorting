# COLEMAN-INDICES.R
#
# This script plots Coleman indices for men and women in the co-authorship
# network and its subfield subnetworks.
#
# Ben Davies
# March 2022

# Define function for computing Coleman indices
get_coleman_index = function(G) {
  
  # Compute female share
  f = mean(V(G)$female)
  
  # Compute mixing matrix
  A = as_adj(G, sparse = T)
  M = matrix(0, 2, 2)
  for (i in 0:1) {
    for (j in 0:1) {
      M[i + 1, j + 1] = sum(A[which(V(G)$female == i), which(V(G)$female == j)])
    }
  }
  # Note: M[i, j] = # edges to nodes of gender j, summed over nodes of gender i.
  #   Diagonal entries are not "double counted" because each within-gender edge
  #   contributes to # same-gender neighbors of each incident node, and it is
  #   correct to include both nodes' counts when computing mean # such neighbors
  
  # Return results
  tibble(
    Gender = c('Male', 'Female'),
    Share = c(1 - f, f),
    Homophily = diag(M / rowSums(M)),
    `Coleman index` = (Homophily - Share) / (1 - Share)
  )
}

# Compute Coleman indices
coleman_indices = subfield_nets %>%
  select(Subfield = subfield, i, known_net) %>%
  bind_rows(
    tibble(Subfield = 'All', i = seq_along(years)) %>%
      mutate(known_net = map(i, ~known_nets[[.]]))
  ) %>%
  mutate(Year = years[i],
         res = map(known_net, get_coleman_index)) %>%
  unnest('res') %>%
  select(-i, -known_net)

# Generate plot
coleman_indices %>%
  ggplot(aes(Year, `Coleman index`)) +
  geom_line(aes(lty = Gender)) +
  facet_wrap(~paste(Subfield, 'papers')) +
  scale_linetype_manual(values = c('dotted', 'dashed')) +
  guides(lty = guide_legend(title.hjust = 1, label.position = 'left')) +
  theme(legend.justification = c(1, 1),
        legend.position = c(1, 1))
ggsave('figures/coleman-indices.pdf', width = 6, height = 3)
ggsave('figures/coleman-indices.jpeg', width = 6, height = 3, dpi = 400)

# Save plotted datas
coleman_indices %>%
  mutate_if(is.double, round, 2) %>%
  write_csv('figures/coleman-indices.csv')
