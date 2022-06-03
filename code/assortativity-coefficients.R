# ASSORTATIVITY-COEFFICIENTS.R
#
# This script plots the assortativity coefficient of the co-authorship network
# and its subfield subnetworks.
#
# Ben Davies
# March 2022

# Define function for computing assortativity coefficient, and confidence
# interval under gender- and degree-preserving randomization
get_assortativity = function(G, boot_reps = 200, alpha = 0.05, seed = NULL) {
  if (is.null(seed)) seed = 0
  set.seed(seed)
  f = V(G)$name %in% women
  d = degree(G)
  boot = sapply(1:boot_reps, function(i) {
    assortativity(sample_degseq(d), f)
  })
  tibble(
    Actual = assortativity(G, f),
    CI_lower = quantile(boot, alpha / 2, names = F),
    CI_upper = quantile(boot, 1 - alpha / 2, names = F)
  )
}

# Compute assortativity coefficients of full network and subfield subnetworks
assortativity_coefficients = subfield_nets %>%
  select(Subfield = subfield, i, known_net) %>%
  bind_rows(
    tibble(Subfield = 'All', i = seq_along(years)) %>%
      mutate(known_net = map(i, ~known_nets[[.]]))
  ) %>%
  mutate(Year = years[i],
         res = map(known_net, get_assortativity)) %>%
  unnest('res') %>%
  select(-i, -known_net)

# Create plot
assortativity_coefficients %>%
  ggplot(aes(Year, Actual)) +
  geom_ribbon(aes(ymin = CI_lower, ymax = CI_upper), alpha = 0.2) +
  geom_line() +
  facet_wrap(~paste(Subfield, 'papers')) +
  labs(y = 'Assortativity coefficient')
ggsave('figures/assortativity-coefficients.pdf', width = 6, height = 3)
ggsave('figures/assortativity-coefficients.jpeg', width = 6, height = 3, dpi = 400)

# Save plotted data
assortativity_coefficients %>%
  mutate_if(is.double, round, 2) %>%
  write_csv('figures/assortativity-coefficients.csv')
