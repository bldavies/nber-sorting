# nber-sorting

This repository contains replication code for "[Sex-based sorting among economists: Evidence from the NBER](https://doi.org/10.31235/osf.io/zeb7a)."

## Workflow

I generate figures and tables by running `make` in a Terminal window at the repository's top-level directory.
(This is equivalent to running [`code/index.R`](code/index.R) in an `nber-sorting.Rproj` instance within [RStudio](https://www.rstudio.com/).)
[`logs/index.txt`](logs/index.txt) provides R session information for the latest run.

## Dependencies

My analysis uses several R packages.
I identify these packages at the beginning of `code/index.R` and in `logs/index.txt`.
All dependencies can be installed by running

```r
install.packages(c('tidyverse', 'igraph', 'kableExtra', 'nberwp', 'remotes', 'sessioninfo'))
remotes::install_github('bldavies/bldr')
```

at the R console.

## License

[MIT](LICENSE)
