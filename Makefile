all: analysis

analysis:
	Rscript 'code/index.R'

.PHONY: all analysis
