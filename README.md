# Reproduce the analysis

## 1. Data

Copy the imap datasets into the [data/output/imap](data/output/imap) folder.
 

## 2. Restore the R environment

All R packages in this project are versioned with [renv](https://rstudio.github.io/renv/articles/renv.html).

1.  Open the project file [sus-agri-data-paper.Rproj](sus-agri-data-paper.Rproj) in RStudio.
2.  At the R console, run:

``` r
renv::restore()
```

## 3. Run the analysis

Run the [WORKFLOW-visualize.R](WORKFLOW-visualize.R) script and find the output in the [viz](viz) folder.
