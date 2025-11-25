# MSc AS - Term 1: SMM047 Probability and Mathematical Statistics — Group Project

Term 1 group project for Probability and Mathematical Statistics (50% of
coursework grade).

- Group 07 working directory

## Structure

The directory structure is as follows

```{bash}
.
├── air.toml
├── Coursework Group 7.pdf
├── cs1-group07.ipynb
├── cs1-group07.r
├── cs1-group07.rmd
├── fig
│   ├── Element 1-1.svg
│   ├── ...
└── README.md
```

## Contents

A statistical investigation into the validity of the assumptions underlying
'log-normal random walk model', mathematically described by

$$
\log X_{n} = \log X_{n-1} + Z_{n}
$$

for Nasdaq Composite returns (2018–2024).

This project evaluates the core assumptions (normality, constant mean, constant
variance, and serial independence) using a framework of non-parametric tests
(Kruskal-Wallis, Mann-Whitney U) and robust resampling techniques
(Bias-Corrected and accelerated (BCa) bootstrapping).

## Requirements

- R (≥ 4.x)
- Packages: at minimum **rmarkdown** (others as used in the Rmd)

For R Markdown install core package:

```r
install.packages("rmarkdown")
install.packages("quantmod")
install.packages("summarytools")
```

Additional packages used are

```r
install.packages("quantmod")
install.packages("xts") # for downloading
install.packages("zoo") # for downloading

install.packages("ggplot2")
install.packages("dplyr")
install.packages("tidyr")
install.packages("patchwork")
install.packages("kableExtra")

install.packages("e1071") # statistical tests
install.packages("nortest") # normality test

install.packages("boot") # bootstrapping

# for custom functions
install.packages("clipr") # for banner_comment function qol to annotate code
```

- Optional packages: `htmltools` (required only if rendering to HTML)

## Acknowledgements

- BCa Bootstrapping Algorithm: The project utilizes bcanon.R, an open-source
  implementation of the Bias-Corrected and accelerated (BCa) bootstrap
  algorithm.

  - Source: Scott Kosty (GitLab)
  - Methodology: Efron, B., & Tibshirani, R. J. (1993). An Introduction to the
    Bootstrap.
  - Available via:
    https://gitlab.com/scottkosty/bootstrap/-/blob/master/R/bcanon.R
