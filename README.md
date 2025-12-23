# rpairconstrain

<!-- badges: start -->
[![R-CMD-check](https://github.com/VigomezB/Randomly-pairing-two-vectors-under-constraints/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/VigomezB/Randomly-pairing-two-vectors-under-constraints/actions/workflows/R-CMD-check.yaml)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
<!-- badges: end -->


# Randomly-pairing-two-vectors-under-constraints

This repository provides R functions for randomly pairing values from two numeric vectors while enforcing inequality constraints (e.g., one value must be greater than or equal to the other).

The methods support both:
- sampling **with replacement**, and
- sampling **without replacement**.

These tools are particularly useful in applied settings such as tax and income microdata reconstruction from tabulations, where multiple components must be combined randomly while respecting accounting and logical consistency constraints (e.g., upper ≥ lower, or upper + lower ≤ threshold).

These files and functions were created by Leonardo Hansa, Adrian Manzanal Oliva and Víctor M. Gómez-Blanco.

## Installation

Install the development version from GitHub:

```r
# install.packages("devtools")
devtools::install_github(
  "VigomezB/rpairconstrain"
)
```

## Example

This is a basic example which shows you how to solve a common problem:

```r
library(rpairconstrain)

set.seed(123)

# Example vectors
lower <- c(10, 20, 30, 40)
upper <- c(5, 10, 15, 25, 35, 50)

# Pairing with replacement
res_with_replacement <- random_sample_greater(lower, upper)
res_with_replacement

# Pairing without replacement
res_no_replacement <- random_sample_greater_no_replace(lower, upper)
res_no_replacement
