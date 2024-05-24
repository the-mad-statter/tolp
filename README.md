
<!-- README.md is generated from README.Rmd. Please edit that file -->

# tolp <img src="man/figures/logo.png" align="right" width="125px" />

<!-- badges: start -->

[![Project Status: WIP - Initial development is in progress, but there
has not yet been a stable, usable release suitable for the
public.](https://www.repostatus.org/badges/latest/wip.svg)](https://www.repostatus.org/#wip)
[![](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![](https://img.shields.io/github/last-commit/the-mad-statter/tolp.svg)](https://github.com/the-mad-statter/tolp/commits/main)
[![License: GPL (\>=
3)](https://img.shields.io/badge/license-GPL%20(%3E=%203)-blue.svg)](https://cran.r-project.org/web/licenses/GPL%20(%3E=%203))
<br /> [![R build
status](https://github.com/the-mad-statter/tolp/workflows/style/badge.svg)](https://github.com/the-mad-statter/tolp/actions)
[![R build
status](https://github.com/the-mad-statter/tolp/workflows/lint/badge.svg)](https://github.com/the-mad-statter/tolp/actions)
[![R build
status](https://github.com/the-mad-statter/tolp/workflows/test-coverage/badge.svg)](https://github.com/the-mad-statter/tolp/actions)
[![](https://codecov.io/gh/the-mad-statter/tolp/branch/main/graph/badge.svg)](https://codecov.io/gh/the-mad-statter/tolp)
[![R build
status](https://github.com/the-mad-statter/tolp/workflows/r-cmd-check/badge.svg)](https://github.com/the-mad-statter/tolp/actions)
<!-- badges: end -->

## Overview

The goal of `tolp` is to provide a locally hosted Shiny web application
to extract numerical data from plot images.

<br />

## Installation

You can install `tolp` from
[GitHub](https://github.com/the-mad-statter/tolp) with:

``` r
pak::pkg_install("the-mad-statter/tolp")
```

If necessary `pak` can be installed with:

``` r
install.packages("pak", repos = sprintf("https://r-lib.github.io/p/pak/stable/%s/%s/%s", .Platform$pkgType, R.Version()$os, R.Version()$arch))
```

<br />

## Code of Conduct

Please note that the tolp project is released with a [Contributor Code
of
Conduct](https://contributor-covenant.org/version/2/0/CODE_OF_CONDUCT.html).
By contributing to this project, you agree to abide by its terms.

<br />

## Code Style

This package attempts to follow the [tidyverse style
guide](https://style.tidyverse.org/index.html).

The use of [{styler}](https://github.com/r-lib/styler) and
[{lintr}](https://github.com/r-lib/lintr) are recommended.

<br />

## About

### Washington University in Saint Louis <img src="man/figures/brookings_seal.png" align="right" width="125px"/>

Established in 1853, [Washington University in Saint
Louis](https://www.wustl.edu) is among the world’s leaders in teaching,
research, patient care, and service to society. Boasting 24 Nobel
laureates to date, the University is ranked 7th in the world for most
cited researchers, received the 4th highest amount of NIH medical
research grants among medical schools in 2019, and was tied for 1st in
the United States for genetics and genomics in 2018. The University is
committed to learning and exploration, discovery and impact, and
intellectual passions and challenging the unknown.
