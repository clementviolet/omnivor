
<!-- README.md is generated from README.Rmd. Please edit that file -->

# omnivor

[![Travis build
status](https://travis-ci.org/clementviolet/omnivor.svg?branch=master)](https://travis-ci.org/clementviolet/omnivor)
[![lifecycle](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)

The goal of omnivor is to set up a friendly-user environement for
networks analysis in ecology.

## Installation

You can install the released version of omnivor from github with:

``` r
# install.packages("devtools")
devtools::install_github("clementviolet/omnivor")
```

## Licence

This project is licensed under the GPL-3 Licence - see the
[LICENSE.md](LICENCE.md) file for details.

## To do

Package structure :

  - [ ] Adopt a POO structure ;
  - [ ] Use `pkgdown`
  - [ ] Create a vignette.

List of metrics to be added first (Dunne (2009) and, Vermaat *et al.*
(2009)) :

  - [x] Connectance ;
  - [ ] Connectance (bipartite network)
  - [x] Species richness (number of node) ;
  - [x] Linkage density ;
  - [x] Clustering coefficient ;
  - [x] Characteristic path length ;
  - [x] Mean food chain length ;
  - [x] Prey-averaged trophic level ;
  - [x] Shortest trophic level ;
  - [x] Short-weighted trophic level averaged ;
  - [x] Percentage of top species in a web ;
  - [x] Percentage of basal species in a web ;
  - [x] Percentage of intermediate species ;
  - [x] Percentage of canibal species in a web ;
  - [x] Percentage of omnivores in a web ;
  - [x] Normalized standard deviation of generality ;
  - [x] Normalized standard deviation of vulnerability ;
  - [x] Normalized standard deviation of links ;
  - [ ] Mean across taxa of the maximum trophic similarity of each taxon
    to other taxa

List of other metrics to include (Delmas *et al.* (2019)) :

  - [ ] To be continued…

Others :

  - [ ] Other trophic length metrics (Wiliams and Martinez (2004))
