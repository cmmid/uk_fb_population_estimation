# fb_population_paper
Code supporting the paper "Population disruption: estimating population-level movement in the UK during the COVID-19 pandemic
", Gibbs et al.

## Installation

This repository contains source code used in this research paper.

All code was written in `R (3.6.3)`. Individual code files are located in the `src` directory.

Folders correspond to the sections of the analysis as they are presented in the publication:

`0_data`: Data preprocessing.  
`1_population_overview`: Comparison of census population and the number of Facebook users.  
`2_dynamic_population_change`: Comparison time-varying population estimates.  
`3_population_density`: Comparison of population change in population size deciles.  
`4_transmission_impact`: Estimating the effect of population change on transmission models.  

## Documentation

Scripts produce individual figures for the analysis. Each file contains documentation providing information about the function of that script.

## Data Availability

The terms of use of data from the Facebook Data for Good Program prohibit unauthorised distribution. Data is available from the Facebook Data for Good Partner Program by application.

Boundary data for UK administrative geographies is available from the UK Government [Open Geography Portal](https://geoportal.statistics.gov.uk/). Administrative Datasets used in this study:

* Local Authority Districts (2019)
* Built-up Areas (2011)
* Middle-layer Super Output Areas (2011)

Tile boundaries were extracted using the [pyquadkey2](https://pypi.org/project/pyquadkey2/) library.

## Dependencies

This project was written in `R (3.6.3)` and relies on the following R packages:

* `tidyverse (1.3.0)`
* `ggplot2 (3.3.3)`
* `sf (0.9.6)`
* `ggpubr (0.2.5)`
* `deSolve (1.28)`
* `data.table (1.13.2)`
* `here (0.1)`
* `dplyr (1.0.2)`

## Contributions

Have questions or find an issue with this code? Please [open an issue](https://github.com/cmmid/uk_fb_population_estimation/issues/new/choose).
