
<!-- README.md is generated from README.Rmd. Please edit that file -->

# EnHub <img src="man/figures/logo.png" align="right" height="138" />

<!-- badges: start -->
<!-- badges: end -->

This platform integrates the strengths of pre-existing **housing stock
energy models**, notably their delineation of stock composition and
attribution, albeit in a simplified form. It incorporates
functionalities to quantify uncertainties and assess the potential
consequences of policies and strategies aimed at
<span style="font-weight:bold;color:#ff5ea1-size:16px;line-height:1 ">decarbonising</span>
the UK housing stock.

## Features

- Development of an open-access modular platform

- Generation of explicit volumetric archetypes based on survey data

- Energy peformance evaluation using dynamic simulation (i.e. of
  envelope properties and household variables)

- Standardisation of data and modelling algorithms

- Housing stock evaluation

- Analysis of drivers of decision-making regarding energy use intensity

- Analysis of strategies at scale

- Sensitivity analysis and multi-layered parametrisation

- Decoupled processes, enabling the ability to make use of
  `High Performance Computing` facilities.

## System requirements

<p align="left">
<a href="https://cran.r-project.org/" target="_blank"><img src="https://img.shields.io/badge/R-4.4.2-blue.svg"></a>
<a href="https://energyplus.net/downloads" target="_blank"><img src="https://img.shields.io/badge/Energy_Plus-24.1.0-green.svg"></a>
</p>

## Installation

You can install the development version of EnHub from
[GitHub](https://github.com/) with:

``` r
devtools::install_github("EnHub-UK/enhub", build_vignettes = TRUE)
```

It may require a *token*. Define that *token* in `.Renviron`, which
contains environment variables to be set in R sessions. Once
defined—under an arbitrary name (eg. `GITHUBTOKEN`), the *token* needs
to appear when calling `Sys.getenv()`.

``` r
# To display all the environment variables
Sys.getenv()

# To display only the token value
Sys.getenv(c("GITHUBTOKEN"))
```

Use the argument `auth_token` in the installation command:

``` r
devtools::install_github(
  "EnHub-UK/enhub", 
  build_vignettes = TRUE, 
  auth_token = Sys.getenv("GITHUBTOKEN"))
```

## Caveats

Due to the licenses of the the main data sources: **EHS** and
**UK-TUS**, the related datasets are currently excluded from the
R-project. These include: `s_ehs_2011_ext`, `s_ehs_2011_hhd`,
`s_ehs_2011_rom`, `s_ehs_ml`, `s_tus` and `s_tus_ehs`, which are
required as lists for the package to work.

The datasets may available upon request to the authors of the package.

The instructions to generate these files, using the companion
repositories, are available in the vignettes.

## Setup

Load the library

``` r
library(enhub)
```

## Usage

See the **vignettes** in the *documentation*

## References and further reading

:black_small_square: G. Sousa, B.M. Jones, P.A. Mirzaei, D. Robinson,
Platform for dynamic national housing stock simulation to evaluate
decarbonisation scenarios. In Building Simulation 2017.

:black_small_square: G. Sousa, B.M. Jones, P.A. Mirzaei, D. Robinson, An
open-source simulation platform to support the formulation of housing
stock decarbonisation strategies, Energy and Buildings (2018) 459-477.
<doi:http://dx.doi.org/10.1016/j.enbuild.2018.05.015>

:black_small_square: G. Sousa, D. Robinson, Enhanced EnHub: dynamic
simulation of housing stock energy systems, Journal of Building
Performance Simulation (2020) - 13(5), 516–531.
<doi:https://doi.org/10.1080/19401493.2020.1788641>.

## License

<a rel="license" href="http://creativecommons.org/licenses/by-nc-sa/4.0/"><img alt="Creative Commons License" style="border-width:0" src="https://i.creativecommons.org/l/by-nc-sa/4.0/88x31.png" /></a><br /><span xmlns:dct="http://purl.org/dc/terms/"
property="dct:title">EnHub-UK</span> is licensed under a
<a rel="license" href="http://creativecommons.org/licenses/by-nc-sa/4.0/">Creative
Commons Attribution-NonCommercial-ShareAlike 4.0 International
License</a>. You can also read the full terms in
[LICENSE.md](LICENSE.md)

## Authors

G Sousa – [github-author](https://github.com/guxsousa)
