
<!-- README.md is generated from README.Rmd. Please edit that file -->

# qbgbaApp <img src='man/figures/logo.png' align="right" height="139" />

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

The goal of qbgbaApp is the analysis of data from the yearly reports of
German hospitals as published by the “Gemeinsamer Bundesausschuss”
(GBA).

## Installation

You can install the released version of qbgbaApp from
[Gitlab](https://gitlab.com/klinik-db/qbgbaApp) with:

``` r
remotes::install_.packages_gitlab("qbgbaApp")
```

## Run it

It a `golem`-based shiny app, therefore, you can just start it (if
`shiny` is installed):

``` r
qbgbaApp::run_app()
```

![Screenshot](./man/figures/Screenshot_Analyse_App_01.png)

## Funding

<a href='https://klinik-db.de'><img src='man/figures/BMBF_eng.png' align="left" height="139" /></a>

Sponsored through the Prototype Fund by the German Federal Ministry of
Education and Research from March 2021 to August 2021.
