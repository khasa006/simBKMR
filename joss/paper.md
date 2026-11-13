---
title: 'simBKMRdata: Correlated Exposure Simulation and Posterior Inclusion Probability Thresholds for Bayesian Kernel Machine Regression'
tags:
  - R
  - environmental epidemiology
  - exposure mixtures
  - simulation
  - Bayesian kernel machine regression
  - variable selection
authors:
  - name: Kazi Tanvir Hasan
    orcid: 0000-0002-7723-4177
    affiliation: 1
affiliations:
  - name: Yale School of Medicine, United States
    index: 1
date: 12 November 2026
bibliography: paper.bib
---

# Summary

Researchers studying combined exposures to environmental chemicals often encounter measurements that are positively skewed, correlated, and different across population subgroups. Bayesian kernel machine regression (BKMR) is one approach for examining potentially nonlinear associations between exposure mixtures and an outcome [@bobb2015]. Evaluating the behavior of these analyses requires simulated exposure data with specified distributions and dependence structures. `simBKMRdata` is an R package for estimating parameters from multivariate observations, generating correlated Gaussian and Gamma exposure data, simulating subgroup-specific datasets, applying transformations, and computing a response-dependent posterior inclusion probability (PIP) threshold. The package provides building blocks for reproducible simulation experiments rather than fitting BKMR models itself.

# Statement of need

Simulation experiments can expose sensitivity to sampling conditions that may be obscured by a single applied analysis. In environmental mixture studies, Gaussian simulations alone may not represent the skewness observed in biomarker concentrations, while subgroup heterogeneity may change marginal distributions and relationships among exposures. Researchers need to vary these features deliberately while retaining a transparent link between estimated sample characteristics and simulation inputs.

The `simBKMRdata` package is intended for statisticians, environmental epidemiologists, and researchers evaluating mixture methods. Its functions cover estimation of group-specific Gaussian and Gamma-related parameters, generation of multivariate synthetic exposures, and simple grouped data assembly. A separate function computes a PIP selection threshold from the response coefficient of variation and sample size, using coefficients from a simulation-calibrated generalized logistic relationship. The threshold is an empirical selection heuristic for the investigated simulation settings; it does **not** guarantee frequentist Type I error control.

Two related methodological studies motivate the software: a study of BKMR sensitivity to data distributions [@hasan_sensitivity] and an accepted study of dynamic PIP thresholds [@hasan_threshold]. They address statistical methods and their evaluation; this paper describes the reusable software interface and its role in research workflows. The manuscript-specific research scripts, saved simulation outputs, and verification notes are preserved separately [@simbkmr_archive].

# State of the field

The R package `bkmr` implements Bayesian kernel machine regression, including posterior summaries for exposure-response relationships and PIPs [@bobb2015]. The `MASS` package supplies multivariate normal generation with `mvrnorm()` [@mass2002], and R's `stats` package supplies distribution functions and quantiles. These are essential building blocks but do not by themselves provide the package's subgroup estimation and simulation interfaces or its particular calibrated PIP rule. Other copula and multivariate simulation software can be used to generate non-Gaussian data; `simBKMRdata` is not presented as a new copula algorithm.

The rationale for maintaining this package separately, instead of extending `bkmr`, is that synthetic-data generation and simulation calibration are distinct from fitting an inferential model. A researcher may use the generated data with `bkmr`, another mixture model, or a benchmarking pipeline. The package offers a focused interface for the specific simulation conditions evaluated in the related methodological work, rather than replacing general-purpose simulation libraries.

# Software design

The package separates parameter estimation from data generation. Functions estimate multivariate moments or Gamma parameters from observed inputs, including by group; simulation functions then use explicit sample sizes, marginal parameters, and dependence matrices to generate synthetic samples. For multivariate Gamma variables, the implementation draws latent Gaussian observations, applies the standard normal cumulative distribution function, and transforms resulting probabilities through marginal Gamma quantiles. This yields specified Gamma marginals while retaining dependence through a Gaussian copula. The supplied matrix describes the latent Gaussian dependence: Pearson correlations after Gamma transformation need not equal its entries. The Gaussian simulation path delegates to `MASS::mvrnorm()`, and the group dispatcher combines samples from separately parameterized groups.

This composition permits parameter estimation and simulation to be tested separately and allows a common workflow across multiple subgroups. It also exposes limitations: users must supply valid model parameters and dependence matrices, and simulation outputs should be checked against target marginal and dependence summaries. The implementation prioritizes readability and standard R package conventions over replacing the numerical routines supplied by established dependencies.

The `calculate_pip_threshold()` function implements the fitted logistic formula using the absolute response coefficient of variation and sample size, or a response vector from which these are computed. The coefficients are supplied as defaults but may be overridden. Its calibration depends on the underlying simulation design and outcome characteristics; it should not be interpreted as a universally validated decision boundary. Package help files, testthat checks, and Quarto vignettes document common usage and expected numerical behavior. The package is distributed through CRAN and developed publicly on GitHub [@simbkmrdata].

# Research impact statement

The package is associated with a program of methodological research examining BKMR under distributional changes and evaluating PIP selection thresholds [@hasan_sensitivity; @hasan_threshold]. The corresponding `simBKMR` research repository contains original analysis scripts and selected data and provides a versioned archival record [@simbkmr_archive]. This separation preserves historical research artifacts while keeping the installable package reusable for other experiments.

A documented audit of saved PIP outputs reproduced the reported threshold-curve coefficients and selection-frequency summaries from the accepted methodological study. That audit does not constitute an independent rerun of the original BKMR Markov chain Monte Carlo simulations. Nor does the existence of corresponding methods papers alone prove that their analyses directly imported the released `simBKMRdata` version. Before submission, the authors will verify and document the precise use of package functions or identifiable precursor implementations in those studies. No external adoption or performance advantage over alternatives is claimed without evidence.

# AI usage disclosure

Generative AI (OpenAI ChatGPT, GPT-6, November 2026) assisted in drafting this JOSS manuscript and related repository documentation, and in preparing proposed continuous-integration configuration. The statistical methods, original package code, and published or accepted research findings predate this preparation. The human authors remain responsible for checking every scientific claim, bibliographic detail, code change, and test result. This draft has not yet been independently validated or approved by all prospective coauthors.

# Acknowledgements

The authors will confirm funding acknowledgements, institutional affiliations, individual software contributions, and the final JOSS author list before submission.

# References
