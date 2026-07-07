# Direct admission to a level I trauma center and mortality

This repository contains the R code used for the analysis of:

"Direct admission of severely injured patients to a level I trauma center on mortality: a nationwide analysis"

## Data source

This study used data from the Dutch National Trauma Registry (DNTR).

Patient-level data cannot be publicly shared due to privacy regulations.

## Requirements

R version 4.3.3

Required packages:

- tidyverse
- micemd
- ivtools
- rms
- tableone
- geosphere

## Analysis workflow

The analysis consists of:

1. Data cleaning
2. Multiple imputation
3. Construction of the differential distance instrumental variable
4. Descriptive analyses
5. Instrumental variable analyses
6. Sensitivity analyses

## Running the analysis

Execute:

scripts/run_all_analysis.R

after obtaining access to the DNTR dataset.
