# Direct admission to a level I trauma center and mortality

This repository contains the R code accompanying the manuscript:

> **Direct admission of severely injured patients to a level I trauma center and mortality: a nationwide instrumental variable analysis**

## Study overview

This study evaluates the association between direct admission to a level I trauma center and 30-day mortality among severely injured patients in the Netherlands.

To reduce bias from measured and unmeasured confounding, the primary analysis used an instrumental variable (IV) approach with **differential distance** (distance to the nearest level I trauma center minus the distance to the nearest non-level I trauma center) as the instrument.

Missing data were handled using multilevel multiple imputation, accounting for clustering within Dutch trauma regions.

## Data availability

The analyses were performed using data from the **Dutch National Trauma Registry (DNTR)**.

The underlying patient-level data cannot be shared publicly because they contain potentially identifiable health information and are subject to Dutch privacy legislation and data-sharing agreements.

Researchers interested in accessing the data should contact the Dutch National Trauma Registry (DNTR) and obtain the appropriate approvals.

## Software

The analyses were performed in:

- R 4.3.3

## Required R packages

The required packages are installed and loaded in:

```
R/00_packages.R
```

The main packages include:

- tidyverse
- micemd
- ivtools
- rms
- tableone
- geosphere
- splines

## Repository structure

```
├── R/
│   ├── 00_packages.R
│   ├── 01_import_and_clean_data.R
│   ├── 02_geospatial_processing.R
│   ├── 03_multiple_imputation.R
│   ├── 04_baseline_characteristics.R
│   ├── 05_iv_analysis.R
│   ├── 06_conventional_analysis.R
│   └── 07_subgroup_analyses.R
│
├── data/
│   ├── raw/
│   └── processed/
│
├── results/
│
└── README.md
```

## Analysis workflow

The analyses should be run in the following order:

1. **01_import_and_clean_data.R**
   - Import DNTR data
   - Apply inclusion and exclusion criteria
   - Create derived clinical variables

2. **02_multiple_imputation.R**
   - Perform multilevel multiple imputation
   - Account for clustering within trauma regions
   - Create 30 imputed datasets

3. **03_geospatial_processing.R**
   - Link patient postal codes to geographical coordinates
   - Calculate differential distance to trauma centers
   - Construct the instrumental variable

4. **04_baseline_characteristics.R**
   - Generate baseline characteristics
   - Produce descriptive tables

5. **05_iv_analysis.R**
   - Perform the primary instrumental variable analysis
   - Estimate two-stage IV models using cluster-robust standard errors at the trauma region level

6. **06_conventional_analysis.R**
   - Perform unadjusted and adjusted logistic regression analyses

7. **07_subgroup_analyses.R**
   - Prespecified subgroup analyses:
     - Age ≥65 years
     - Traumatic brain injury (AIS Head ≥3)
     - Critical injury (ISS ≥25)

## Corresponding manuscript

If you use this code, please cite the corresponding publication once available.
