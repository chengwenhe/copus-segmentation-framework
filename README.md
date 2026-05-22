# COPUS Segmentation Framework

This repository contains code and de-identified data associated with the manuscript:
  
**From Classroom Behaviors to Instructional Segments: Automated Detection of Instructional Activities Using COPUS Observations**

## Overview

This project provides a segmentation framework for identifying instructional practices from COPUS classroom observation data. The framework uses rule-based detectors and residual clustering analysis to transform COPUS behaviors into instructional segments.

## Repository structure

- `data/`: de-identified COPUS dataset
- `scripts/`: R scripts for detector implementation, clustering analysis, labeling, and visualization
- `outputs/`: generated intermediate outputs and summary tables
- `figures/`: manuscript figures generated from the analysis

## How to run the analysis

Run the scripts in the following order:
  
1. `scripts/00_setup.R`
2. `scripts/01_primary_secondary_detectors.R`
3. `scripts/02_label_precedence_residual.R`
4. `scripts/03_pam_clustering_analysis.R`
5. `scripts/04_tertiary_detectors.R`
6. `scripts/05_label_tertiary_coverage.R`
7. `scripts/06_data_visualization.R`

## Data

The dataset has been de-identified prior to public sharing. Session identifiers have been replaced with anonymized IDs.

## Requirements

This project uses R and the following packages:
  
- readr
- stringr
- tidyverse
- dplyr
- purrr
- tibble
- tidyr
- ggplot2
- forcats
- scales
- patchwork
- cluster   
- mclust
- NbClust

## Reproducibility

All analyses were conducted in R. Scripts are organized sequentially to reproduce the segmentation workflow, clustering analyses, coverage summaries, and manuscript figures.

## Citation

Citation information will be added after publication.
