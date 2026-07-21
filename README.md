# COPUS Segmentation Framework

This repository contains code and de-identified data associated with the manuscript:

**From Classroom Behaviors to Instructional Segments: Automated Detection of Instructional Activities Using COPUS Observations**

## Overview

This project provides a segmentation framework for identifying instructional practices from COPUS classroom observation data. The framework uses rule-based detectors and residual clustering analysis to transform interval-level COPUS behaviors into consecutive instructional segments.

The repository supports two related workflows:

1. Reproducing the full manuscript analysis
2. Applying the completed segmentation framework to one classroom observation through a standalone function or live demo

## Repository structure

- `data/`: de-identified COPUS dataset
- `scripts/`: R scripts for detector implementation, clustering analysis, labeling, visualization, and live segmentation
- `outputs/`: generated intermediate outputs and summary tables
- `figures/`: manuscript figures generated from the analysis

The scripts are organized as follows:

```text
scripts/
├── 00_setup.R
├── 01_primary_secondary_detectors.R
├── 02_label_precedence_residual.R
├── 03_pam_clustering_analysis.R
├── 04_tertiary_detectors.R
├── 05_label_tertiary_coverage.R
├── 06_data_visualization.R
├── 07_detect_segments_function.R
└── 08_live_demo.R
```

Scripts `00`–`06` reproduce the manuscript analyses. Script `07` provides a standalone function for applying the complete segmentation framework to one classroom observation. Script `08` provides a live-demo interface for selecting and segmenting one of the de-identified observations.

## Data

The dataset has been de-identified prior to public sharing. Original session identifiers have been replaced with anonymized IDs.

The public dataset is located at:

```text
data/copus_public_deidentified_dataset.csv
```

It contains 1,876 classroom observations and 54,927 two-minute COPUS intervals.

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

The standalone segmentation function and live demo require only:

- dplyr
- readr
- purrr
- tibble
- tidyr

## Reproduce the manuscript analysis

Run the scripts in the following order:

1. `scripts/00_setup.R`
2. `scripts/01_primary_secondary_detectors.R`
3. `scripts/02_label_precedence_residual.R`
4. `scripts/03_pam_clustering_analysis.R`
5. `scripts/04_tertiary_detectors.R`
6. `scripts/05_label_tertiary_coverage.R`
7. `scripts/06_data_visualization.R`

These scripts reproduce the detector outputs, precedence-based interval labels, residual clustering analyses, tertiary labels, coverage summaries, and manuscript figures.

## Run the standalone segmentation function

The complete rule-based segmentation framework is implemented in:

```text
scripts/07_detect_segments_function.R
```

The function accepts COPUS data from one classroom observation and applies:

1. Primary and secondary activity detectors
2. Primary/secondary precedence rules
3. Tertiary detectors on residual unlabeled intervals
4. Tertiary precedence rules
5. Consecutive-interval collapsing to produce final instructional segments

From the repository root directory, run:

```r
library(dplyr)
library(readr)

source("scripts/07_detect_segments_function.R")

copus_data <- readr::read_csv(
  "data/copus_public_deidentified_dataset.csv",
  show_col_types = FALSE
)

dat <- copus_data %>%
  dplyr::filter(id == "Session_0325")

result <- detect_segments(dat)

result$segments
```

The primary output contains one row for each consecutive instructional segment:

```text
Segment_ID   Start   End   Label
1            1       4     InstructorQA
2            5       6     Lecture
3            7       7     Unlabeled
```

Additional outputs include:

- `result$intervals`: final label assigned to every COPUS interval
- `result$display_table`: final segments and lower-priority alternative labels
- `result$alternatives`: detailed labels masked by the precedence rules
- `result$candidate_segments`: all original detector outputs
- `result$candidate_intervals`: all interval-level detector candidates
- `result$segment_details`: final segments with interval counts and durations

## Run the live segmentation demo

`scripts/08_live_demo.R` provides a simple interface for selecting and segmenting one of the 1,876 de-identified classroom observations.

From the repository root directory, run:

```r
source("scripts/08_live_demo.R")
```

The script loads the dataset once and assigns each classroom observation a stable number from 1 to 1,876 based on its de-identified session ID.

To segment an observation selected by number:

```r
demo <- run_demo_observation(325)
```

This prints the final consecutive instructional segments and the detected segment timeline graph, and stores the complete output in `demo`.

Useful outputs include:

```r
demo$segments
demo$raw_copus
demo$labeled_copus
demo$display_table
demo$alternatives
demo$result
demo$plot
```

To display final segments together with lower-priority alternative labels:

```r
demo <- run_demo_observation(
  325,
  show_alternatives = TRUE
)
```

To view part of the observation-number lookup table:

```r
show_demo_index(1, 20)
```

To randomly select an observation:

```r
random_number <- sample_demo_number()
demo <- run_demo_observation(random_number)
```

## Validation

The standalone `detect_segments()` function was validated against the original multi-script analysis pipeline.

Final interval labels were compared across all:

- 1,876 de-identified classroom observations
- 54,927 COPUS intervals

The standalone function and the original pipeline produced identical final labels for all 54,927 intervals.

## Reproducibility

All analyses were conducted in R. Scripts `00`–`06` are organized sequentially to reproduce the manuscript workflow, while scripts `07` and `08` provide a faster standalone implementation for applying and demonstrating the completed segmentation framework.

## Citation

Citation information will be added after publication.
