# ==================================================
# COPUS Segmentation Framework
# 00_setup.R
# Purpose: Load required packages and define paths
# ==================================================
# Load packages
library(readr)
library(stringr)
library(tidyverse)
library(dplyr)
library(purrr)
library(tibble)
library(tidyr)
library(ggplot2)
library(forcats)
library(scales)
library(patchwork)
library(cluster)   # pam(), daisy()
library(mclust)    # stability check
library (NbClust)  # supplementary NbClust check

# Define data path
data_path <- "data/copus_public_deidentified_dataset.csv"

# Read dataset
master_data <- read_csv(data_path)

# Preview data
glimpse(master_data)