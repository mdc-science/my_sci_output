# Scientific Output Management and Analysis

This repository is dedicated to managing and analyzing my scientific output. It includes various R scripts that process data stored in CSV files, calculate metrics, and generate data visualizations related to my publications.

## Features

**Currently working almost exclusively with manually gathered data saved in CSV files.**

- **Data Import and Processing**: The scripts load data from local CSV files and external sources (e.g., Google Scholar) to maintain an up-to-date record of my scientific output.

- **Data Visualization**: The repository contains scripts to create various plots and tables that summarize the publication data, including:
  - Number of publications per year.
  - Authorship positions in different publications.
  - Impact factors of journals.
  - Distribution of publications across publishers, issue types, and document types.
  - Citations from Web of Science and Scopus databases.
  - Quartile rankings and Scimago area classifications of journals.
  - Wordclouds for co-authors and keywords.
  - Qualis CAPES, a Brazilian journal classification system.

## Dependencies

This project uses the following R packages:

```r
pacman::p_load(plyr, ggthemes, tidyverse, here, readxl, ggsci, ggpmisc, ggforce, scales, lubridate, ggrepel, ggpubr)
```