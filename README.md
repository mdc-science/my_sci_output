# Scientific Output Analysis and Visualization

This repository is dedicated to managing and analyzing my scientific output. It includes various R scripts that process data stored in CSV files, calculate metrics, and generate data visualizations related to my publications.

## Features

**Currently working with manually gathered data stored in CSV files. Optimizing code and reducing clutter is in progress.**

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
data.table, ggpmisc, ggpubr, ggrepel, ggsci, ggthemes, here, lubridate, plyr, RColorBrewer, readr, tidyverse
```

Make sure to install these packages before running the scripts.

## Directory Structure

- `data/`: Contains the CSV files used for data analysis, such as `sci_outuput_database.csv`, `journal_rankings.csv`, `wos_savedrecs.csv`, and `scopus.csv`.
- `R/`: Contains R scripts for data visualization (`plotting_sci_output.R`), generating summaries (`sum_tb_scimago_quartile.R`, `sum_tb_capes_qualis.R`), and creating word clouds (`wordcloud_authors.R`, `wordcloud_keywords.R`, `wordcloud_indexed_keywords.R`).
- `utils/`: Contains utility scripts, such as the one for retrieving the h-index from Google Scholar (`getting_h-index_google_scholar.R`).
- `plot/`: Stores the generated plots in PNG format.
- `table/`: Stores the generated tables in PNG format.
- `wordcloud/`: Stores the generated wordclouds in PNG format.

## Usage

1. **Data Loading and Processing**:
   - Use the provided R scripts to load the data from CSV files into the R environment.
   - The data is merged and cleaned for analysis, with special attention given to handling missing values and ensuring consistency.

2. **Data Visualization**:
   - The scripts create various plots and tables to provide insights into the publication data.
   - Examples of visualizations include the number of publications per year, cumulative publications, authorship position, and journal impact factors.

3. **Output**:
   - The generated plots and tables are saved in the `plot/`, `table/` and `wordcloud/` directories, respectively, with file names including the date of generation and the period covered by the data.
  
## Contributing

Contributions and suggestions are welcome! Feel free to fork the repository and submit pull requests.
