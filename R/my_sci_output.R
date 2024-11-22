#### Loading packages ####
library(plyr)
library(data.table)
library(here)
library(readr)
library(tidyverse)
library(ggpubr)
library(ggthemes)
library(ggpmisc)
library(ggrepel)
library(ggsci)
library(lubridate)
library(RColorBrewer)
library(forcats)

#### Defining function to read the latest version of a .CSV file ####
source(here("utils", "getting_h-index_google_scholar.R"))

#### Function to save plots ####
save_plot <- function(plot, name, width = 18, height = 18, units = "cm", skip_if_exists = FALSE) {
  # Construct the file path
  file_path <- here("plot", paste0(format(Sys.Date(), "%Y.%m.%d"), "_", period_min, "-", period_max, "_", name, ".png"))
  
  # Check if the plot already exists and the skip_if_exists parameter is TRUE
  if (file.exists(file_path) && skip_if_exists) {
    message(paste("Plot", file_path, "already exists. Skipping save."))
  } else {
    # Save the plot if it doesn't exist or skip_if_exists is FALSE
    ggsave(file_path, plot, width = width, height = height, units = units)
    message(paste("Plot saved as", file_path))
  }
}

#### Retrieving current h-index from Google Scholar ####
h_index_scholar <- get_h_index_google("https://scholar.google.com/citations?user=4MTUwDgAAAAJ")

#### Importing data into the environment ####
df_papers <- fread(here("data", "sci_outuput_database.csv"))
df_journals <- fread(here("data", "journal_rankings.csv"))

# Merging dataframes
df_local <- df_papers %>%
  left_join(df_journals, by = c("journal_abbr", "journal")) %>%
  mutate(across(doi, tolower))

# Finding the latest update date
last_updated <- max(as.Date(df_local$last_updated_output, tryFormats = "%d-%m-%Y"),
  as.Date(df_local$last_updated_ranking, tryFormats = "%d-%m-%Y"),
  na.rm = TRUE
)

#### Importing and Tidying Web of Science Data ####
df_wos <- fread(here("data", "wos_savedrecs.csv"))
df_wos <- df_wos[, c("Authors", "Author Keywords", "Keywords Plus", "Times Cited, All Databases", "DOI", "Pubmed Id")] %>%
  rename_with(~ c("authors_wos", "keywords_auth_wos", "keywords_wos", "citations_wos", "doi", "pmid"), everything()) %>%
  mutate(doi = tolower(doi))

# Calculating h-index based on Web of Science Records
h_index_wos <- df_wos %>%
  arrange(desc(citations_wos)) %>%
  mutate(h = pmin(row_number(), citations_wos)) %>%
  summarise(h_index = max(h)) %>%
  pull(h_index)

#### Importing and Tidying Scopus Data ####
df_scopus <- fread(here("data", "scopus.csv"))

df_scopus <-
  df_scopus[, c("Authors", "Author Keywords", "Index Keywords", "Cited by", "DOI")] %>%
  rename_with(~ c("authors_scopus", "keywords_auth_scopus", "keywords_scopus", "citations_scopus", "doi"), everything()) %>%
  mutate(doi = tolower(doi))

# Calculating h-index based on Scopus Records
h_index_scopus <- df_scopus %>%
  arrange(desc(citations_scopus)) %>%
  mutate(h = pmin(row_number(), citations_scopus)) %>%
  summarise(h_index = max(h)) %>%
  pull(h_index)

#### Merging all data into a single dataframe and creating the truncated title col ####
df_all <- df_local %>%
  left_join(df_wos, by = "doi") %>%
  left_join(df_scopus, by = "doi") %>%
  mutate(title_trunc = str_trunc(title, 35, "center")) %>%
  mutate(across(everything(), ~ ifelse(. == "", NA, .)))

#### Setting year cutoff if needed ####
period_min <- 2012
period_max <- 2024
desired_period <- seq(period_min, period_max, by = 1)

# Filtering for the selected period
df_all <- df_all %>% filter(year %in% desired_period)

#### Plotting ####
source(here("R", "plotting_sci_output.R"))

#### Plotting GG Text Tables ####
source(here("R", "sum_tb_scimago_quartile.R"))
source(here("R", "sum_tb_capes_qualis.R"))
source(here("R", "sum_tb_total.R"))

#### Creating Wordclouds ####
# Co-authors wordcloud
source(here("R", "wordcloud_authors.R"))

# Keywords wordclouds
cloud_color <- "Blues"
source(here("R", "wordcloud_keywords.R"))
source(here("R", "wordcloud_indexed_keywords.R"))
