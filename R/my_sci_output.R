#### Loading packages ####
pacman::p_load(here, readr, tidyverse, ggpubr, ggrepel, ggsci, lubridate, RColorBrewer)

#### Defining function to read the latest version of a .CSV file ####
source(here('utils', 'getting_h-index_google_scholar.R'))

#### Retrieving current h-index from Google Scholar ####
h_index_scholar <- get_h_index_google("https://scholar.google.com/citations?user=4MTUwDgAAAAJ")

#### Importing local data into the environment ####
df_papers <- read_csv(here('data', 'sci_outuput_database.csv'))
df_journals <- read_csv(here('data', 'journal_rankings.csv'))

# Merging dataframes
df_local <- merge(df_papers, df_journals, by = c("journal_abbr", 'journal'), all.x = T)

df_local <- df_local %>% mutate(doi = tolower(doi))

last_updated <- max(as.Date(df_local$last_updated_output, tryFormats = "%d-%m-%Y"),      
                    as.Date(df_local$last_updated_ranking, tryFormats = "%d-%m-%Y"), 
                    na.rm = TRUE)

#### Importing and Tidying Web of Science Data ####
df_wos <- read_csv(here('data', 'wos_savedrecs.csv'))
df_wos <- df_wos[,c("Authors", "Author Keywords", "Keywords Plus", "Times Cited, All Databases", "DOI", "Pubmed Id")]

# Renaming cols
colnames(df_wos)[which(names(df_wos) == "Authors")] <- "authors_wos"
colnames(df_wos)[which(names(df_wos) == "Author Keywords")] <- "keywords_auth_wos"
colnames(df_wos)[which(names(df_wos) == "Keywords Plus")] <- "keywords_wos"
colnames(df_wos)[which(names(df_wos) == "Times Cited, All Databases")] <- "citations_wos"
colnames(df_wos)[which(names(df_wos) == "DOI")] <- "doi"
colnames(df_wos)[which(names(df_wos) == "Pubmed Id")] <- "pmid"

df_wos <- df_wos %>% mutate(doi = tolower(doi))

# Calculating h-index based on Web of Science Records
h_index_wos <- df_wos %>%
  arrange(desc(citations_wos)) %>%
  mutate(h = pmin(row_number(), citations_wos)) %>%
  summarise(h_index = max(h)) %>%
  pull(h_index)

#### Importing and Tidying Scopus Data ####
df_scopus <- read_csv(here('data', 'scopus.csv'))

df_scopus <- df_scopus[, c("Authors", "Author Keywords", "Index Keywords", "Cited by", "DOI")]

# Renaming cols
colnames(df_scopus)[which(names(df_scopus) == "Authors")] <- "authors_scopus"
colnames(df_scopus)[which(names(df_scopus) == "Author Keywords")] <- "keywords_auth_scopus"
colnames(df_scopus)[which(names(df_scopus) == "Index Keywords")] <- "keywords_scopus"
colnames(df_scopus)[which(names(df_scopus) == "Cited by")] <- "citations_scopus"
colnames(df_scopus)[which(names(df_scopus) == "DOI")] <- "doi"

df_scopus <- df_scopus %>% mutate(doi = tolower(doi))

# Calculating h-index based on Scopus Records
h_index_scopus <- df_scopus %>%
  arrange(desc(citations_scopus)) %>%
  mutate(h = pmin(row_number(), citations_scopus)) %>%
  summarise(h_index = max(h)) %>%
  pull(h_index)

#### Merging all data into a single dataframe and creating the truncated title col ####
df_all <- df_local %>% left_join(df_wos, by = 'doi') %>%
  left_join(df_scopus, by = 'doi') %>%
  mutate(title_trunc = str_trunc(title, 35, "center"))

df_all <- df_all %>%
  mutate(across(everything(), ~ifelse(. == "", NA, .)))

#### Setting year cutoff if needed ####
period_min <- 2012
period_max <- 2024
desired_period <- seq(period_min, period_max, by = 1)

# Filtering for the selected period
df_all <- df_all %>% filter(year %in% desired_period)
df_papers <- df_papers %>% filter(year %in% desired_period)

#### Plotting ####
source(here('R', 'plotting_sci_output.R'))

#### GG Text Tables ####
source(here('R', 'sum_tb_scimago_quartile.R'))
source(here('R', 'sum_tb_capes_qualis.R'))

#### Wordclouds ####
source(here('R', 'wordcloud_authors.R'))

cloud_color <- "Blues"
source(here('R', 'wordcloud_keywords.R'))
source(here('R', 'wordcloud_indexed_keywords.R'))

#### Final Summary Table ####
total_publications <- df_all %>% filter(year %in% desired_period) %>% nrow()
total_papers <- df_all %>% filter(type != 'chapter') %>% nrow()
total_chapters <- df_all %>% filter(type == 'chapter') %>% nrow()

papers_first <- df_all %>% filter(type != 'chapter') %>% filter(first_auth == 'First') %>% nrow()
papers_shared <- df_all %>% filter(type != 'chapter') %>% filter(first_auth == 'First_shared') %>% nrow()
papers_corresponding <- df_all %>% filter(type != 'chapter') %>% filter(corresp_auth == 'Moreira, D.C.') %>% nrow()

chapters_first <- df_all %>% filter(type == 'chapter') %>% filter(first_auth == 'First') %>% nrow()
chapters_shared <- df_all %>% filter(type == 'chapter') %>% filter(first_auth == 'First_shared') %>% nrow()
chapters_corresponding <- df_all %>% filter(type == 'chapter') %>% filter(corresp_auth == 'Moreira, D.C.') %>% nrow()

df_sum <- data.frame(Role = character(4), Chapter = numeric(4), Paper = numeric(4))

df_sum$Role <-  c('First', 'Shared 1st', 'Corresponding', 'All')
df_sum$Chapter <- c(chapters_first, chapters_shared, chapters_corresponding, total_chapters)
df_sum$Paper <- c(papers_first, papers_shared, papers_corresponding, total_papers)
df_sum$Total <- rowSums(df_sum[sapply(df_sum, is.numeric)])

tb_sumtable <- ggtexttable(df_sum, 
                         rows = NULL, 
                         theme = ttheme("mBlue"))
# Saving summary table
if (!dir.exists('table')) {
  dir.create('table')
}

ggsave(file = here('table', paste0(format(Sys.Date(), "%Y.%m.%d"), '_', period_min, '-', period_max, "_sum_tb.png")), 
       tb_sumtable, bg = "white", 
       height = 5, width = 10, units = "cm", dpi = 600)
