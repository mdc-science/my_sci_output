library(wordcloud)
library(textstem)

#### Word Cloud - Keywords ####
max_count <- df_all %>%
  mutate(count = lengths(strsplit(keywords_auth_scopus, ";"))) %>%
  summarise(max_count = max(count)) %>%
  pull(max_count)

df_keywords <- df_all %>% 
  select(year, title, keywords_auth_scopus) %>% 
  separate_wider_delim(keywords_auth_scopus, delim = ";", too_few = "align_start",
                       names = c(sprintf("keyword_%d", seq(1:max_count))))

df_keywords <- df_keywords %>% 
  gather(key = "key", value = "keyword", -year, -title)

# Convert the text to lower case
df_keywords$keyword <- tolower(df_keywords$keyword)
df_keywords$keyword <- trimws(df_keywords$keyword)
df_keywords$keyword <- lemmatize_words(df_keywords$keyword)

dt_keywords <- df_keywords %>% 
  group_by(keyword) %>% 
  summarize(freq = n()) %>% 
  rename(word = keyword) %>%
  filter(!is.na(word)) %>%
  filter(word != 'article') %>%
  filter(freq > 0)

set.seed(1234)

# Open PNG graphics device
if (!dir.exists('wordcloud')) {
  dir.create('wordcloud')
}

png(here('wordcloud', paste0(format(Sys.Date(), "%Y.%m.%d"), "_keywords_cloud.png")), 
    width = 28, height = 20, units = 'cm', res = 600)

wordcloud(words = dt_keywords$word, freq = dt_keywords$freq, min.freq = 2,
          max.words = 150, random.order = FALSE, rot.per = 0, scale = c(4, 0.4),
          colors = brewer.pal(6, "Blues"))

# Close the graphics device
dev.off()