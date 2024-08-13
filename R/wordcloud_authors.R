library(wordcloud)
library(magick)

#### Word Cloud - Authors ####
df_authors <- df_all %>% 
  select(year, title, authors) %>% 
  separate_wider_delim(authors, delim = ";", too_few = "align_start",
                       names = c(sprintf("author_%d", seq(1:50))))

df_authors <- df_authors %>% 
  gather(key = "name", value = "author", -year, -title)

dt_names <- df_authors %>% 
  group_by(author) %>% 
  summarize(freq = n()) %>% 
  rename(word = author) %>%
  filter(!is.na(word)) %>%
  filter(word != "Moreira, D.C.") %>%
  arrange(freq) %>%
  filter(freq > 2)

set.seed(123)

# Open PNG graphics device
if (!dir.exists('wordcloud')) {
  dir.create('wordcloud')
}

output_path <- here('wordcloud', paste0(format(Sys.Date(), "%Y.%m.%d"), "_authors_cloud.png"))

png(output_path, 
    width = 20, height = 18, units = 'cm', res = 300)

wordcloud(words = dt_names$word, freq = dt_names$freq, min.freq = 2,
          max.words = 150, random.order = FALSE, rot.per=0, scale = c(5, 0.5),
          colors = rev(brewer.pal(5, "Spectral")))

# Close the graphics device
dev.off()

# Read the image and trim the margins
image <- image_read(output_path)
image_trimmed <- image_trim(image)

# Save the trimmed image
image_write(image_trimmed, output_path)
