#### Loading packages ####
library(rvest)

#### Define the function to retrieve h-index ####
get_h_index_google <- function(scholar_url) {
  # Read the HTML content from the Google Scholar profile
  page <- read_html(scholar_url)
  
  # Extract all elements with class 'gsc_rsb_std'
  h_index_elements <- page %>%
    html_nodes(".gsc_rsb_std") %>%
    html_text()
  
  # Convert elements to numeric. The h-index is the third element
  h_index <- as.numeric(h_index_elements[3])
  
  return(h_index)
}