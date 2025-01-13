# Load required libraries
library(pdftools)
library(qpdf)
library(here)

# Define the folder containing PDF files and the output file name
input_folder <- here("_all_papers_PDFs") # Replace with your folder path

# Define the year range for filtering
start_year <- 2012 # Replace with your desired start year
end_year <- 2025   # Replace with your desired end year

# Construct the output file name dynamically with the year range
output_file <- here(
  "_all_papers_PDFs", 
  "combined_first_pages", 
  paste0("combined_first_pages_", start_year, "-", end_year, ".pdf")
)

# List all PDF files in the folder
pdf_files <- list.files(input_folder, pattern = "\\.pdf$", full.names = TRUE)

# Filter files based on the year range extracted from filenames
filtered_files <- pdf_files[sapply(pdf_files, function(file) {
  file_year <- as.numeric(substr(basename(file), 1, 4)) # Extract year from filename
  file_year >= start_year & file_year <= end_year       # Check if year is within range
})]

# Create a temporary directory to store the first pages
temp_dir <- tempdir()

# Extract the first page of each filtered PDF
first_page_files <- c()
for (pdf in filtered_files) {
  temp_file <- file.path(temp_dir, paste0("first_page_", basename(pdf)))
  pdf_subset(pdf, pages = 1, output = temp_file)
  first_page_files <- c(first_page_files, temp_file)
}

# Combine all the first pages into one PDF
pdf_combine(input = first_page_files, output = output_file)

# Cleanup temporary files
unlink(first_page_files)

cat("Combined PDF saved to:", output_file, "\n")
cat("Number of files processed:", length(filtered_files), "\n")
