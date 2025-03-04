# Load necessary packages
library(pdftools)
library(qpdf)
library(here)

# Specify the folder containing PDF files
input_folder <- here("_all_papers_PDFs")  # Replace with your folder path
output_file <- "merged_file.pdf"       # Output file name

# Get a list of all PDF files in the folder
pdf_files <- list.files(input_folder, pattern = "\\.pdf$", full.names = TRUE)

# Sort files to ensure consistent order (optional)
pdf_files <- sort(pdf_files)

# Check if there are any PDF files
if (length(pdf_files) == 0) {
  stop("No PDF files found in the specified folder.")
}

# Merge the PDF files
cat("Merging the following PDF files:\n")
print(basename(pdf_files))

pdf_combine(pdf_files, output_file)

cat("PDF files successfully merged into:", output_file, "\n")
