# Kør dette R Script manuelt for at konvertere alle PDF-filer til PNG-filer

# Load the 'pdftools' package
library(pdftools)

# Set working directory to this script's location
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Get all PDF files in the parent folder
pdf_files <- list.files(pattern = "\\.pdf$", full.names = TRUE)

# Convert each PDF to PNG
for (pdf in pdf_files) {
  base_name <- tools::file_path_sans_ext(basename(pdf))
  
  # Get number of pages in the PDF
  n_pages <- pdf_info(pdf)$pages
  
  # Generate filenames for each page
  out_files <- file.path(paste0(base_name, "_", seq_len(n_pages), ".png"))
  
  # Convert and save PNGs
  pdf_convert(pdf = pdf, dpi = 72, filenames = file.path(paste0(base_name, "-", seq_len(n_pages), ".png")))
}