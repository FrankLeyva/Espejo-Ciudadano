# R/extras/thumbnail_generator.R

# Required libraries
if (!require(pdftools)) install.packages("pdftools")
if (!require(magick)) install.packages("magick")

library(pdftools)
library(magick)

# Function to generate thumbnails for all PDFs
generate_all_thumbnails <- function(reports_dir = "www/reports", 
                                  thumbnails_dir = "www/thumbnails",
                                  force_regenerate = FALSE) {
  
  # Create thumbnails directory if it doesn't exist
  if (!dir.exists(thumbnails_dir)) {
    dir.create(thumbnails_dir, recursive = TRUE)
  }
  
  # Get all PDF files
  pdf_files <- list.files(reports_dir, pattern = "\\.pdf$", ignore.case = TRUE, full.names = TRUE)
  
  if (length(pdf_files) == 0) {
    warning("No PDF files found in ", reports_dir)
    return(data.frame())
  }
  
  # Generate thumbnails for each PDF
  results <- data.frame(
    pdf_file = character(),
    thumbnail_path = character(),
    success = logical(),
    error = character(),
    stringsAsFactors = FALSE
  )
  
  for (pdf_path in pdf_files) {
    filename <- basename(pdf_path)
    result <- generate_pdf_thumbnail(pdf_path, thumbnails_dir, force_regenerate)
    
    results <- rbind(results, data.frame(
      pdf_file = filename,
      thumbnail_path = result$thumbnail_path,
      success = result$success,
      error = result$error %||% "",
      stringsAsFactors = FALSE
    ))
  }
  
  # Print summary
  successful <- sum(results$success)
  total <- nrow(results)
  
  cat("Thumbnail generation completed:\n")
  cat("- Successful:", successful, "/", total, "\n")
  
  if (any(!results$success)) {
    failed_files <- results$pdf_file[!results$success]
    cat("- Failed files:", paste(failed_files, collapse = ", "), "\n")
  }
  
  return(results)
}

# Function to generate thumbnail for a single PDF
generate_pdf_thumbnail <- function(pdf_path, 
                                 thumbnails_dir = "www/thumbnails",
                                 force_regenerate = FALSE,
                                 width = 300,
                                 height = 400) {
  
  # Generate thumbnail filename
  pdf_filename <- basename(pdf_path)
  thumbnail_filename <- paste0(tools::file_path_sans_ext(pdf_filename), "_thumb.png")
  thumbnail_path <- file.path(thumbnails_dir, thumbnail_filename)
  
  # Check if thumbnail already exists
  if (file.exists(thumbnail_path) && !force_regenerate) {
    return(list(
      success = TRUE,
      thumbnail_path = thumbnail_filename,
      error = NULL
    ))
  }
  
  # Try to generate thumbnail
  tryCatch({
    # Check if PDF exists and is readable
    if (!file.exists(pdf_path)) {
      stop("PDF file not found: ", pdf_path)
    }
    
    # Get PDF info
    info <- pdf_info(pdf_path)
    if (info$pages == 0) {
      stop("PDF has no pages")
    }
    
    # Convert first page to image
    # Using density for quality (150 DPI is good balance between quality and file size)
    first_page <- pdf_render_page(pdf_path, page = 1, dpi = 150)
    
    # Convert to magick image
    img <- image_read(first_page)
    
    # Resize to thumbnail dimensions while maintaining aspect ratio
    img <- image_resize(img, paste0(width, "x", height))
    
    # Add a subtle border
    img <- image_border(img, "#e0e0e0", "2x2")
    
    # Save thumbnail
    image_write(img, thumbnail_path, format = "PNG")
    
    return(list(
      success = TRUE,
      thumbnail_path = thumbnail_filename,
      error = NULL
    ))
    
  }, error = function(e) {
    warning("Failed to generate thumbnail for ", pdf_filename, ": ", e$message)
    return(list(
      success = FALSE,
      thumbnail_path = NULL,
      error = e$message
    ))
  })
}

# Function to get thumbnail path for a report
get_thumbnail_path <- function(pdf_filename, thumbnails_dir = "www/thumbnails") {
  thumbnail_filename <- paste0(tools::file_path_sans_ext(pdf_filename), "_thumb.png")
  thumbnail_path <- file.path(thumbnails_dir, thumbnail_filename)
  
  # Check if thumbnail exists
  if (file.exists(thumbnail_path)) {
    # Return relative path for web serving
    return(file.path("thumbnails", thumbnail_filename))
  } else {
    return(NULL)
  }
}

# Function to clean up orphaned thumbnails
cleanup_thumbnails <- function(reports_dir = "www/reports", 
                              thumbnails_dir = "www/thumbnails") {
  
  if (!dir.exists(thumbnails_dir)) {
    return(0)
  }
  
  # Get list of existing PDFs
  pdf_files <- list.files(reports_dir, pattern = "\\.pdf$", ignore.case = TRUE)
  pdf_basenames <- tools::file_path_sans_ext(pdf_files)
  
  # Get list of thumbnails
  thumbnail_files <- list.files(thumbnails_dir, pattern = "_thumb\\.png$", ignore.case = TRUE)
  
  # Find orphaned thumbnails
  orphaned <- c()
  for (thumb in thumbnail_files) {
    pdf_name <- gsub("_thumb\\.png$", "", thumb, ignore.case = TRUE)
    if (!pdf_name %in% pdf_basenames) {
      orphaned <- c(orphaned, thumb)
    }
  }
  
  # Remove orphaned thumbnails
  if (length(orphaned) > 0) {
    for (thumb in orphaned) {
      file.remove(file.path(thumbnails_dir, thumb))
    }
    cat("Removed", length(orphaned), "orphaned thumbnails\n")
  }
  
  return(length(orphaned))
}

# Utility function to check if thumbnail generation is available
check_thumbnail_dependencies <- function() {
  deps <- list(
    pdftools = requireNamespace("pdftools", quietly = TRUE),
    magick = requireNamespace("magick", quietly = TRUE)
  )
  
  missing <- names(deps)[!unlist(deps)]
  
  if (length(missing) > 0) {
    cat("Missing packages for thumbnail generation:", paste(missing, collapse = ", "), "\n")
    cat("Install with: install.packages(c(", paste0("'", missing, "'", collapse = ", "), "))\n")
    return(FALSE)
  }
  
  return(TRUE)
}